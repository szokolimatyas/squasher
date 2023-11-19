-module(squasher_trans).

-export([parse_transform/2]).

-define(SYN, erl_syntax).

-define(BAD_COMPOUND, sets:from_list([application, if_expr, case_expr, catch_expr, else_expr, block_expr, conjunction, disjunction,
                                      maybe_expr, maybe_match_expr, receive_expr, try_expr, generator, list_comp, named_fun_expr, 
                                      prefix_expr, clause, function, fun_expr, infix_expr], [{version, 2}])).

-define(VALID_OPS, sets:from_list(['++', '+', '-', '*', '/', '=', 'div', 'rem', 
                                   'band', 'and', 'bor', 'bxor', 'bsl', 'bsr', 'or', 'xor'], [{version, 2}])).

-spec parse_transform(Forms, Options) -> Forms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Options :: [compile:option()].
parse_transform(Forms, _Options) -> 
    Tree = erl_syntax:form_list(Forms),
    TreeList = ?SYN:form_list_elements(Tree),
    save_formal_parameters(TreeList), 
    ModifiedTree = replace(TreeList),
    erl_syntax:revert_forms(ModifiedTree).

save_formal_parameters(TreeList) ->
    case lists:search(fun is_module_attribute/1, TreeList) of
        {value, Tree} ->
            case erl_syntax_lib:analyze_attribute(Tree) of
                {module, ModName} when is_atom(ModName) ->
                    Ps = get_formal_parameters(TreeList),
                    file:write_file(atom_to_list(ModName) ++ "_args.bin", term_to_binary(Ps));
                {module, {ModName, _}} when is_atom(ModName) ->
                    Ps = get_formal_parameters(TreeList),
                    file:write_file(atom_to_list(ModName) ++ "_args.bin", term_to_binary(Ps));
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

is_module_attribute(Tree) ->
    case ?SYN:type(Tree) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(Tree) of
                {module, _} -> true;
                _ -> false
            end;
        _ ->
            false
    end.

get_formal_parameters([]) -> [];
get_formal_parameters([Tree|TreeList]) ->
    case ?SYN:type(Tree) of
        function ->
            Name = erl_syntax_lib:analyze_function(Tree),
            Pack = {Name, formal_parameters_of_clauses(?SYN:function_clauses(Tree))},
            [ Pack | get_formal_parameters(TreeList)];
        _ ->
            get_formal_parameters(TreeList) 
    end.

formal_parameters_of_clauses([]) -> [];
formal_parameters_of_clauses([Clause|Clauses]) ->
    Pats = ?SYN:clause_patterns(Clause),
    Names = [ name_of_parameter(P) || P <- Pats],
    [Names | formal_parameters_of_clauses(Clauses)].

name_of_parameter(P) ->
    P1 = case ?SYN:type(P) of
        list ->
            Prefix = ?SYN:list_prefix(P),
            case Prefix of
                [Single] ->
                    Single;
                _ ->
                    P
            end;
        _ ->
            P
    end,            
    case ?SYN:type(P1) of
        variable -> 
            Str = atom_to_list(?SYN:variable_name(P1)),
            %% what if it is all underscores? empty name?
            {name, string:trim(Str, leading, "_")};  
        _ -> no_name
    end.

-spec replace(Forms) -> Forms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()].
replace([H|T]) ->
    case erl_syntax:type(H) of
        function ->
            replace_function(H) ++ replace(T);
        Type ->
            io:format("~p~n", [Type]),
            [H | replace(T)]
    end;
replace(L) -> L.

-spec replace_function(Function) -> [Function] when
    Function :: erl_syntax:syntaxTree().
replace_function(F) ->
    {Name, Arity} = erl_syntax_lib:analyze_function(F),
    Clauses = ?SYN:function_clauses(F),
    Original = ?SYN:function(?SYN:function_name(F), [proxy_fun_clause(Name, Arity)]),
    %% not sure about the name
    New = ?SYN:function(new_function_name(Name), optimize_clauses(Name, Arity, Clauses)),
    [Original, New].

optimize_clauses(_OrigName, _Arity, []) -> [];
optimize_clauses(OrigName, Arity, [Clause | Clauses]) ->
    Vars = ?SYN:clause_patterns(Clause),
    Guards = ?SYN:clause_guard(Clause),
    Exprs = ?SYN:clause_body(Clause),
    Exprs1 = 
        [map_calls_in_expr({OrigName, Arity}, 
                           fun(E) -> optimize_call(OrigName, Arity, Vars, E) end, 
                           Expr) || Expr <- Exprs],
    NewClause = ?SYN:clause(Vars, Guards, opt_tail_calls_in_exprs(OrigName, Arity, Exprs1)),
    [NewClause | optimize_clauses(OrigName, Arity, Clauses)].

opt_tail_calls_in_exprs(_OrigName, _Arity, []) -> [];
opt_tail_calls_in_exprs(OrigName, Arity, Exprs) ->
    EndExpr = lists:last(Exprs),
    Type = ?SYN:type(EndExpr),
    lists:droplast(Exprs) ++ [do_opt_tail_call(OrigName, Arity, Type, EndExpr)].

opt_tail_calls_in_clauses(_OrigName, _Arity, []) -> [];
opt_tail_calls_in_clauses(OrigName, Arity, [Clause|Clauses]) ->
    Vars = ?SYN:clause_patterns(Clause),
    Guards = ?SYN:clause_guard(Clause),
    Exprs = ?SYN:clause_body(Clause),
    NewExprs = opt_tail_calls_in_exprs(OrigName, Arity, Exprs),
    NewClause = ?SYN:clause(Vars, Guards, NewExprs),
    [NewClause | opt_tail_calls_in_clauses(OrigName, Arity, Clauses)].    

%%% todo: make it accept term contruction that only contains recursive calls
do_opt_tail_call(OrigName, Arity, application, Expr) ->
    %%% change the original function call to bring in the collect:track call
    Expr1 =
        case erl_syntax_lib:analyze_application(Expr) of
            {OrigName, Arity} ->
                Args = ?SYN:application_arguments(Expr),
                TaggedArgs = [ {A, '$track'} || A <- Args ],
                mocker_fun_call(OrigName, Arity, TaggedArgs);
            _ ->
                Expr
        end,
    %%% remove unnecessary collect:track call
    case erl_syntax_lib:analyze_application(Expr1) of
        {collect, {track, 2}} ->
            %% _Arg2 is the path
            [Arg1, _Arg2] = ?SYN:application_arguments(Expr1),
            NewName = new_function_name_atom(OrigName),
            case erl_syntax:type(Arg1) of
                application ->        
                    case erl_syntax_lib:analyze_application(Arg1) of
                        {NewName, Arity} ->
                            Arg1;
                        _ ->
                            Expr
                    end;
                _ ->
                    Expr
            end;
        _ ->
            Expr
    end;
do_opt_tail_call(OrigName, Arity, block_expr, Expr) ->
    Exprs = ?SYN:block_expr_body(Expr),
    NewExprs = opt_tail_calls_in_exprs(OrigName, Arity, Exprs),
    ?SYN:block_expr(NewExprs);
do_opt_tail_call(OrigName, Arity, case_expr, Expr) ->
    Arg = ?SYN:case_expr_argument(Expr),
    Clauses = ?SYN:case_expr_clauses(Expr),
    NewClauses = opt_tail_calls_in_clauses(OrigName, Arity, Clauses),
    ?SYN:case_expr(Arg, NewClauses);
do_opt_tail_call(OrigName, Arity, if_expr, Expr) ->
    Clauses = ?SYN:if_expr_clauses(Expr),
    NewClauses = opt_tail_calls_in_clauses(OrigName, Arity, Clauses),
    ?SYN:if_expr(NewClauses);
do_opt_tail_call(OrigName, Arity, receive_expr, Expr) ->
    Clauses = ?SYN:receive_expr_clauses(Expr),
    Timeout = ?SYN:receive_expr_timeout(Expr),
    Action = ?SYN:receive_expr_action(Expr),
    NewClauses = opt_tail_calls_in_clauses(OrigName, Arity, Clauses),
    ?SYN:receive_expr(NewClauses, Timeout, Action);
do_opt_tail_call(OrigName, Arity, infix_expr, Tree) ->
    Left = ?SYN:infix_expr_left(Tree),
    Op = ?SYN:infix_expr_operator(Tree),
    Right = ?SYN:infix_expr_right(Tree),
    case sets:is_element(?SYN:operator_name(Op), ?VALID_OPS) of
        true ->
            NewLeft = do_opt_tail_call(OrigName, Arity, ?SYN:type(Left), Left), 
            NewRight = do_opt_tail_call(OrigName, Arity, ?SYN:type(Right), Right), 
            ?SYN:infix_expr(NewLeft, Op, NewRight);
        false -> Tree
    end;
do_opt_tail_call(OrigName, Arity, Type, Tree) ->
    case sets:is_element(Type, ?BAD_COMPOUND) of
        true -> Tree;
        false ->
            case ?SYN:subtrees(Tree) of
                [] ->
                    Tree;
                Gs ->
                    Tree1 = ?SYN:make_tree(?SYN:type(Tree),
                                                [[do_opt_tail_call(OrigName, Arity, ?SYN:type(T), T) || T <- G]
                                                    || G <- Gs]),
                    ?SYN:copy_attrs(Tree, Tree1)
            end
    end. 

optimize_call(OrigName, Arity, FormalParams, Application) ->
    %%% warning! shadowing in funs :(, might not handle it just yet
    Params = ?SYN:application_arguments(Application),
    F = 
        fun(Index, P, FP) -> 
            %%% P is part of the formal parameters of the original function
            {optimize_growing_param(OrigName, Arity, Index, P, FP), '$no_track'}
        end,
    TaggedParams = indexedZipWith(F, Params, FormalParams),
    mocker_fun_call(OrigName, Arity, TaggedParams). 

indexedZipWith(F, L1, L2) -> indexedZipWith(F, L1, L2, 1).

indexedZipWith(F, [H1|T1], [H2|T2], I) -> [F(I, H1, H2) | indexedZipWith(F, T1, T2, I + 1)];
indexedZipWith(_, _, _, _) -> [].

%%% growing parameters special case: list building
%%% e.g. [E1, E2, E3 | L1] or L1 ++ (L2 ++ L3)
optimize_growing_param(Name, Arity, Index, Param, FormalParam) ->
    Path = dom(Index, Name, Arity),
    Wrapper = fun(What) ->
        case is_in_expr(What, FormalParam) of 
            true -> What;
            false -> 
                %% optimization
                ?SYN:application(?SYN:atom(collect), 
                                 ?SYN:atom(track), 
                                 [What, Path, ?SYN:integer(10), ?SYN:integer(10)])
        end
    end,
    case ?SYN:type(Param) of
        list ->
            Prefixes = ?SYN:list_prefix(Param),
            NewPrefixes = [ optimize_growing_param(Name, Arity, Index, P, FormalParam) || P  <- Prefixes],
            Suffix = ?SYN:list_suffix(Param),
            NewSuffix = case Suffix of
                none -> none;
                _ -> optimize_growing_param(Name, Arity, Index, Suffix, FormalParam)
            end,
            ?SYN:list(NewPrefixes, NewSuffix);
        infix_expr ->
            Path = dom(Index, Name, Arity),
            Op = ?SYN:infix_expr_operator(Param),
            case ?SYN:operator_name(Op) of
                '++' ->
                    Left = ?SYN:infix_expr_left(Param),
                    Right = ?SYN:infix_expr_right(Param),
                    NewLeft = optimize_growing_param(Name, Arity, Index, Left, FormalParam),
                    NewRight = optimize_growing_param(Name, Arity, Index, Right, FormalParam),
                    ?SYN:infix_expr(NewLeft, Op, NewRight);
                _ ->
                    Wrapper(Param)
            end;
        _ ->
            Wrapper(Param)
    end.

-spec proxy_fun_clause(Name, Arity) -> Clause when
    Name :: atom(),
    Arity :: integer(),
    Clause :: erl_syntax:syntaxTree().
proxy_fun_clause(Name, Arity) ->
    Vars = [?SYN:variable("V" ++ integer_to_list(I)) || 
            I <- lists:seq(1, Arity)],
    TaggedParams = [ {V, '$track'} || V <- Vars ],
    Call = mocker_fun_call(Name, Arity, TaggedParams),
    ?SYN:clause(Vars, none, [Call]).

mocker_fun_call(Name, Arity, TaggedParams) ->
    %% collect:track(V_i, P)
    Args = [ tracked_var(Index, TV, Name, Arity) || {Index, TV} <- lists:enumerate(TaggedParams)],
    %% foo'(collect:track(V_i, P)...)
    Call = ?SYN:application(none, new_function_name(Name), Args),
    %% Path = [{rng, Arity}, {name, atom_to_list(Name), Arity}],
    %% collect:track(foo'(collect:track(V_i, P)...), P')
    Path = range(Name, Arity),
    ?SYN:application(?SYN:atom(collect),?SYN:atom(track), [Call, Path]).

%%% collect:track(V, Path) if track, V if no_track
tracked_var(_Index, {V, '$no_track'}, _Name, _Arity) -> V;
tracked_var(Index, {V, '$track'}, Name, Arity) ->
    %Path [{dom, Index, Arity}, {name, atom_to_list(Name), Arity}],
    Path = dom(Index, Name, Arity),
    ?SYN:application(?SYN:atom(collect), ?SYN:atom(track), [V, Path]).

range(Name, Arity) ->
    ?SYN:abstract([{rng, Arity}, {name, atom_to_list(Name), Arity}]).

-spec new_function_name(atom()) -> erl_syntax:syntaxTree().
new_function_name(Name) ->
    ?SYN:atom(new_function_name_atom(Name)).

new_function_name_atom(Name) ->
    list_to_atom(atom_to_list(Name) ++ "__").

dom(Index, Name, Arity) ->
    ?SYN:abstract([{dom, Index, Arity}, {name, atom_to_list(Name), Arity}]).

is_in_expr(Elem, InExpr) ->
    erl_syntax_lib:fold(fun(Elem2, Acc) -> do_is_in_expr(Elem, Elem2, Acc) end, false, InExpr).

do_is_in_expr(_, _, true) -> true;
do_is_in_expr(Elem1, Elem2, false) ->
    (erl_syntax:type(Elem1) == erl_syntax:type(Elem2)) andalso (delete_posinfo(Elem1) == delete_posinfo(Elem2)).

delete_posinfo(Expr) ->
    erl_syntax_lib:map(fun(T) -> case element(2, T) of {_, _} -> setelement(2, T, {0, 0}); _ -> T end end, Expr).

map_calls_in_expr(FunName, F, Expr) ->
    Tree = erl_syntax_lib:map(fun(Elem) -> do_map_call(FunName, F, Elem) end, Expr),
    erl_syntax:revert(Tree).

%%% do erl_syntax:application_arguments on the results
%%% TODO: qualified calls!! M:F:A (in the same module)
-spec do_map_call(FunName, F, Expr) -> Expr when
    FunName :: {atom(), arity()} | {atom(), {atom(), integer()}},
    F :: fun((Expr) -> Expr),
    Expr :: erl_syntax:syntaxTree().
do_map_call(FunName, F, Expr) ->
    case erl_syntax:type(Expr) of
        application -> 
            case erl_syntax_lib:analyze_application(Expr) of
                FunName ->
                    F(Expr);
                _ ->
                    Expr
            end;
        _ -> Expr
    end.
