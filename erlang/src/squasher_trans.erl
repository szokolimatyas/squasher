-module(squasher_trans).

-export([parse_transform/2]).

-define(SYN, erl_syntax).

%%% use this as example: https://github.com/saleyn/etran/blob/master/src/str.erl
%%% and this: https://github.com/erlang/otp/blob/master/lib/syntax_tools/src/erl_syntax_lib.erl
-spec parse_transform(Forms, Options) -> Forms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
    Options :: [compile:option()].
parse_transform(Forms, _Options) -> 
    Tree = erl_syntax:form_list(Forms),
    ModifiedTree = replace(?SYN:form_list_elements(Tree)),
    erl_syntax:revert_forms(ModifiedTree).

-spec replace(Forms) -> Forms when
    Forms :: [erl_parse:abstract_form() | erl_parse:form_info()].
replace([H|T]) ->
    case erl_syntax:type(H) of
        function ->
            replace_function(H) ++ replace(T);
        _ ->
            [H | replace(T)]
    end;
replace(L) -> L.


% foo(Args...) ->
%     ...
%     foo(Args1...)
%     ...
%     foo(Args2...).

% to:

% foo(Args...) ->
%     track(foo'(track(Args)...))

% foo'(Args...) ->
%     ...
%     foo(Args1...)
%     ...
%     foo(Args2...).
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
do_opt_tail_call(_OrigName, _Arity, _Type, Expr) ->
    Expr.

optimize_call(OrigName, Arity, FormalParams, Application) ->
    %%% warning! shadowing in funs :(, might not handle it just yet
    Params = ?SYN:application_arguments(Application),
    F = 
        fun(P, FP) -> 
            %%% P is part of the formal parameters of the original function
            case is_in_expr(P, FP) of 
                true -> {P, '$no_track'}; 
                false -> {P, '$track'} 
            end 
        end,
    TaggedParams = lists:zipwith(F, Params, FormalParams),
    case lists:any(fun({_, A}) -> A == '$no_track' end, TaggedParams) of
        true -> 
            mocker_fun_call(OrigName, Arity, TaggedParams);
        _ -> 
            Application
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
    list_to_atom("squasher_mocked_fun_" ++ atom_to_list(Name)).

dom(Index, Name, Arity) ->
    ?SYN:abstract([{dom, Index, Arity}, {name, atom_to_list(Name), Arity}]).

%%% optimization:
%%% for each clause of the intercepted function:
%%% recursive calls are changed so that
%%% if a parameter is shrinking or equal in all recursive calls in that clause
%%% then call the intercepted (not original function) with that parameter untracked

%%% for shrinking parameters, it is ok :: we only track the first occurrence of it
%%% for growing parameters, it is not! :: we have to track only the last occurrence of it, how?

%%% for growing params: if (in all clauses!!!) a parameter is equals or growing,
%%% find all the clauses where it is not used again, track there::
%%% rename the parameter to something else, and do :: OrigName = track(SynthesizedName, ...)
%%% (further opt: if this parameter is subterm in the returned term, this is unnecessary)

%%% reverse([], Acc) -> Acc   reverse([H|T], Acc) -> reverse(T, [H| Acc]) becomes
%%% reverse(P1, P2) -> track(reverse_(track(P1), track(P2)))  reverse_([], Acc_) -> Acc=track(Acc_), Acc  reverse_([H|T], Acc) -> track(reverse_(T, [H|T]))
%%%
%%% this does not make tail calls ok yet! that is a separate analysis step, this removes unnecessary tracking of variables!
%%% tail call elim: if a recursive call is the last expr in a block, use the intercepted function name to call, without the track()
%%% reverse(P1, P2) -> track(reverse_(track(P1), track(P2)))  reverse_([], Acc_) -> Acc=track(Acc_), Acc  reverse_([H|T], Acc) -> reverse_(T, [H|T])

is_legal_expr(Expr) ->
    erl_syntax_lib:fold(fun do_is_legal_expr/2, true, Expr).

do_is_legal_expr(_, false) -> false;
do_is_legal_expr({op, _, Op, _, _}, true) ->
    ValidOps = sets:from_list(['++', '+', '-', '*', '/', '=', 'div', 'rem', 
                               'band', 'and', 'bor', 'bxor', 'bsl', 'bsr', 'or', 'xor'], [{version, 2}]),
    sets:is_element(Op, ValidOps);
do_is_legal_expr(Expr, true) ->
    T = erl_syntax:type(Expr),
    Bad = sets:from_list([application, if_expr, case_expr, catch_expr, else_expr, block_expr, conjunction, disjunction,
                          maybe_expr, maybe_match_expr, receive_expr, try_expr, generator, list_comp, named_fun_expr, 
                          prefix_expr, clause, function, fun_expr, infix_expr], [{version, 2}]),
    not sets:is_element(T, Bad).

is_expr_subterm(Expr, InExpr) ->
    is_legal_expr(InExpr) andalso
    is_in_expr(Expr, InExpr).

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
