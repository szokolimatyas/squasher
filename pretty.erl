-type tuple_list() :: {atom(), integer()} | list().

-type atom_outdir() :: atom() | {outdir, [integer()]}.

-record(lint,{field1 :: start | attribute | function,
              field2 :: '' | bead,
              field3 :: list(),
              field4 :: gb_trees:tree(_, _),
              field5 :: list(),
              field6 :: list(),
              field7 :: map(),
              field8 :: gb_sets:set(_),
              field9 :: gb_sets:set(any()) | gb_trees:tree(any(), any()),
              field10 :: gb_sets:set(_),
              field11 :: list(),
              field12 :: integer(),
              field13 :: list(),
              field14 :: list(),
              field15 ::
                  gb_sets:set(any()) | gb_trees:tree(any(), any()),
              field16 :: any(),
              field17 :: list(),
              field18 :: integer(),
              field19 :: [atom()],
              field20 :: list(),
              field21 :: list(),
              field22 ::
                  [{[integer()],
                    {{integer(), integer()}, erl_lint, export_all}}],
              field23 :: [integer()],
              field24 :: false,
              field25 :: false,
              field26 :: [{any(), any()}],
              field27 :: undefined,
              field28 :: any(),
              field29 :: map(),
              field30 :: map(),
              field31 :: map(),
              field32 :: map(),
              field33 ::
                  gb_sets:set(any()) | gb_trees:tree(any(), any()),
              field34 :: map(),
              field35 :: none,
              field36 :: guard,
              field37 :: false}).

-type location_file() ::
          {location, {integer(), integer()}} | {file, [integer()]}.

-type union() ::
          {'receive', {integer(), integer()}, [#clause{}]} |
          {'if', {integer(), integer()}, [#clause{}]} |
          {match, {integer(), integer()}, union(), union()} |
          {string,
           {integer(), integer()} | [location_file()],
           [integer()]} |
          {tuple, {integer(), integer()}, [union()]} |
          {call,
           {integer(), integer()} | [location_file()],
           union(),
           [union()]} |
          {var, {integer(), integer()}, atom()} |
          {op,
           {integer(), integer()},
           '>=' | '!' | '+',
           union(),
           union()} |
          {lc, {integer(), integer()}, union(), [#generate{}]} |
          {atom, {integer(), integer()} | [location_file()], atom()} |
          {'fun', {integer(), integer()}, {clauses, [#clause{}]}} |
          {integer, {integer(), integer()}, integer()} |
          {'case', {integer(), integer()}, union(), [#clause{}]} |
          {remote, {integer(), integer()}, union(), union()}.

-record(clause,{field1 :: {integer(), integer()},
                field2 :: [union()],
                field3 :: [[union()]],
                field4 :: list()}).

-type function_eof_attribute() ::
          {function,
           {integer(), integer()} | [location_file()],
           atom(),
           integer(),
           [#clause{}]} |
          {eof, {integer(), integer()}} |
          {attribute,
           {integer(), integer()} | [location_file()],
           module | file | compile,
           {[integer()], integer()} | export_all | bead}.

-record(generate,{field1 :: {integer(), integer()},
                  field2 :: union(),
                  field3 :: union()}).

-type tuple922() ::
          {atom(),
           {unsafe() | bound, used | unused, [{integer(), integer()}]}}.

-type if_receive_case_tuple() ::
          {'if' | 'receive' | 'case', {integer(), integer()}}.

-type unsafe() :: {unsafe, if_receive_case_tuple()}.

-type ok() :: {ok, list()}.

-spec add_lint_warning({{integer(), integer()}, erl_lint, export_all},
                       [integer()],
                       #lint{}) ->
                          #lint{}.

-spec add_warning([location_file()], export_all, #lint{}) -> #lint{}.

-spec all_behaviour_callbacks(list(), list(), #lint{}) ->
                                 {list(), #lint{}}.

-spec anno_set_file(union(), [integer()]) -> union().

-spec any_control_characters([integer()]) -> false.

-spec attribute_state(function_eof_attribute(), #lint{}) -> #lint{}.

-spec auto_import_suppressed([atom_outdir()]) ->
                                gb_sets:set(any()) |
                                gb_trees:tree(any(), any()).

-spec behaviour_add_conflicts(list(), #lint{}) -> #lint{}.

-spec behaviour_check(list(), #lint{}) -> #lint{}.

-spec behaviour_conflicting(list(), #lint{}) -> #lint{}.

-spec behaviour_missing_callbacks(list(), #lint{}) -> #lint{}.

-spec bif_clash_specifically_disabled(#lint{}, tuple_list()) -> false.

-spec bif_clashes([function_eof_attribute()], #lint{}) -> #lint{}.

-spec bool_option(atom(), atom(), boolean(), [atom_outdir()]) ->
                     boolean().

-spec call_function({integer(), integer()} | [location_file()],
                    atom(),
                    integer(),
                    #lint{}) ->
                       #lint{}.

-spec check_behaviour(#lint{}) -> #lint{}.

-spec check_deprecated([function_eof_attribute()], #lint{}) -> #lint{}.

-spec check_imports([function_eof_attribute()], #lint{}) -> #lint{}.

-spec check_inlines([function_eof_attribute()], #lint{}) -> #lint{}.

-spec check_load_nif({integer(), integer()},
                     lists,
                     flatlength | sort,
                     [union()],
                     #lint{}) ->
                        #lint{}.

-spec check_module_name(lists | bead,
                        {integer(), integer()} | [location_file()],
                        #lint{}) ->
                           #lint{}.

-spec check_old_unused_vars([tuple922()], list(), #lint{}) ->
                               {[tuple922()], #lint{}}.

-spec check_option_functions([function_eof_attribute()],
                             inline, bad_inline,
                             #lint{}) ->
                                #lint{}.

-spec check_qlc_hrl({integer(), integer()},
                    lists,
                    flatlength | sort,
                    [union()],
                    #lint{}) ->
                       #lint{}.

-spec check_remote_function({integer(), integer()},
                            lists,
                            flatlength | sort,
                            [union()],
                            #lint{}) ->
                               #lint{}.

-spec check_undefined_functions(#lint{}) -> #lint{}.

-spec check_unused_functions([function_eof_attribute()], _) -> _.

-spec check_unused_vars([tuple922()], [tuple922()], #lint{}) ->
                           {[tuple922()], #lint{}}.

-spec clause(#clause{}, #lint{}) -> {[tuple922()], #lint{}}.

-spec clauses([#clause{}], #lint{}) -> #lint{}.

-spec compiler_options([function_eof_attribute()]) -> [export_all].

-spec define_function([location_file()], atom(), integer(), #lint{}) ->
                         #lint{}.

-spec deprecated_function({integer(), integer()},
                          lists | erlang,
                          atom(),
                          [union()],
                          #lint{}) ->
                             #lint{}.

-spec disallowed_compile_flags([function_eof_attribute()], #lint{}) ->
                                  #lint{}.

-spec do_expr_var(atom(), {integer(), integer()}, [tuple922()], #lint{}) ->
                     {[tuple922()], #lint{}}.

-spec eof({integer(), integer()}, #lint{}) -> #lint{}.

-spec eval_file_attr([function_eof_attribute()], [integer()]) ->
                        [function_eof_attribute()].

-spec eval_file_attribute([function_eof_attribute()], #lint{}) ->
                             [function_eof_attribute()].

-spec exports(#lint{}) -> gb_sets:set(_).

-spec expr(union(), [tuple922()], #lint{}) -> {[tuple922()], #lint{}}.

-spec expr_list([union()], [tuple922()], #lint{}) ->
                   {[tuple922()], #lint{}}.

-spec expr_var(atom(), {integer(), integer()}, [tuple922()], #lint{}) ->
                  {[tuple922()], #lint{}}.

-spec exprs([union()], [tuple922()], #lint{}) -> {[tuple922()], #lint{}}.

-spec exprs_opt([union()],
                [{'Pid', pid()}],
                [bitlevel_binaries | binary_comprehension]) ->
                   ok().

-spec feature_keywords() -> map().

-spec form(function_eof_attribute(), #lint{}) -> #lint{}.

-spec format_function({integer(), integer()},
                      lists,
                      flatlength | sort,
                      [union()],
                      #lint{}) ->
                         #lint{}.

-spec forms([function_eof_attribute()], #lint{}) -> _.

-spec fun_clause(#clause{}, [tuple922()], #lint{}) ->
                    {[tuple922()], #lint{}}.

-spec fun_clauses([#clause{}], [tuple922()], #lint{}) ->
                     {[tuple922()], #lint{}}.

-spec fun_clauses1([#clause{}], [tuple922()], #lint{}) ->
                      {[tuple922()], #lint{}}.

-spec func_location_error(bad_inline, list(), #lint{}) -> #lint{}.

-spec function([location_file()],
               atom(),
               integer(),
               [#clause{}],
               #lint{}) ->
                  #lint{}.

-spec function_check_max_args([location_file()], integer(), #lint{}) ->
                                 #lint{}.

-spec function_state(function_eof_attribute(), #lint{}) -> #lint{}.

-spec gexpr(union(), [tuple922()], #lint{}) -> {[tuple922()], #lint{}}.

-spec gexpr_list([union()], [tuple922()], #lint{}) ->
                    {[tuple922()], #lint{}}.

-spec guard([[union()]], [tuple922()], #lint{}) ->
               {[tuple922()], #lint{}}.

-spec guard_test(union(), [tuple922()], #lint{}) ->
                    {[tuple922()], #lint{}}.

-spec guard_test2(union(), [tuple922()], #lint{}) ->
                     {[tuple922()], #lint{}}.

-spec guard_tests([union()], [tuple922()], #lint{}) ->
                     {[tuple922()], #lint{}}.

-spec handle_comprehension(union(),
                           [#generate{}],
                           [tuple922()],
                           #lint{}) ->
                              {[tuple922()], #lint{}}.

-spec handle_generator(union(), union(), [tuple922()], list(), #lint{}) ->
                          {[tuple922()], list(), #lint{}}.

-spec head([union()], [tuple922()], #lint{}) ->
              {list(), [tuple922()], #lint{}}.

-spec head([union()], [tuple922()], [tuple922()], #lint{}) ->
              {list(), [tuple922()], #lint{}}.

-spec icrt_clause(#clause{}, [tuple922()], #lint{}) ->
                     {[tuple922()], #lint{}}.

-spec icrt_clauses([#clause{}], [tuple922()], #lint{}) ->
                      {[[tuple922()]], #lint{}}.

-spec icrt_clauses([#clause{}],
                   if_receive_case_tuple(),
                   [tuple922()],
                   #lint{}) ->
                      {[tuple922()], #lint{}}.

-spec icrt_export([[tuple922()]],
                  [tuple922()],
                  if_receive_case_tuple(),
                  #lint{}) ->
                     [tuple922()].

-spec icrt_export([tuple922()],
                  [tuple922()],
                  if_receive_case_tuple(),
                  integer(),
                  [tuple922()]) ->
                     [tuple922()].

-spec ignore_predefined_funcs([tuple_list()]) -> [tuple_list()].

-spec imported(atom(), integer(), #lint{}) -> no.

-spec includes_qlc_hrl([function_eof_attribute()], #lint{}) -> #lint{}.

-spec is_autoimport_suppressed(gb_sets:set(any()) |
                               gb_trees:tree(any(), any()),
                               tuple_list()) ->
                                  false.

-spec is_format_function(lists, flatlength | sort) -> false.

-spec is_gexpr_op('>=', integer()) -> true.

-spec is_local_function(gb_sets:set(_), tuple_list()) -> boolean().

-spec is_valid_call(union()) -> true.

-spec is_warn_enabled(atom(), #lint{}) -> boolean().

-spec keyword_warning({integer(), integer()} | [location_file()],
                      atom(),
                      #lint{}) ->
                         #lint{}.

-spec lc_quals([#generate{}], [tuple922()], #lint{}) ->
                  {[tuple922()], list(), #lint{}}.

-spec lc_quals([#generate{}], [tuple922()], list(), #lint{}) ->
                  {[tuple922()], list(), #lint{}}.

-spec loc({integer(), integer()} | [location_file()], #lint{}) ->
             {[integer()], {integer(), integer()}}.

-spec local_functions([function_eof_attribute()]) -> gb_sets:set(_).

-spec maps_prepend(tuple_list(), tuple_list(), map()) -> map().

-spec merge_annos([{integer(), integer()}], [{integer(), integer()}]) ->
                     [{integer(), integer()}].

-spec merge_state(bound, bound) -> bound.

-spec merge_used(used | unused, used | unused) -> used.

-spec module([function_eof_attribute()], [integer()], [atom_outdir()]) ->
                _.

-spec not_deprecated([function_eof_attribute()], #lint{}) -> #lint{}.

-spec not_removed([function_eof_attribute()], #lint{}) -> #lint{}.

-spec nowarn_function(nowarn_bif_clash, [atom_outdir()]) -> list().

-spec obsolete_guard(union(), #lint{}) -> #lint{}.

-spec pack_errors(list()) -> list().

-spec pack_warnings(list()) -> list().

-spec pat_var(atom(),
              {integer(), integer()},
              [tuple922()],
              list(),
              #lint{}) ->
                 {list(), [tuple922()], #lint{}}.

-spec pattern(union(), [tuple922()], #lint{}) ->
                 {list(), [tuple922()], #lint{}}.

-spec pattern(union(), [tuple922()], [tuple922()], #lint{}) ->
                 {list(), [tuple922()], #lint{}}.

-spec pattern_list([union()], [tuple922()], [tuple922()], #lint{}) ->
                      {list(), [tuple922()], #lint{}}.

-spec post_traversal_check([function_eof_attribute()], #lint{}) -> _.

-spec pre_scan([function_eof_attribute()], #lint{}) -> #lint{}.

-spec pseudolocals() -> [tuple_list()].

-spec reject_invalid_alias_expr(union(), union(), [tuple922()], #lint{}) ->
                                   #lint{}.

-spec remove_non_visible([integer()]) -> [integer()].

-spec return_status(#lint{}) -> ok().

-spec set_file([union()], [integer()]) -> [union()].

-spec set_form_file(function_eof_attribute(), [integer()]) ->
                       function_eof_attribute().

-spec shadow_vars([tuple922()], [tuple922()], generate | 'fun', #lint{}) ->
                     #lint{}.

-spec start([integer()], [atom_outdir()]) -> #lint{}.

-spec start_state(function_eof_attribute(), #lint{}) -> #lint{}.

-spec unused_vars([tuple922()], [tuple922()], #lint{}) -> list().

-spec value_option(warn_format,
                   integer(),
                   warn_format,
                   integer(),
                   nowarn_format,
                   integer(),
                   [atom_outdir()]) ->
                      integer().

-spec vt_no_unsafe([tuple922()]) -> [tuple922()].

-spec vt_no_unused([tuple922()]) -> [tuple922()].

-spec vtmerge([tuple922()], [tuple922()]) -> [tuple922()].

-spec vtmerge_pat([tuple922()], [tuple922()], #lint{}) ->
                     {[tuple922()], #lint{}}.

-spec vtnew([tuple922()], [tuple922()]) -> [tuple922()].

-spec vtold([tuple922()], [tuple922()]) -> [tuple922()].

-spec vtsubtract([tuple922()], [tuple922()]) -> [tuple922()].

-spec vtupdate([tuple922()], [tuple922()]) -> [tuple922()].

-spec warn_invalid_call({integer(), integer()}, union(), #lint{}) ->
                           #lint{}.

-spec warn_unused_vars(list(), [tuple922()], #lint{}) ->
                          {[tuple922()], #lint{}}.

