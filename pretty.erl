-type rec_mod_tuple() :: {record_info | module_info, integer()}.

-type list_tuple() :: list() | {worker | collector, integer()}.

-type pany() :: {pany, integer()}.

-type test() :: {test, integer()}.

-type atom_outdir() :: atom() | {outdir, [integer()]}.

-record(lint,{field1 :: start | attribute | function,
              field2 :: '' | bead,
              field3 :: list(),
              field4 ::
                  {integer(),
                   {rec_mod_tuple(), {rec_mod_tuple(), nil, nil}, nil}},
              field5 :: list(),
              field6 :: [atom_outdir()],
              field7 :: map(),
              field8 ::
                  {integer(),
                   {test(),
                    {pany(), {list_tuple(), nil, nil}, nil},
                    {list_tuple(), nil, nil}}} |
                  {integer(), nil},
              field9 :: {integer(), nil},
              field10 ::
                  {integer(),
                   {rec_mod_tuple(),
                    {rec_mod_tuple(), {list_tuple(), nil, nil}, nil},
                    {rec_mod_tuple(), nil, {list_tuple(), nil, nil}}}} |
                  {integer(),
                   {rec_mod_tuple(),
                    {rec_mod_tuple(), nil, nil},
                    {rec_mod_tuple(), nil, {list_tuple(), nil, nil}}}} |
                  {integer(),
                   {rec_mod_tuple(),
                    {rec_mod_tuple(), nil, nil},
                    {rec_mod_tuple(), nil, nil}}},
              field11 :: list(),
              field12 :: integer(),
              field13 :: list(),
              field14 :: list(),
              field15 :: {integer(), nil},
              field16 :: list_tuple(),
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
              field28 :: {usage, map(), list(), {integer(), nil}, map()},
              field29 :: map(),
              field30 :: map(),
              field31 :: map(),
              field32 :: map(),
              field33 :: {integer(), nil},
              field34 :: map(),
              field35 :: none,
              field36 :: guard,
              field37 :: false}).

-type location_file() ::
          {location, {integer(), integer()}} | {file, [integer()]}.

-type union() ::
          {match, {integer(), integer()}, union(), union()} |
          {lc,
           {integer(), integer()},
           union(),
           [{generate, {integer(), integer()}, union(), union()}]} |
          {'receive', {integer(), integer()}, [#clause{}]} |
          {'if', {integer(), integer()}, [#clause{}]} |
          {atom, {integer(), integer()} | [location_file()], atom()} |
          {'case', {integer(), integer()}, union(), [#clause{}]} |
          {var, {integer(), integer()}, atom()} |
          {op,
           {integer(), integer()},
           '>=' | '!' | '+',
           union(),
           union()} |
          {integer, {integer(), integer()}, integer()} |
          {call,
           {integer(), integer()} | [location_file()],
           union() | {remote, {integer(), integer()}, union(), union()},
           [{'fun', {integer(), integer()}, {clauses, [#clause{}]}} |
            union()]} |
          {tuple, {integer(), integer()}, [union()]} |
          {string,
           {integer(), integer()} | [location_file()],
           [integer()]}.

-record(clause,{field1 :: {integer(), integer()},
                field2 :: [union()],
                field3 :: [[union()]],
                field4 :: list()}).

-type eof_attribute_function() ::
          {eof, {integer(), integer()}} |
          {attribute,
           {integer(), integer()} | [location_file()],
           module | file | compile,
           {[integer()], integer()} | export_all | bead} |
          {function,
           {integer(), integer()} | [location_file()],
           atom(),
           integer(),
           [#clause{}]}.

-type cd_c_tuple() :: {cd | c, integer()}.

-type tuple() ::
          {atom(), {bound, used | unused, [{integer(), integer()}]}}.

-type ok() :: {ok, list()}.

-type if_receive_case_tuple() ::
          {'if' | 'receive' | 'case', {integer(), integer()}}.

-spec add_lint_warning({{integer(), integer()}, erl_lint, export_all},
                       [integer()],
                       #lint{}) ->
                          #lint{}.

-spec add_warning([location_file()], export_all, #lint{}) -> #lint{}.

-spec anno_set_file(union(), [integer()]) -> union().

-spec any_control_characters([integer()]) -> false.

-spec attribute_state(eof_attribute_function(), #lint{}) -> #lint{}.

-spec auto_import_suppressed([atom_outdir()]) -> {integer(), nil}.

-spec bif_clash_specifically_disabled(#lint{}, cd_c_tuple()) -> false.

-spec bif_clashes([eof_attribute_function()], #lint{}) -> #lint{}.

-spec bool_option(atom(), atom(), boolean(), [atom_outdir()]) ->
                     boolean().

-spec call_function([location_file()], cd | c, integer(), #lint{}) ->
                       #lint{}.

-spec check_module_name(bead, [location_file()], #lint{}) -> #lint{}.

-spec check_unused_vars([tuple()], list(), #lint{}) ->
                           {[tuple()], #lint{}}.

-spec clause(#clause{}, #lint{}) -> {[tuple()], #lint{}}.

-spec clauses([#clause{}], #lint{}) -> #lint{}.

-spec compiler_options([eof_attribute_function()]) -> [export_all].

-spec define_function([location_file()],
                      worker | collector,
                      integer(),
                      #lint{}) ->
                         #lint{}.

-spec disallowed_compile_flags([eof_attribute_function()], #lint{}) ->
                                  #lint{}.

-spec do_expr_var(atom(), {integer(), integer()}, [tuple()], #lint{}) ->
                     {[tuple()], #lint{}}.

-spec eval_file_attr([eof_attribute_function()], [integer()]) ->
                        [eof_attribute_function()].

-spec eval_file_attribute([eof_attribute_function()], #lint{}) ->
                             [eof_attribute_function()].

-spec expr(union(), [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec expr_list([union()], [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec expr_var(atom(), {integer(), integer()}, [tuple()], #lint{}) ->
                  {[tuple()], #lint{}}.

-spec exprs([union()], [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec exprs_opt([union()],
                [{'Pid', pid()}],
                [bitlevel_binaries | binary_comprehension]) ->
                   ok().

-spec feature_keywords() -> map().

-spec form(eof_attribute_function(), #lint{}) -> #lint{}.

-spec forms([eof_attribute_function()], #lint{}) -> _.

-spec function([location_file()],
               worker | collector,
               integer(),
               [#clause{}],
               #lint{}) ->
                  #lint{}.

-spec function_check_max_args([location_file()], integer(), #lint{}) ->
                                 #lint{}.

-spec function_state(eof_attribute_function(), #lint{}) -> #lint{}.

-spec gexpr(union(), [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec gexpr_list([union()], [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec guard([[union()]], [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec guard_test(union(), [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec guard_test2(union(), [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec guard_tests([union()], [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec head([union()], [tuple()], #lint{}) ->
              {list(), [tuple()], #lint{}}.

-spec head([union()], [tuple()], [tuple()], #lint{}) ->
              {list(), [tuple()], #lint{}}.

-spec icrt_clause(#clause{}, [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec icrt_clauses([#clause{}], [tuple()], #lint{}) ->
                      {[[tuple()]], #lint{}}.

-spec icrt_clauses([#clause{}],
                   if_receive_case_tuple(),
                   [tuple()],
                   #lint{}) ->
                      {[tuple()], #lint{}}.

-spec icrt_export([[tuple()]],
                  [tuple()],
                  if_receive_case_tuple(),
                  #lint{}) ->
                     [tuple()].

-spec icrt_export([tuple()],
                  [tuple()],
                  if_receive_case_tuple(),
                  integer(),
                  [tuple()]) ->
                     [tuple()].

-spec imported(atom(), integer(), #lint{}) -> no.

-spec includes_qlc_hrl([eof_attribute_function()], #lint{}) -> #lint{}.

-spec is_autoimport_suppressed({integer(), nil}, cd_c_tuple()) -> false.

-spec is_gexpr_op('>=', integer()) -> true.

-spec is_local_function({integer(), nil}, cd_c_tuple()) -> false.

-spec is_valid_call(union()) -> true.

-spec is_warn_enabled(export_all | keyword_warning | bif_clash, #lint{}) ->
                         boolean().

-spec keyword_warning({integer(), integer()} | [location_file()],
                      atom(),
                      #lint{}) ->
                         #lint{}.

-spec loc({integer(), integer()} | [location_file()], #lint{}) ->
             {[integer()], {integer(), integer()}}.

-spec local_functions([eof_attribute_function()]) ->
                         {integer(),
                          {test(),
                           {pany(), {list_tuple(), nil, nil}, nil},
                           {list_tuple(), nil, nil}}}.

-spec maps_prepend(list(), cd_c_tuple(), map()) -> map().

-spec merge_annos([{integer(), integer()}], [{integer(), integer()}]) ->
                     [{integer(), integer()}].

-spec merge_state(bound, bound) -> bound.

-spec merge_used(used, used | unused) -> used.

-spec module([eof_attribute_function()], [integer()], [atom_outdir()]) ->
                _.

-spec not_deprecated([eof_attribute_function()], #lint{}) -> #lint{}.

-spec not_removed([eof_attribute_function()], #lint{}) -> #lint{}.

-spec nowarn_function(nowarn_bif_clash, [atom_outdir()]) -> list().

-spec obsolete_guard(union(), #lint{}) -> #lint{}.

-spec pack_errors(list()) -> list().

-spec pack_warnings(list()) -> list().

-spec pat_var(atom(),
              {integer(), integer()},
              [tuple()],
              list(),
              #lint{}) ->
                 {list(), [tuple()], #lint{}}.

-spec pattern(union(), [tuple()], [tuple()], #lint{}) ->
                 {list(), [tuple()], #lint{}}.

-spec pattern_list([union()], [tuple()], [tuple()], #lint{}) ->
                      {list(), [tuple()], #lint{}}.

-spec pre_scan([eof_attribute_function()], #lint{}) -> #lint{}.

-spec pseudolocals() -> [rec_mod_tuple()].

-spec remove_non_visible([integer()]) -> [integer()].

-spec return_status(#lint{}) -> ok().

-spec set_file([union()], [integer()]) -> [union()].

-spec set_form_file(eof_attribute_function(), [integer()]) ->
                       eof_attribute_function().

-spec start([integer()], [atom_outdir()]) -> #lint{}.

-spec start_state(eof_attribute_function(), #lint{}) -> #lint{}.

-spec unused_vars([tuple()], list(), #lint{}) -> list().

-spec value_option(warn_format,
                   integer(),
                   warn_format,
                   integer(),
                   nowarn_format,
                   integer(),
                   [atom_outdir()]) ->
                      integer().

-spec vtmerge([tuple()], [tuple()]) -> [tuple()].

-spec vtmerge_pat([tuple()], [tuple()], #lint{}) -> {[tuple()], #lint{}}.

-spec vtnew(list(), list()) -> list().

-spec vtupdate([tuple()], [tuple()]) -> [tuple()].

-spec warn_invalid_call({integer(), integer()}, union(), #lint{}) ->
                           #lint{}.

-spec warn_unused_vars(list(), [tuple()], #lint{}) ->
                          {[tuple()], #lint{}}.

