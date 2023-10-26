%%% Aliases:
-module(formatted).

-type bound() :: {bound, used | unused, list({integer(), integer()})}.
-type t1() :: bitlevel_binaries | export_all | {outdir, list(integer())} | report_warnings | binary_comprehension | report_errors.
-type module_info() :: {module_info, integer()}.
-type record_info() :: {record_info, integer()}.

-type lint() :: {'lint', 'start' | 'attribute' | 'function', '' | 'bead', list(?), {integer(), {module_info(), {module_info(), 'nil', 'nil'}, 'nil'}}, list(?), list(t1() | 'bitlevel_binaries' | 'export_all' | 'report_warnings' | 'binary_comprehension' | 'report_errors'), #{}, {integer(), 'nil'} | {integer(), {{'test', integer()}, {{'pany', integer()}, {t3(), 'nil', 'nil'}, 'nil'}, {t3(), 'nil', 'nil'}}}, {integer(), 'nil'}, {integer(), {module_info(), {module_info(), 'nil', 'nil'}, {record_info(), 'nil', 'nil'}}} | {integer(), {module_info(), {module_info(), 'nil', 'nil'}, {record_info(), 'nil', {t3(), 'nil', 'nil'}}}} | {integer(), {module_info(), {module_info(), {t3(), 'nil', 'nil'}, 'nil'}, {record_info(), 'nil', {t3(), 'nil', 'nil'}}}}, list(?), integer(), list(?), list(?), {integer(), 'nil'}, list(?) | t3(), list(?), integer(), list('unused_record' | 'underscore_match' | 'unused_function' | 'shadow_vars' | 'obsolete_guard' | 'deprecated_function' | 'removed' | 'export_all' | 'bif_clash' | 'nif_inline' | 'unused_type' | 'unused_vars' | 'deprecated_type'), list(?), list(?), list({list(integer()), {{integer(), integer()}, 'erl_lint', 'export_all'}}), list(integer()), 'false', 'false', list({any(), any()}), 'undefined', {'usage', #{}, list(?), {integer(), 'nil'}, #{}}, #{}, #{}, #{}, #{}, {integer(), 'nil'}, #{}, 'none', 'guard', 'false'}.

-type clause() :: {'clause', {integer(), integer()}, list(t4()), list(list(t4())), list(t4())}.
-type clauses() :: {'clauses', list(clause())}.
-type t2() :: {'FinishedNum', bound()} | 
              {'E', bound()} | {'MaxNum', bound()} | 
              {'Pid', bound()} | 
              {'MainPid', bound()} | 
              {'F', bound()}.
-type t3() :: {'worker', integer()} | 
              {'collector', integer()}.
-type file_location() :: {'location', {integer(), integer()}} | 
                         {'file', list(integer())}.
-type t4() :: {'var', {integer(), integer()}, atom()} | 
              {'match', {integer(), integer()}, t4(), t4()} | 
              {'receive', {integer(), integer()}, list(clause())} | 
              {'case', {integer(), integer()}, t4(), list(clause())} | 
              {'op', {integer(), integer()}, '>=' | '!' | '+', t4(), t4()} | 
              {'tuple', {integer(), integer()}, list(t4())} | 
              {'string', {integer(), integer()} | list($1833), list(integer())} | 
              {'atom', {integer(), integer()} | list($1833), atom()} | 
              {'integer', {integer(), integer()}, integer()} | 
              {'if', {integer(), integer()}, list(clause())} | 
              {'lc', {integer(), integer()}, t4(), list({'generate', {integer(), integer()}, t4(), t4()})} | 
              {'call', {integer(), integer()} | list($1833), t4() | 
              {'remote', {integer(), integer()}, t4(), t4()}, list({'fun', {integer(), integer()}, clauses()} | t4())}.
-type t5() :: {'eof', {integer(), integer()}} | 
              {'function', {integer(), integer()} | list($1833), 'worker' | 'pany' | 'test' | 'collector', integer(), list(clause())} | 
              {'attribute', {integer(), integer()} | list($1833), 'module' | 'file' | 'compile', {list(integer()), integer()} | 'export_all' | 'bead'}.

%%% Functions:

-spec add_lint_warning({{integer(), integer()}, 'erl_lint', 'export_all'}, list(integer()), lint()) -> lint().
-spec add_warning(list($1833), 'export_all', lint()) -> lint().
-spec anno_set_file(t4(), list(integer())) -> t4().
-spec any_control_characters(list(integer())) -> 'false'.
-spec attribute_state(t5(), lint()) -> lint().
-spec auto_import_suppressed(list(t1())) -> {integer(), 'nil'}.
-spec bif_clash_specifically_disabled(lint(), {'cd', integer()} | {'c', integer()}) -> 'false'.
-spec bif_clashes(list(t5()), lint()) -> lint().
-spec bool_option('warn_unused_import' | 'warn_missing_spec' | 'warn_unused_function' | 'warn_removed' | 'warn_obsolete_guard' | 'warn_export_all' | 'warn_deprecated_function' | 'warn_export_vars' | 'warn_missing_spec_all' | 'warn_deprecated_type' | 'warn_unused_vars' | 'warn_untyped_record' | 'warn_bif_clash' | 'warn_unused_record' | 'warn_unused_type' | 'warn_keywords' | 'warn_nif_inline' | 'warn_shadow_vars' | 'warn_underscore_match'>, 'nowarn_bif_clash' | 'nowarn_unused_type' | 'nowarn_keywords' | 'nowarn_removed' | 'nowarn_export_vars' | 'nowarn_nif_inline' | 'nowarn_export_all' | 'nowarn_obsolete_guard' | 'nowarn_unused_record' | 'nowarn_unused_vars' | 'nowarn_deprecated_type' | 'nowarn_untyped_record' | 'nowarn_deprecated_function' | 'nowarn_unused_function' | 'nowarn_missing_spec_all' | 'nowarn_shadow_vars' | 'nowarn_unused_import' | 'nowarn_underscore_match' | 'nowarn_missing_spec', boolean(), list(t1())) -> boolean().
-spec call_function(list($1833), 'cd' | 'c', integer(), lint()) -> lint().
-spec check_module_name('bead', list($1833), lint()) -> lint().
-spec check_unused_vars(list(t2()), list(?), lint()) -> {list(t2()), lint()}.
-spec clause(clause(), lint()) -> {list(t2()), lint()}.
-spec clauses(list(clause()), lint()) -> lint().
-spec compiler_options(list(t5())) -> list('export_all').
-spec define_function(list($1833), 'worker' | 'collector', integer(), lint()) -> lint().
-spec disallowed_compile_flags(list(t5()), lint()) -> lint().
-spec do_expr_var('MaxNum' | 'Pid' | 'MainPid' | 'F' | 'E' | 'FinishedNum', {integer(), integer()}, list(t2()), lint()) -> {list(t2()), lint()}.
-spec eval_file_attr(list(t5()), list(integer())) -> list(t5()).
-spec eval_file_attribute(list(t5()), lint()) -> list(t5()).
