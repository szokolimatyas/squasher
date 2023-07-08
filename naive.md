Aliases:
$14438 -> {'case', {integer(), integer()}, $28376, list($28450)}
$14451 -> {'receive', ?, list($28450)}
$16581 -> {'integer', {integer(), integer()}, integer()}
$16822 -> {'if', {integer(), integer()}}
$16824 -> {'receive', {integer(), integer()}}
$17036 -> {'case', {integer(), integer()}}
$26070 -> {'if', ?, list($28450)}
$26113 -> {'c', integer()}
$26114 -> {'cd', integer()}
$26122 -> {'ok', list(?)}
$26148 -> {'string', {integer(), integer()} | list($28463 | $28465), list(integer())}
$28366 -> {'generate', ?, ?, any() | $28938}
$28376 -> {'call', {integer(), integer()} | list($28463 | $28465), $28441 | $28938, list(any() | $26148 | $28441 | $28938)}
$28377 -> {'lc', ?, $28376, list($28366)}
$28378 -> {'match', ?, $28938, $28376 | $28377}
$28441 -> {'atom', {integer(), integer()} | list($28463 | $28465) | list($28465) | list($28465), 'bead' | 'c' | 'cd' | 'collector' | 'exit' | 'false' | 'self' | 'true' | 'worker' | boolean()}
$28445 -> {'op', {integer(), integer()}, '!' | '+' | '>=', $28938, $16581 | $28441 | $28447 | $28938}
$28447 -> {'tuple', {integer(), integer()}, list(any() | $28441 | $28938)}
$28450 -> {'clause', {integer(), integer()}, list(any() | $28441 | $28447 | $28938), list(list($28441 | $28445)), list(any() | $14438 | $14451 | $26070 | $28376 | 
$28378 | $28441 | $28445)}
$28451 -> {'function', list($28463 | $28465), 'collector' | 'pany' | 'worker', integer(), list($28450)}
$28463 -> {'file', list(integer())}
$28465 -> {'location', {integer(), integer()}}
$28466 -> {'attribute', list($28463 | $28465), 'compile' | 'module', 'bead'}
$28911 -> {'FinishedNum', $29105}
$28919 -> {'MainPid', $29105}
$28927 -> {'MaxNum', $29105}
$28938 -> {'var', {integer(), integer()}, 'Collector' | 'E' | 'F' | 'FinishedNum' | 'L' | 'MainPid' | 'MaxNum' | 'Pid' | 'Pids' | 'Res' | '_'}
$29026 -> {'E', $29105}
$29066 -> {'F', $29105}
$29105 -> {'bound', 'unused' | 'used', list({integer(), integer()} | {integer(), ?})}
$29106 -> {'Pid', $29105 | pid()}
$29109 -> {'outdir', list(integer())}
$29110 -> {'test', integer()}
$29111 -> {'pany', integer()}
$29112 -> {'collector', integer()}
$29118 -> {'worker', integer()}
$29119 -> {'module_info', integer()}
$29120 -> {'record_info', integer()}
$29121 -> {'usage', #{}, list(?), {integer(), 'nil'}, #{}}
$29122 -> {'lint', 'attribute' | 'function' | 'start', '' | 'bead', list(?), {integer(), {any(), {any(), 'nil', 'nil'}, 'nil'}} | {integer(), {$29119, {$29119, 'nil', 'nil'}, 'nil'}}, list(?), list('binary_comprehension' | 'bitlevel_binaries' | 'export_all' | 'report_errors' | 'report_warnings' | any() | $29109), #{}, {integer(), 'nil'} | {integer(), {$29110, {$29111, {$29112, 'nil', 'nil'}, 'nil'}, {$29118, 'nil', 'nil'}}} | {integer(), any()}, {integer(), 'nil'}, {integer(), {any(), {any(), any(), 'nil'}, {any(), 'nil', any()}}} | {integer(), {$29119, {$29119, 'nil', 'nil'}, {$29120, 'nil', 'nil'}}} | {integer(), {$29119, {$29119, 'nil', 'nil'}, {$29120, 'nil', {$29118, 'nil', 'nil'}}}} | {integer(), {$29119, {$29119, 'nil', 'nil'}, {$29120, 'nil', 'nil' | {$29118, 'nil', 'nil'} | {?, 'nil', 'nil'} | {?, ?, 'nil'}}}} | {integer(), {$29119, {$29119, 'nil' | {$29112, 'nil', 'nil'} | {?, 'nil', 'nil'} | {?, ?, 'nil'}, 'nil'}, {$29120, 'nil', {$29118, 'nil', 'nil'}}}} | {integer(), {$29119, {$29119, 'nil' | {$29112, 'nil', 'nil'} | {?, 'nil', 'nil'} | {?, ?, 'nil'}, 'nil'}, {$29120, 'nil', 'nil' | {$29118, 'nil', 'nil'} | {?, 'nil', 'nil'} | {?, ?, 'nil'}}}}, list(?), integer(), list(?), list(?), {integer(), 'nil'}, $29112 | $29118 | list(?), list(?), integer(), list('bif_clash' | 'deprecated_function' | 'deprecated_type' | 'export_all' | 'nif_inline' | 'obsolete_guard' | 'removed' | 'shadow_vars' | 'underscore_match' | 'unused_function' | 'unused_record' | 'unused_type' | 'unused_vars'), list(?), list(?), list({list(integer()), {{integer(), ?}, 'erl_lint', 'export_all'} | {{?, integer()}, 'erl_lint', 'export_all'}}), list(integer()), 'false', 'false', list({any(), any()} | {$26113, ?} | {$26114, integer() | list($28463 | $28465)} | {$26114, list($28463 | $28465)} 
| {$29119, integer()} | {$29119, integer() | list($28463 | $28465) | list($28465)} | {$29120, ?} | {$29120, list($28463 | $28465)}), 'undefined', any() | $29121, #{}, #{}, #{}, #{}, {integer(), 'nil'}, #{}, 'none', 'guard', 'false'}

Functions:
add_lint_warning/3 -> fun(({{integer(), ?}, 'erl_lint', 'export_all'} | {{?, integer()}, 'erl_lint', 'export_all'}, list(integer()), $29122) -> $29122)
add_warning/3 -> fun((list($28463 | $28465), 'export_all', $29122) -> $29122)
anno_set_file/2 -> fun(($28376, list(integer())) -> $28376)
any_control_characters/1 -> fun(list(integer()) -> 'false')
attribute_state/2 -> fun(($28451, $29122) -> $29122)
auto_import_suppressed/1 -> fun(list('export_all' | 'report_errors' | 'report_warnings' | $29109) -> {integer(), 'nil'})
bif_clash_specifically_disabled/2 -> fun(($29122, $26113 | $26114) -> 'false')
bif_clashes/2 -> fun((list(any() | $28451), $29122) -> $29122)
bool_option/4 -> fun(('warn_bif_clash' | 'warn_deprecated_function' | 'warn_deprecated_type' | 'warn_export_all' | 'warn_export_vars' | 'warn_keywords' | 'warn_missing_spec' | 'warn_missing_spec_all' | 'warn_nif_inline' | 'warn_obsolete_guard' | 'warn_removed' | 'warn_shadow_vars' | 'warn_underscore_match' | 'warn_untyped_record' | 'warn_unused_function' | 'warn_unused_import' | 'warn_unused_record' | 'warn_unused_type' | 'warn_unused_vars', 'nowarn_bif_clash' | 'nowarn_deprecated_function' | 'nowarn_deprecated_type' | 'nowarn_export_all' | 'nowarn_export_vars' | 'nowarn_keywords' | 'nowarn_missing_spec' | 'nowarn_missing_spec_all' | 'nowarn_nif_inline' | 'nowarn_obsolete_guard' | 'nowarn_removed' | 'nowarn_shadow_vars' | 'nowarn_underscore_match' | 'nowarn_untyped_record' | 'nowarn_unused_function' | 'nowarn_unused_import' | 'nowarn_unused_record' | 'nowarn_unused_type' | 'nowarn_unused_vars', boolean(), list('binary_comprehension' | 'bitlevel_binaries' | 'export_all' | 'report_errors' | 'report_warnings' | $29109)) -> boolean())
call_function/4 -> fun((list($28463 | $28465), 'c' | 'cd', integer(), $29122) -> $29122)
check_module_name/3 -> fun(('bead', list($28463 | $28465), $29122) -> $29122)
check_unused_vars/3 -> fun((list($29026 | $29066 | $29106), list(?), $29122) -> {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122})
clause/2 -> fun(($28450, $29122) -> {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122})
clauses/2 -> fun((list($28450), $29122) -> $29122)
compiler_options/1 -> fun(list(any() | $28451) -> list('export_all'))
define_function/4 -> fun((list($28463 | $28465), 'collector' | 'worker', integer(), $29122) -> $29122)
disallowed_compile_flags/2 -> fun((list(any() | $28451), $29122) -> $29122)
do_expr_var/4 -> fun(('E' | 'F' | 'FinishedNum' | 'MainPid' | 'MaxNum' | 'Pid', {integer(), integer()}, list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), 
$29122) -> {list($28911), $29122} | {list($28919), $29122} | {list($28927), $29122} | {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122})    
eval_file_attr/2 -> fun((list(any() | $28451), list(integer())) -> list(any() | $28451 | $28466))
eval_file_attribute/2 -> fun((list(any() | $28451), $29122) -> list(any() | $28451))
expr/3 -> fun((any() | $14438 | $14451 | $26070 | $28376 | $28441 | $28445 | $28447 | $28938, list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28919), $29122} | {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122} | {list(?), $29122})
expr_list/3 -> fun((list($26148 | $28441 | $28445 | $28447 | $28938), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28919), $29122} 
| {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122} | {list(?), $29122})
expr_var/4 -> fun(('E' | 'F' | 'FinishedNum' | 'MainPid' | 'MaxNum' | 'Pid', {integer(), integer()}, list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28911), $29122} | {list($28919), $29122} | {list($28927), $29122} | {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122})       
exprs/3 -> fun((list(any() | $14438 | $14451 | $26070 | $28376 | $28445), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28919), $29122} | {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122} | {list(?), $29122})
exprs_opt/3 -> fun((list($28376), list($29106), list('binary_comprehension' | 'bitlevel_binaries')) -> $26122)
feature_keywords/0 -> fun(() -> #{})
form/2 -> fun((any() | $28451 | $28466, $29122) -> $29122)
forms/2 -> fun((list(any() | $28451), $29122) -> ?)
function/5 -> fun((list($28463 | $28465), 'collector' | 'worker', integer(), list($28450), $29122) -> $29122)
function_check_max_args/3 -> fun((list($28463 | $28465), integer(), $29122) -> $29122)
function_state/2 -> fun(($28451, $29122) -> $29122)
gexpr/3 -> fun(($28441 | $28445 | $28938, list($28911 | $28919 | $28927), $29122) -> {list($28911), $29122} | {list($28927), $29122} | {list(?), $29122})
gexpr_list/3 -> fun((list($28938), list($28911 | $28919 | $28927), $29122) -> {list($28911), $29122} | {list($28927), $29122})
guard/3 -> fun((list(list($28441 | $28445)), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28911), $29122} | {list($28927), $29122} 
| {list(?), $29122})
guard_test/3 -> fun(($28441 | $28445, list($28911 | $28919 | $28927), $29122) -> {list($28911), $29122} | {list($28927), $29122} | {list(?), $29122})
guard_test2/3 -> fun(($28441 | $28445, list($28911 | $28919 | $28927), $29122) -> {list($28911), $29122} | {list($28927), $29122} | {list(?), $29122})
guard_tests/3 -> fun((list($28441 | $28445), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28911), $29122} | {list($28927), $29122} 
| {list(?), $29122})
head/3 -> fun((list($28441 | $28447 | $28938), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list(?), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122})
head/4 -> fun((list($28441 | $28447 | $28938), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list(?), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122})
icrt_clause/3 -> fun(($28450, list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28911), $29122} | {list($28919), $29122} | {list($28927), $29122} | {list($29026), $29122} | {list($29106), $29122})
icrt_clauses/3 -> fun((list($28450), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list(list($29026)), $29122} | {list(list($29106)), $29122})
icrt_clauses/4 -> fun((list($28450), $16822 | $16824 | $17036, list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($29026), $29122} | {list($29106), $29122})
icrt_export/4 -> fun((list(list($29026 | $29106)), list($29026 | $29066 | $29106), $17036, $29122) -> list($29026 | $29106))
icrt_export/5 -> fun((list($29026 | $29106), list($29026 | $29066 | $29106), $17036, integer(), list($29026 | $29106)) -> list($29026 | $29106))
imported/3 -> fun(('c' | 'cd' | 'collector' | 'worker', integer(), $29122) -> 'no')
includes_qlc_hrl/2 -> fun((list(any() | $28451), $29122) -> $29122)
is_autoimport_suppressed/2 -> fun(({integer(), 'nil'}, $26113 | $26114) -> 'false')
is_gexpr_op/2 -> fun(('>=', integer()) -> 'true')
is_local_function/2 -> fun(({integer(), 'nil'}, $26113 | $26114) -> 'false')
is_valid_call/1 -> fun($28938 -> 'true')
is_warn_enabled/2 -> fun(('bif_clash' | 'export_all' | 'keyword_warning', $29122) -> boolean())
keyword_warning/3 -> fun(({integer(), integer()} | {?, integer()} | list($28463 | $28465), 'bead' | 'c' | 'cd' | 'collector' | 'false' | 'worker' | boolean(), $29122) -> $29122)
loc/2 -> fun(({integer(), integer()} | {?, integer()} | list($28463 | $28465), $29122) -> {list(integer()), {integer(), integer()}})
local_functions/1 -> fun(list(any() | $28451) -> {integer(), {$29110, {$29111, {$29112, 'nil', 'nil'}, 'nil'}, {$29118, 'nil', 'nil'}}})
maps_prepend/3 -> fun((list(?), $26113 | $26114, #{}) -> #{})
merge_annos/2 -> fun((list({integer(), integer()}), list({integer(), integer()})) -> list({integer(), integer()}))
merge_state/2 -> fun(('bound', 'bound') -> 'bound')
merge_used/2 -> fun(('used', 'unused' | 'used') -> 'used')
module/3 -> fun((list(any() | $28451), list(integer()), list('report_errors' | 'report_warnings' | $29109)) -> ?)
not_deprecated/2 -> fun((list(any() | $28451), $29122) -> $29122)
not_removed/2 -> fun((list(any() | $28451), $29122) -> $29122)
nowarn_function/2 -> fun(('nowarn_bif_clash', list('binary_comprehension' | 'bitlevel_binaries' | 'export_all' | 'report_errors' | 'report_warnings' | $29109)) -> 
list(?))
obsolete_guard/2 -> fun(($28441 | $28445, $29122) -> $29122)
pack_errors/1 -> fun(list(?) -> list(?))
pack_warnings/1 -> fun(list(?) -> list(?))
pat_var/5 -> fun(('E' | 'F' | 'FinishedNum' | 'MainPid' | 'MaxNum' | 'Pid', {integer(), integer()}, list($28911 | $28919 | $28927), list(?), $29122) -> {list(?), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122})
pattern/4 -> fun(($28441 | $28447 | $28938, list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list(?), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122})
pattern_list/4 -> fun((list($28441 | $28938), list($28911 | $28919 | $28927), list($28911 | $28919 | $28927), $29122) -> {list(?), list($29026), $29122})
pre_scan/2 -> fun((list(any() | $28451), $29122) -> $29122)
pseudolocals/0 -> fun(() -> list($29119 | $29120))
remove_non_visible/1 -> fun(list(integer()) -> list(integer()))
return_status/1 -> fun($29122 -> $26122)
set_file/2 -> fun((list($28376), list(integer())) -> list($28376))
set_form_file/2 -> fun((any() | $28451, list(integer())) -> any() | $28451)
start/2 -> fun((list(integer()), list('binary_comprehension' | 'bitlevel_binaries' | 'export_all' | 'report_errors' | 'report_warnings' | $29109)) -> $29122)      
start_state/2 -> fun(($28466, $29122) -> $29122)
unused_vars/3 -> fun((list($29026 | $29066 | $29106), list(?), $29122) -> list(?))
value_option/7 -> fun(('warn_format', integer(), 'warn_format', integer(), 'nowarn_format', integer(), list('binary_comprehension' | 'bitlevel_binaries' | 'export_all' | 'report_errors' | 'report_warnings' | $29109)) -> integer())
vtmerge/2 -> fun((list($28911 | $28927 | $29026 | $29066), list($28911 | $29026 | $29106)) -> list($28911 | $28927 | $29026 | $29066 | $29106))
vtmerge_pat/3 -> fun((list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), $29122) -> {list($28911), $29122} | {list($28919), $29122} | {list($28927), $29122} | {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122} | {list(?), $29122})       
vtnew/2 -> fun((list(?), list(?)) -> list(?))
vtupdate/2 -> fun((list($28911 | $28919 | $28927 | $29026 | $29066 | $29106), list($28911 | $28919 | $28927 | $29026 | $29066 | $29106)) -> list($28911 | $28919 | 
$28927 | $29026 | $29066 | $29106))
warn_invalid_call/3 -> fun(({integer(), integer()}, $28938, $29122) -> $29122)
warn_unused_vars/3 -> fun((list(?), list($29026 | $29066 | $29106), $29122) -> {list($29026), $29122} | {list($29066), $29122} | {list($29106), $29122})
