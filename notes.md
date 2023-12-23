# Compile necessary Erlang files


erlc -I erlang\include\ erlang\src\collect.erl


Pid = collect:start_erlang_trace(erl_scan).
cd("erlang/src").
c(bead).
collect:stop_erlang_trace(Pid).
{ok, B} = file:read_file("out.bin"), file:write_file("out1.erlterm", io_lib:print(binary_to_term(B))).


Pid = collect:start_erlang_trace(constraints).


Pid = collect:start_erlang_trace(erl_lint).
cd("erlang/src").
c(bead).
collect:stop_erlang_trace(Pid).


 stack run squasher-exe -- --out myerl.erl C:\Users\User\source\repos\squasher\erlang\src\out_erl_lint.bin


c(collect, [{i, "../include"}]).


To see what a parse transform does with your code use the -P compilation flag either directly as "erlc -P" or in the shell as "c("filename.erl", ['P'])". This flag will give you the code after preprocessing and the parse transforms have been applied.

c("erlang/src/bead.erl", ['P']).


1> gradualizer:type_check_file("path/to/some_file.erl").

You can also use the Rebar3 shell.

gradualizer:type_check_file("test/should_pass/any.erl").

 collect:prepare_trace().

 Fs = filelib:wildcard("test/should_pass/*.erl").
 
 timer:tc(fun() -> lists:foreach(fun gradualizer:type_check_file/1, Fs) end).
 collect:stop_erlang_trace("gradualizer_tyvar.bin").
 
stack run squasher-exe -- -u -s S2 --out myerl1.erl C:\Users\User\Desktop\testdata\gradualizer_int.bin   

stack run squasher-exe -- -u -s S2 --out myerl1.erl C:\Users\User\Desktop\testdata\out1.bin

stack run squasher-exe -- -u -s S2 --out myerl1.erl C:\Users\User\Desktop\testdata\gradualizer_typechecker.bin

1> c("erlang/src/squasher_trans.erl").
{ok,squasher_trans}
2> c("erlang/src/tree.erl").
{ok,tree}
collect:prepare_trace().
tree:eval(tree:t1()).
collect:save_traces("outsimple.bin").


-compile({parse_transform, squasher_trans}).
-save_to("C:/Users/User/Desktop/testdata/constr_simp_arg.bin").

{eunit_tests, [{setup, fun collect:prepare_trace/0, fun collect:save_traces_/1, {dir, "C:\\Users\\User\\Desktop\\testdata\\etylizer\\_build\\test\\lib\\ety\\test"}}]}.


https://github.com/inaka/elvis_core/tree/main/src
https://github.com/inaka/zipper
https://github.com/inaka/elvis

stack run squasher-exe -- -u -s S3 -p C:\Users\User\Desktop\testdata\elvis_arg.bin --out myerl1.erl C:\Users\User\Desktop\testdata\elvis.bin

stack run squasher-exe -- -u -s S3 -p C:\Users\User\Desktop\testdata\ssl_verify_fingerprintarg.bin --out myerl1.erl C:\Users\User\Desktop\testdata\ssl_verify_fingerprint.bin
