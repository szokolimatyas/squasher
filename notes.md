
{'if', ?, list(<$12198>)}

$12198 should not be in a union!


upcast unions like this?:

$13736 -> <{'E', $6510} | {'F', $6510} | {'FinishedNum', $6510} | {'MainPid', $6510} | {'MaxNum', $6510} | {'Pid', $6510}>

{ 'E' | 'F' | 'FinishedNum' | 'MainPid' | 'MaxNum' | 'Pid', $6510 }
or
{ atom(), $6510 }


for sure we want to combine things like this:

<{integer(), integer()} | {integer(), ?}>

we can also try to avoid generating traces like this, but how?

the problem is here as expected:

$13797 -> <{'attribute', ?, 'compile', ?} | {'function', list($13713), <'pany' | 'worker'>, integer(), list($13350)} | any()>
attribute_state/2 -> fun(($13797, $11883) -> $11883)

most likely attribute_state only works on attributes, but according to the spec it also works on functions!

Next todo:

add a subtypeOf :: ErlType -> ErlType -> Bool
lub, glb use it
also make combine use it?
combine should accept more tuples that have elements that are compatible
or somehow do structural squashing for non-recordlike tuples ---> this is pretty necessary!!
also investigate the union size, improvements of it --> kindof done
and why the hell the start of the program slowed down so much ---> combine

LARGE: rethink the combine operation on unions!! from the ground up!
global squash a still a bit slow ---> perf and code quality improvements are needed!

Need to clean up equate and union creation, maybe take some
notes from the original impl.

Do we need the tag squash for unions if we have the equate?
Or the reverse?


the any comes from:

{$11, integer()},
{$12, ?},
{$26, integer()},
{$27, ?},
{$44, integer()},
{$45, ?},
{$59, integer()},
{$60, ?},
{$118, integer()},
{$119, ?},
{$135, integer()},
{$136, ?},
{$146, integer()},
{$147, ?},
{$275, integer()},
{$276, ?},
{$290, integer()},
{$291, ?},
{$304, integer()},
{$305, ?},
{$313, ?},
{$314, list($315)},
{$316, list($317)},
{$318, integer()},
{$335, integer()},
{$336, ?},
{$350, integer()},
{$351, ?},
{$374, integer()},
{$375, ?},
{$393, integer()},
{$394, ?},
{$412, integer()},
{$413, ?},
{$431, integer()},
{$432, ?},
...

benchmark, rethink the lub somehow,
test lazy vs strict data structures and algos


Should we use a sequence instead?
squash :: SquashConfig -> Seq Int -> [Int] -> SquashConfig
squash conf Sequence.Empty     _d = conf
squash conf@SquashConfig{..} (a1 Sequence.:<| w) d  = squash conf' (w Sequence.>< Sequence.fromList as) (a1 : d) where
    as = aliases (lookupAlias a1 aliasEnv) Data.List.\\ d
    ap = Data.List.delete a1 d
    -- refers to ai, what is it???
    f delta a2 =
        if not (shouldMerge (map (resolve delta) [EAliasMeta a1, EAliasMeta a2]))
        then delta
        else mergeAliases delta [a1, a2] -- or include the whole worklist?

    conf' =
        if a1 `elem` d then conf else foldl f conf (ap ++ as)


IntSet in global multi key squashing?

STT in equivalence?


fromList [(993,1),(994,1),(1348,1),(1360,1),(1375,1),(1376,1),(1418,1),(1490,1)]

when trying to inline something, what happens with mutually recursive aliases??

$1 -> {foo, $2}
$2 -> {bar, $1}

into:
$3 -> {zat, $2}

or:


$1 -> {foo, $2}
$2 -> {bar, $3}
$3 -> {zap, $1}

into:
$3 -> {zat, $2}



$1 -> {foo, {bar, $3}}
$2 -> {bar, {zap, $1}}
$3 -> {zap, {foo, $2}}

when substituting, see if what we substitute contains aliases that are to be inlined
- try to inline that alias, but if inlining would introduce another alias that is to be inlined, abort




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

7> bead:pany(fun(A) -> A end, [false, false, true, true, true]).
{true,true}
8> collect:get_traces().
[{pid,[{dom,1,3},{name,"collector",3}]},
 {pid,[{dom,1,3},{name,"worker",3}]},
 {{atom,false},[list_element,{dom,2,2},{name,"pany",2}]},
 {{atom,false},[{dom,3,3},{name,"worker",3}]},
 {{atom,false},[{dom,1,1},{dom,2,3},{name,"worker",3}]},
 {{atom,false},[{dom,1,1},{dom,1,2},{name,"pany",2}]},
 {{atom,false},[{rng,1},{dom,1,2},{name,"pany",2}]},
 {{atom,false},[{rng,1},{dom,2,3},{name,"worker",3}]},
 {{atom,false},[{rng,3},{name,"worker",3}]},
 {integer,[{dom,2,3},{name,"collector",3}]},
 {integer,[{dom,3,3},{name,"collector",3}]},
 {{atom,true},[list_element,{dom,2,2},{name,"pany",2}]},
 {{atom,true},[{dom,3,3},{name,"worker",3}]},
 {{atom,true},[{dom,1,1},{dom,2,3},{name,"worker",3}]},
 {{atom,true},[{dom,1,1},{dom,1,2},{name,"pany",2}]},
 {{atom,true},[{rng,1},{dom,1,2},{name,"pany",2}]},
 {{atom,true},[{rng,1},{dom,2,3},{name,"worker",3}]},
 {{atom,true},
  [{tuple_index,{atom,true},2,2},{rng,3},{name,"worker",3}]},
 {{atom,true},
  [{tuple_index,{atom,true},2,2},
   {rng,3},
   {name,"collector",3}]},
 {{atom,true},
  [{tuple_index,{atom,true},2,2},{rng,2},{name,"pany",2}]}]

Hack instead of flow analysis?

erl_syntax:is_literal/1

Returns true if Node represents a literal term, otherwise false. This function returns true if and only if the value of concrete(Node) is defined. 

The parameter is in the same position, part of a term, but not under a function call.


 Pid = collect:start_erlang_trace(constraints).
 collect:get_traces().
 
 
 $ make shell

1> gradualizer:type_check_file("path/to/some_file.erl").

You can also use the Rebar3 shell.

gradualizer:type_check_file("test/should_pass/any.erl").

 collect:start_erlang_trace(constraints).

 Fs = filelib:wildcard("test/should_pass/*.erl").
 
 lists:foreach(fun gradualizer:type_check_file/1, Fs).
 collect:stop_erlang_trace("out2.bin").
 
 
 
 https://github.com/Feuerlabs/exometer_core
 https://github.com/rabbitmq/ra/tree/main/test
 https://github.com/rabbitmq/khepri
 https://github.com/ninenines/cowboy/blob/master/test/compress_SUITE.erl
 
 free_vars({var, _, '_'}, Vars) ->
    Vars;
free_vars({var, _, X}, Vars) ->
    Vars#{ X => true };
free_vars([H | T], Vars) ->
    free_vars(T, free_vars(H, Vars));
free_vars({type, _, _, Args}, Vars) ->
    free_vars(Args, Vars);
free_vars(_, Vars) -> Vars.


rewrites to: (naive algo does not accept Vars to be optimized)
first parameter shrinks

or can Vars be accepted?
- if we extend valid_term to accept recursive calls, then it is!
  Vars is in:  free_vars(T, free_vars(H, Vars));
  and: free_vars(H, Vars);

	map update would change the subterm if we had proper support for maps
	in the same vein, does the record update operation invalidate things??

free_vars(P1, P2) -> track(free_vars(track(P1), track(P2)))

free_vars_({var, _, '_'}, Vars) ->
    Vars;
free_vars_({var, _, X}, Vars) ->
    Vars#{ X => true };
free_vars_([H | T], Vars) ->
    free_vars_(T, track(track(free_vars_(H, track(Vars)))));
free_vars_({type, _, _, Args}, Vars) ->
    free_vars_(track(Args), Vars);
free_vars_(_, Vars) -> Vars.

All-or-none, but easy optimization:
if all recursive calls are subterms in result position,
then we do not track the recursive call's results

we also need to handle growing parameters


c("erlang/src/bead.erl", ['P']).

shrinking parameters are ok, tail calls are ok

this is the problem:


collect:track(FinishedNum + 1, [{dom,   2, 3}])

we retrack the subterms multiple times,
if we had a growing param A -> {nest, A}, we would have

    A, {nest, A}, {nest, {nest, A}}, ...
    but this is reflected in the types!

    the param type (before squashing) looks like:
    A | {nest, A} | {nest, {nest, A}} | ...
    which finally leads to a recursive type

if A is integer()
A -> {A, A} with a depth limit of 1

{integer(), integer()}  | {{integer(), integer()}, {integer(), integer()}}

TODO: S3 is not good, NAMING!
the type and expression adts somehow intersect!
maybe examine the aliases after the local squash for some ideas

stack run squasher-exe -- -u -s S2 --out myerl1.erl C:\Users\User\Desktop\testdata\gradualizer_int.bin   

stack run squasher-exe -- -u -s S2 --out myerl1.erl C:\Users\User\Desktop\testdata\out1.bin

stack run squasher-exe -- -u -s S2 --out myerl1.erl C:\Users\User\Desktop\testdata\gradualizer_typechecker.bin


[{{worker,3},[[{name,"Pid"},{name,"F"},{name,"E"}]]},
 {{collector,3},[[{name,"MainPid"},{name,"FinishedNum"},{name,"MaxNum"}]]},
 {{rev,1},[[{name,"L"}]]},
 {{rev,2},[[{name,"H"},{name,"Acc"}],[no_name,{name,"Acc"}]]},
 {{dup,1},[[{name,"H"}],[no_name]]},
 {{len,1},[[no_name],[no_name]]},
 {{pany,2},[[{name,"F"},{name,"L"}]]},
 {{test,1},[[{name,"L"}]]}]