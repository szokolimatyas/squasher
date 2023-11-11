
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




Pid = collect:start_erlang_trace(erl_lint).
cd("erlang/src").
c(bead).
collect:stop_erlang_trace(Pid).


 stack run squasher-exe -- --out myerl.erl C:\Users\User\source\repos\squasher\erlang\src\out_erl_lint.bin


c(collect, [{i, "../include"}]).