
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


error: {error,function_clause,
              [{lists,map_1,
                      [#Fun<collect.4.105128635>,eof],
                      [{file,"lists.erl"},{line,1319}]},
               {lists,map,2,[{file,"lists.erl"},{line,1315}]},
               {collect,map_with_index,3,
                        [{file,"erlang/src/collect.erl"},{line,177}]},
               {collect,collect_loop,0,
                        [{file,"erlang/src/collect.erl"},{line,60}]}],
              [[46|eof],
               {erl_scan,fun erl_scan:f_reserved_word/1,#Fun<epp.4.4871482>,
                         false,false,true},
               49,18,
               [{')',{49,17}},
                {var,{49,16},'L'},
                {'(',{49,15}},
                {atom,{49,11},sort},
                {':',{49,10}},
                {atom,{49,5},lists},
                {'->',{48,9}},
                {')',{48,7}},
                {var,{48,6},'L'},
                {'(',{48,5}},
                {atom,{48,1},test}],
               #Fun<erl_scan.0.53253250>,[]]}
               
               
               5> error: {error,function_clause,
              [{lists,map_1,
                      [#Fun<collect.4.105128635>,eof],
                      [{file,"lists.erl"},{line,1319}]},
               {lists,map,2,[{file,"lists.erl"},{line,1315}]},
               {collect,map_with_index,3,
                        [{file,"erlang/src/collect.erl"},{line,177}]},
               {collect,collect_loop,0,
                        [{file,"erlang/src/collect.erl"},{line,60}]}],
                        
              [[46|eof],
               {erl_scan,fun erl_scan:f_reserved_word/1,#Fun<epp.4.4871482>,
                         false,false,true},
               49,18,
               [{')',{49,17}},
                {var,{49,16},'L'},
                {'(',{49,15}},
                {atom,{49,11},sort},
                {':',{49,10}},
                {atom,{49,5},lists},
                {'->',{48,9}},
                {')',{48,7}},
                {var,{48,6},'L'},
                {'(',{48,5}},
                {atom,{48,1},test}],
               []]}
               
               
               5> error: {error,function_clause,
              [{lists,map_1,
                      [#Fun<collect.4.105128635>,eof],
                      [{file,"lists.erl"},{line,1319}]},
               {lists,map,2,[{file,"lists.erl"},{line,1315}]},
               {collect,map_with_index,3,
                        [{file,"erlang/src/collect.erl"},{line,177}]},
               {collect,collect_loop,0,
                        [{file,"erlang/src/collect.erl"},{line,60}]}],
              [[46|eof],
               {erl_scan,fun erl_scan:f_reserved_word/1,#Fun<epp.4.4871482>,
                         false,false,true},
               49,18,
               [{')',{49,17}},
                {var,{49,16},'L'},
                {'(',{49,15}},
                {atom,{49,11},sort},
                {':',{49,10}},
                {atom,{49,5},lists},
                {'->',{48,9}},
                {')',{48,7}},
                {var,{48,6},'L'},
                {'(',{48,5}},
                {atom,{48,1},test}]]}5>
				
				
- git: https://github.com/example-user/my-repo.git
  commit: 08c9b4cdf977d5bcd1baba046a007940c1940758


- github: example-user/my-repo
  commit: 08c9b4cdf977d5bcd1baba046a007940c1940758
  subdirs:
  - my-package


https://github.com/hamler-lang/Erlang/tree/master/src/Erlang


whoops:
$6727 -> <{'MainPid', $5281} | {'F', $5281} | {'FinishedNum', $5281} | {'E', $5281} | {'MaxNum', $5281} | {'Pid', <pid() | $5281>}>

what is this pid() doing here???

also what is this:

module(list($6659), list(integer()), list($5006)) -> ?
forms(list($6659), $5280) -> ?

did it throw or what?


less agressive shquashing?
limit the number of different tags between the inpected tagsets

also investigate the large number of any()

upcast list(?) to list() or list(any)
upcast atom unions larger than a certain number
upcast false, true <-- this should be done really early on

container types should already be handled in the tracer part
this means that the FromTerm instance of ErlType should also be extended
also update as well

there is a list() inside of a union for some reason???

Less agressive squashing #1:
- at least one similar same-tagged type between two sets

Less agressive squashing #2:
- number of missing tags must be under a threshold, at least one same-tagged type
