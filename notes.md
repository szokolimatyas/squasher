(fromList [RecordTag "E" 1],[5020]),
(fromList [RecordTag "E" 1,RecordTag "F" 1,RecordTag "Pid" 1],[290]),
(fromList [RecordTag "F" 1],[5021]),
(fromList [RecordTag "FinishedNum" 1],[2165]),
(fromList [RecordTag "MainPid" 1],[2179]),
(fromList [RecordTag "MaxNum" 1],[2193]),
(fromList [RecordTag "Pid" 1],[5022]),
(fromList [RecordTag "atom" 2],[5087]),
(fromList [RecordTag "atom" 2,RecordTag "string" 2],[95,69]),
(fromList [RecordTag "atom" 2,RecordTag "tuple" 2],[4922,3531,1986,128]),
(fromList [RecordTag "atom" 2,RecordTag "var" 2],[126,120,110]),
(fromList [RecordTag "bound" 2],[2206]),
(fromList [RecordTag "c" 1],[5004]),
(fromList [RecordTag "c" 1,RecordTag "cd" 1],[179]),
(fromList [RecordTag "call" 3],[5079]),
(fromList [RecordTag "call" 3,RecordTag "lc" 3],[3597,3572]),
(fromList [RecordTag "case" 3],[5081]),
(fromList [RecordTag "case" 3,RecordTag "match" 3],[3599]),
(fromList [RecordTag "case" 3,RecordTag "receive" 2],[628,457]),
(fromList [RecordTag "cd" 1],[5005]),
(fromList [RecordTag "clause" 4],[4853]),
(fromList [RecordTag "clauses" 1],[4854]),
(fromList [RecordTag "collector" 1],[5070]),
(fromList [RecordTag "file" 1],[5052]),
(fromList [RecordTag "file" 1,RecordTag "location" 1],[2009,243,215,100,93,88,78,74,33]),
(fromList [RecordTag "fun" 2],[4855]),
(fromList [RecordTag "function" 4],[5091,4941,3602,3550,2005]),
(fromList [RecordTag "if" 2],[625]),
(fromList [RecordTag "lc" 3],[5080]),
(fromList [RecordTag "lint" 37],[4952]),
(fromList [RecordTag "location" 1],[5054]),
(fromList [RecordTag "match" 3],[5083]),
(fromList [RecordTag "module_info" 1],[4949]),
(fromList [RecordTag "op" 4],[3577]),
(fromList [RecordTag "outdir" 1],[5092]),
(fromList [RecordTag "outdir" 1,SingleAtom "binary_comprehension",SingleAtom "bitlevel_binaries",SingleAtom "export_all",SingleAtom "report_errors",SingleAtom "report_warnings"],[211]),
(fromList [RecordTag "outdir" 1,SingleAtom "export_all",SingleAtom "report_errors",SingleAtom "report_warnings"],[167,3]),
(fromList [RecordTag "pany" 1],[2141]),
(fromList [RecordTag "receive" 2],[5042]),
(fromList [RecordTag "record_info" 1],[4950]),
(fromList [RecordTag "string" 2],[4979]),
(fromList [RecordTag "test" 1],[2140]),
(fromList [RecordTag "tuple" 2],[5088]),
(fromList [RecordTag "usage" 4],[4951]),
(fromList [RecordTag "var" 2],[4994]),
(fromList [RecordTag "worker" 1],[5071])

https://github.com/pa-ba/equivalence/blob/master/testsuite/tests/Data/Equivalence/Monad_Test.hs
Seems like a union find problem?

RecordTags+SingleAtom form an equivalence relation??
Might be too agressive?
Two tags are equivalent when they are in the same set?

This can lead to problems when atoms are used inconsistently!
We need some kind of sanity check for this.


there could be for instance two different options proplists,
but both have the {file, _} tuple. We should not unify them in that case.

[[RecordTag "if" 2],
 [RecordTag "test" 1],
 [RecordTag "pany" 1],
 [RecordTag "FinishedNum" 1],
 [RecordTag "MainPid" 1],
 [RecordTag "MaxNum" 1],
 [RecordTag "bound" 2],
 [RecordTag "op" 4],
 [RecordTag "clause" 4],
 [RecordTag "clauses" 1],
 [RecordTag "fun" 2],
 [RecordTag "module_info" 1],
 [RecordTag "record_info" 1],
 [RecordTag "usage" 4],
 [RecordTag "lint" 37],
 [RecordTag "c" 1,RecordTag "cd" 1],
 [RecordTag "E" 1,RecordTag "F" 1,RecordTag "Pid" 1],
 [RecordTag "file" 1,RecordTag "location" 1],
 [RecordTag "collector" 1],
 [RecordTag "worker" 1],
 [RecordTag "call" 3,RecordTag "lc" 3],
 [RecordTag "case" 3,RecordTag "match" 3,RecordTag "receive" 2],
 [RecordTag "atom" 2,RecordTag "string" 2,RecordTag "tuple" 2,RecordTag "var" 2],
 [RecordTag "function" 4],
 [RecordTag "outdir" 1,SingleAtom "binary_comprehension",SingleAtom "bitlevel_binaries",SingleAtom "export_all",SingleAtom "report_errors",SingleAtom "report_warnings"]]

(RecordTag "E" 1,[5020,290]),
(RecordTag "F" 1,[5021,290]),
(RecordTag "FinishedNum" 1,[2165]),
(RecordTag "MainPid" 1,[2179]),
(RecordTag "MaxNum" 1,[2193]),
(RecordTag "Pid" 1,[5022,290]),
(RecordTag "atom" 2,[5087,4922,3531,1986,128,126,120,110,95,69]),
(RecordTag "bound" 2,[2206]),
(RecordTag "c" 1,[5004,179]),
(RecordTag "call" 3,[5079,3597,3572]),
(RecordTag "case" 3,[5081,3599,628,457]),
(RecordTag "cd" 1,[5005,179]),
(RecordTag "clause" 4,[4853]),
(RecordTag "clauses" 1,[4854]),
(RecordTag "collector" 1,[5070]),
(RecordTag "file" 1,[5052,2009,243,215,100,93,88,78,74,33]),
(RecordTag "fun" 2,[4855]),
(RecordTag "function" 4,[5091,4941,3602,3550,2005]),
(RecordTag "if" 2,[625]),
(RecordTag "lc" 3,[5080,3597,3572]),
(RecordTag "lint" 37,[4952]),
(RecordTag "location" 1,[5054,2009,243,215,100,93,88,78,74,33]),
(RecordTag "match" 3,[5083,3599]),
(RecordTag "module_info" 1,[4949]),
(RecordTag "op" 4,[3577]),
(RecordTag "outdir" 1,[5092,211,167,3]),
(RecordTag "pany" 1,[2141]),
(RecordTag "receive" 2,[5042,628,457]),
(RecordTag "record_info" 1,[4950]),
(RecordTag "string" 2,[4979,95,69]),
(RecordTag "test" 1,[2140]),
(RecordTag "tuple" 2,[5088,4922,3531,1986,128]),
(RecordTag "usage" 4,[4951]),
(RecordTag "var" 2,[4994,126,120,110]),
(RecordTag "worker" 1,[5071]),
(SingleAtom "binary_comprehension",[211]),
(SingleAtom "bitlevel_binaries",[211]),
(SingleAtom "export_all",[211,167,3]),
(SingleAtom "report_errors",[211,167,3]),
(SingleAtom "report_warnings",[211,167,3])


lot of unnecessary unions:

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