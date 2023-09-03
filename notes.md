nodes/1 -> 
fun({'leaf', integer()} | 
    {'node', 
        {'leaf', integer()}, 
        {'leaf', integer()} | 
        {'node', 
            {'leaf', integer()}, 
            {'leaf', integer()}}} | 
    {'node', 
        ?, 
        {'leaf', integer()}} -> integer())


Aliases:
aliased type:
fun($10 -> integer())

show aliases:
$0 -> {'leaf', integer()}
$1 -> {'leaf', integer()}
$2 -> {'leaf', integer()}
$3 -> {'leaf', integer()}
$4 -> {'leaf', integer()}
$5 -> {'node', $3, $4}
$6 -> $2 | $5
$7 -> {'node', $1, $6}
$8 -> {'leaf', integer()}
$9 -> {'node', ?, $8}
$10 -> $0 | $7 | $9



$0 -> $4
$1 -> $4
$2 -> $4
$3 -> $4
$4 -> {'leaf', integer()}
$5 -> {'node', $4, $4 | $6}
$6 -> $4 | $5
$7 -> $5
$8 -> $4
$9 -> $5
$10 -> $4 | $5

Heuristic:

we can merge two aliases if:
- both are record-like
- In every (most??) occurrence of them:

$2 -> {'leaf', integer()}
$3 -> {'node', $2, $2}
$4 -> $2 | $3
$9 -> {'leaf', integer()}
$10 -> {'node', $9, $9}
$11 -> $9 | $10

Functions:
map_tree/1 -> fun((fun(integer() -> integer()), $4) -> $11)

erlc -I

erlc -I erlang\include\ erlang\src\collect.erl


Pid = collect:start_erlang_trace(erl_lint).
cd("erlang/src").
c(bead).
collect:stop_erlang_trace(Pid).
{ok, B} = file:read_file("out.bin"), file:write_file("out.erlterm", io_lib:print(binary_to_term(B))).    

show aliases:
$0 -> {'atom', {integer(), integer()}, 'c' | 'cd'}
$1 -> {'atom', {integer(), integer()}, 'bead'}
$2 -> {'atom', {?, integer()}, 'bead'}
$3 -> {'atom', ?, 'bead'}
$4 -> {'string', {integer(), integer()}, list(integer())}
$5 -> $1 | $2 | $3 | $4
$6 -> {'call', {integer(), integer()}, $0, list($5)}
$7 -> {'file', list(integer())}
$8 -> {'location', {integer(), integer()}}
$9 -> {'location', {?, integer()}}
$10 -> $7 | $8 | $9
$11 -> {'file', list(integer())}
$12 -> {'location', {integer(), integer()}}
$13 -> {'location', {?, integer()}}
$14 -> $11 | $12 | $13
$15 -> {'atom', list($14), 'c' | 'cd'}
$16 -> {'atom', ?, 'bead'}
$17 -> {'location', {integer(), integer()}}
$18 -> {'atom', list($17), 'bead'}
$19 -> {'location', {?, integer()}}
$20 -> {'atom', list($19), 'bead'}
$21 -> {'file', list(integer())}
$22 -> {'location', {integer(), integer()}}
$23 -> {'location', {?, integer()}}
$24 -> $21 | $22 | $23
$25 -> {'atom', list($24), 'bead'}
$26 -> {'file', list(integer())}
$27 -> {'location', {integer(), integer()}}
$28 -> {'location', {?, integer()}}
$29 -> $26 | $27 | $28
$30 -> {'string', list($29), list(integer())}
$31 -> $16 | $18 | $20 | $25 | $30
$32 -> {'call', list($10), $15, list($31)}


$3 -> {'atom', {integer(), integer()}, 'bead' | 'c' | 'cd'}
$4 -> {'string', {integer(), integer()}, list(integer())}
$5 -> $3 | $4
$6 -> {'call', {integer(), integer()}, $3, list($5)}
$10 -> $26 | $28
$14 -> $26 | $28
$24 -> $26 | $28
$25 -> {'atom', list($14 | $24 | $28), 'bead' | 'c' | 'cd'}
$26 -> {'file', list(integer())}
$28 -> {'location', {integer(), integer()}}
$29 -> $26 | $28
$30 -> {'string', list($29), list(integer())}
$31 -> $25 | $30
$32 -> {'call', list($10), $25, list($31)}

Functions:
anno_set_file/2 -> fun(($6, list(integer())) -> $32)

=======================================================
without aliasing unions:

show aliases:
$0 -> {'atom', {integer(), integer()}, 'c' | 'cd'}
$1 -> {'atom', {integer(), integer()}, 'bead'}
$2 -> {'atom', {?, integer()}, 'bead'}
$3 -> {'atom', ?, 'bead'}
$4 -> {'string', {integer(), integer()}, list(integer())}
$5 -> {'call', {integer(), integer()}, $0, list($1 | $2 | $3 | $4)}
$6 -> {'file', list(integer())}
$7 -> {'location', {integer(), integer()}}
$8 -> {'location', {?, integer()}}
$9 -> {'file', list(integer())}
$10 -> {'location', {integer(), integer()}}
$11 -> {'location', {?, integer()}}
$12 -> {'atom', list($9 | $10 | $11), 'c' | 'cd'}
$13 -> {'atom', ?, 'bead'}
$14 -> {'location', {integer(), integer()}}
$15 -> {'atom', list($14), 'bead'}
$16 -> {'location', {?, integer()}}
$17 -> {'atom', list($16), 'bead'}
$18 -> {'file', list(integer())}
$19 -> {'location', {integer(), integer()}}
$20 -> {'location', {?, integer()}}
$21 -> {'atom', list($18 | $19 | $20), 'bead'}
$22 -> {'file', list(integer())}
$23 -> {'location', {integer(), integer()}}
$24 -> {'location', {?, integer()}}
$25 -> {'string', list($22 | $23 | $24), list(integer())}
$26 -> {'call', list($6 | $7 | $8), $12, list($13 | $15 | $17 | $21 | $25)}


$3 -> {'atom', {integer(), integer()}, 'bead' | 'c' | 'cd'}
$4 -> {'string', {integer(), integer()}, list(integer())}
$5 -> {'call', {integer(), integer()}, $3, list($3 | $4)}
$21 -> {'atom', list($22 | $24), 'bead' | 'c' | 'cd'}
$22 -> {'file', list(integer())}
$24 -> {'location', {integer(), integer()}}
$25 -> {'string', list($22 | $24), list(integer())}
$26 -> {'call', list($22 | $24), $21, list($21 | $25)}

Functions:
anno_set_file/2 -> fun(($5, list(integer())) -> $26)


Map.take 1 $ Map.drop 2


MkTyEnv (Map.take 1 $ Map.drop 13 (unTyEnv env))
--> a huge record

MkTyEnv (Map.take 1 $ Map.drop 14 (unTyEnv env))

--> 1000  aliases!!!!
merge problem???
compiler_options/1


 MkTyEnv (Map.take 1 $ Map.drop 15 (unTyEnv env))

 -->


 $0 -> {'file', list(integer())}
$1 -> {'location', {integer(), integer()}}
$2 -> {'location', {?, integer()}}



$0 -> {'function', ?, 'pany', ?, ?}
$1 -> {'atom', ?, 'worker'}
$2 -> {'call', ?, $1, list($5)}
$3 -> {'clause', ?, ?, ?, list($2 | $8)}
$4 -> {'clauses', list($3)}
$5 -> {'fun', ?, $4}
$7 -> {'lc', ?, $2, ?}
$8 -> {'match', ?, ?, $7}
$10 -> {'function', ?, 'pany', ?, list($3)}
$11 -> {'atom', ?, 'worker'}
$12 -> {'call', ?, $11, list($15)}
$13 -> {'clause', ?, ?, ?, list($12 | $18)}
$14 -> {'clauses', list($13)}
$15 -> {'fun', ?, $14}
$17 -> {'lc', ?, $12, ?}
$18 -> {'match', ?, ?, $17}
$20 -> {'function', ?, 'pany' | 'worker', ?, list($13)}
$23 -> {'atom', ?, 'self' | 'worker'}
$24 -> {'call', ?, $23, list($27)}
$25 -> {'clause', ?, ?, ?, list($24 | $30)}
$26 -> {'clauses', list($25)}
$27 -> {'fun', ?, $26}
$29 -> {'lc', ?, $24, ?}
$30 -> {'match', ?, ?, $24 | $29}
$32 -> {'function', ?, 'pany' | 'worker', ?, list($25)}
$48 -> {'case', ?, ?, list($88)}
$60 -> {'var', ?, 'L'}
$81 -> {'generate', ?, ?, $161}
$86 -> {'atom', ?, 'false' | 'self' | 'worker'}
$87 -> {'call', ?, $86, list($60 | $87 | $90 | $161)}
$88 -> {'clause', ?, list($86 | $161 | $162), ?, list(any() | $48 | $86 | $87 | $93 | $157 | $159 | $164)}
$89 -> {'clauses', list($88)}
$90 -> {'fun', ?, $89}
$92 -> {'lc', ?, $87, list($81)}
$93 -> {'match', ?, ?, $87 | $92}
$157 -> {'op', ?, '!', ?, $86}
$159 -> {'if', ?, list($88)}
$161 -> {'var', ?, 'L' | 'Res' | '_'}
$162 -> {'tuple', ?, list($161)}
$164 -> {'receive', ?, list($88)}
$166 -> {'function', ?, 'pany' | 'worker', ?, list($88)}
$182 -> {'case', ?, ?, list($212)}
$210 -> {'atom', ?, 'false' | 'self' | 'worker'}
$211 -> {'call', ?, $210, list($214 | $287)}
$212 -> {'clause', ?, list($210 | $287 | $288), ?, list(any() | $182 | $210 | $211 | $219 | $283 | $285 | $290)}
$213 -> {'clauses', list($212)}
$214 -> {'fun', ?, $213}
$217 -> {'generate', ?, ?, $287}
$218 -> {'lc', ?, $211, list($217)}
$219 -> {'match', ?, ?, $211 | $218}
$283 -> {'op', ?, '!', ?, $210}
$285 -> {'if', ?, list($212)}
$287 -> {'var', ?, 'L' | 'Res' | '_'}
$288 -> {'tuple', ?, list($287)}
$290 -> {'receive', ?, list($212)}
$292 -> {'function', ?, 'pany' | 'worker', ?, list($212)}
$308 -> {'case', ?, ?, list($321)}
$319 -> {'atom', ?, 'false' | 'self' | 'worker'}
$320 -> {'call', ?, $319, list($323 | $394)}
$321 -> {'clause', ?, list($319 | $394 | $395), ?, list(any() | $308 | $319 | $320 | $326 | $390 | $392 | $397)}
$322 -> {'clauses', list($321)}
$323 -> {'fun', ?, $322}
$325 -> {'lc', ?, $320, ?}
$326 -> {'match', ?, ?, $320 | $325}
$390 -> {'op', ?, '!', ?, $319}
$392 -> {'if', ?, list($321)}
$394 -> {'var', ?, 'L' | 'Res' | '_'}
$395 -> {'tuple', ?, list($394)}
$397 -> {'receive', ?, list($321)}
$399 -> {'function', ?, 'pany' | 'worker', ?, list($321)}
$424 -> {'atom', ?, 'false' | 'self' | 'worker'}
$425 -> {'call', ?, $424, list($428)}
$426 -> {'clause', ?, list($424 | $474), ?, list(any() | $425 | $481)}
$427 -> {'clauses', list($426)}
$428 -> {'fun', ?, $427}
$474 -> {'var', ?, 'Res' | '_'}
$481 -> {'op', ?, '!', ?, $424}
$487 -> {'function', ?, 'pany' | 'worker', ?, list($426)}
$497 -> {'var', ?, '_'}
$501 -> {'case', ?, ?, list($514)}
$512 -> {'atom', ?, 'false' | 'self' | 'worker'}
$513 -> {'call', ?, $512, list($516)}
$514 -> {'clause', ?, list($497 | $512), ?, list($501 | $512 | $513 | $519 | $554 | $556 | $558)}
$515 -> {'clauses', list($514)}
$516 -> {'fun', ?, $515}
$518 -> {'lc', ?, $513, ?}
$519 -> {'match', ?, ?, $513 | $518}
$554 -> {'op', ?, '!', ?, $512}
$556 -> {'if', ?, list($514)}
$558 -> {'receive', ?, list($514)}
$560 -> {'function', ?, 'pany' | 'worker', ?, list($514)}
$574 -> {'case', ?, ?, list($587)}
$585 -> {'atom', ?, 'false' | 'self' | 'worker'}
$586 -> {'call', ?, $585, list($589)}
$587 -> {'clause', ?, list($585 | $645 | $646), ?, list(any() | $574 | $585 | $586 | $592 | $641 | $643 | $648)}
$588 -> {'clauses', list($587)}
$589 -> {'fun', ?, $588}
$591 -> {'lc', ?, $586, ?}
$592 -> {'match', ?, ?, $586 | $591}
$641 -> {'op', ?, '!', ?, $585}
$643 -> {'if', ?, list($587)}
$645 -> {'var', ?, 'Res' | '_'}
$646 -> {'tuple', ?, list($645)}
$648 -> {'receive', ?, list($587)}
$650 -> {'function', ?, 'pany' | 'worker', ?, list($587)}
$655 -> {'var', ?, '_'}
$659 -> {'case', ?, ?, list($672)}
$670 -> {'atom', ?, 'false' | 'self' | 'worker'}
$671 -> {'call', ?, $670, list($674)}
$672 -> {'clause', ?, list($655 | $670), ?, list($659 | $670 | $671 | $677 | $699 | $701 | $703)}
$673 -> {'clauses', list($672)}
$674 -> {'fun', ?, $673}
$676 -> {'lc', ?, $671, ?}
$677 -> {'match', ?, ?, $671 | $676}
$699 -> {'op', ?, '!', ?, $670}
$701 -> {'if', ?, list($672)}
$703 -> {'receive', ?, list($672)}
$705 -> {'function', ?, 'pany' | 'worker', ?, list($672)}
$710 -> {'var', ?, '_'}
$714 -> {'case', ?, ?, list($727)}
$725 -> {'atom', ?, 'false' | 'self' | 'worker'}
$726 -> {'call', ?, $725, list($729)}
$727 -> {'clause', ?, list($710 | $725), ?, list($714 | $725 | $726 | $732 | $759 | $761 | $763)}
$728 -> {'clauses', list($727)}
$729 -> {'fun', ?, $728}
$731 -> {'lc', ?, $726, ?}
$732 -> {'match', ?, ?, $726 | $731}
$759 -> {'op', ?, '!', ?, $725}
$761 -> {'if', ?, list($727)}
$763 -> {'receive', ?, list($727)}
$765 -> {'function', ?, 'pany' | 'worker', ?, list($727)}
$769 -> {'case', ?, ?, list($782)}
$780 -> {'atom', ?, 'false' | 'self' | 'worker'}
$781 -> {'call', ?, $780, list($784)}
$782 -> {'clause', ?, list($780), ?, list($769 | $780 | $781 | $787 | $801 | $803 | $805)}
$783 -> {'clauses', list($782)}
$784 -> {'fun', ?, $783}
$786 -> {'lc', ?, $781, ?}
$787 -> {'match', ?, ?, $781 | $786}
$801 -> {'op', ?, ?, ?, $780}
$803 -> {'if', ?, list($782)}
$805 -> {'receive', ?, list($782)}
$807 -> {'function', ?, 'pany' | 'worker', ?, list($782)}
$808 -> {'var', ?, '_'}
$812 -> {'case', ?, ?, list($825)}
$823 -> {'atom', ?, 'false' | 'self' | 'worker'}
$824 -> {'call', ?, $823, list($827)}
$825 -> {'clause', ?, list($808 | $823), ?, list($812 | $823 | $824 | $830 | $844 | $846 | $848)}
$826 -> {'clauses', list($825)}
$827 -> {'fun', ?, $826}
$829 -> {'lc', ?, $824, ?}
$830 -> {'match', ?, ?, $824 | $829}
$844 -> {'op', ?, ?, ?, $823}
$846 -> {'if', ?, list($825)}
$848 -> {'receive', ?, list($825)}
$850 -> {'function', ?, 'pany' | 'worker', ?, list($825)}
$861 -> {'atom', ?, 'false' | 'self' | 'worker'}
$862 -> {'call', ?, $861, list($865)}
$863 -> {'clause', ?, ?, ?, list($861 | $862 | $868 | $871)}
$864 -> {'clauses', list($863)}
$865 -> {'fun', ?, $864}
$867 -> {'lc', ?, $862, ?}
$868 -> {'match', ?, ?, $862 | $867}
$871 -> {'receive', ?, list($863)}
$873 -> {'function', ?, 'pany' | 'worker', ?, list($863)}
$884 -> {'atom', ?, 'false' | 'self' | 'worker'}
$885 -> {'call', ?, $884, list($888)}
$886 -> {'clause', ?, ?, ?, list($884 | $885 | $891 | $894 | $896 | $898)}
$887 -> {'clauses', list($886)}
$888 -> {'fun', ?, $887}
$890 -> {'lc', ?, $885, ?}
$891 -> {'match', ?, ?, $885 | $890}
$894 -> {'op', ?, ?, ?, $884}
$896 -> {'if', ?, list($886)}
$898 -> {'receive', ?, list($886)}
$900 -> {'function', ?, 'pany' | 'worker', ?, list($886)}
$911 -> {'atom', ?, 'false' | 'self' | 'worker'}
$912 -> {'call', ?, $911, list($915)}
$913 -> {'clause', ?, list($911), ?, list($911 | $912 | $918 | $922 | $924 | $926)}
$914 -> {'clauses', list($913)}
$915 -> {'fun', ?, $914}
$917 -> {'lc', ?, $912, ?}
$918 -> {'match', ?, ?, $912 | $917}
$922 -> {'op', ?, ?, ?, $911}
$924 -> {'if', ?, list($913)}
$926 -> {'receive', ?, list($913)}
$928 -> {'function', ?, 'pany' | 'worker', ?, list($913)}
$945 -> {'case', ?, ?, list($1002)}
$957 -> {'var', ?, 'L'}
$1000 -> {'atom', ?, 'false' | 'self' | 'worker'}
$1001 -> {'call', ?, $1000, list($957 | $1001 | $1004 | $1076)}
$1002 -> {'clause', ?, list($1000 | $1076 | $1098), ?, list(any() | $945 | $1000 | $1001 | $1009 | $1093 | $1095 | $1100)}
$1003 -> {'clauses', list($1002)}
$1004 -> {'fun', ?, $1003}
$1009 -> {'match', ?, ?, $1001 | $1078}
$1076 -> {'var', ?, 'L' | 'Pids' | 'Res' | '_'}
$1077 -> {'generate', ?, ?, $1076}
$1078 -> {'lc', ?, $1001, list($1077)}
$1093 -> {'op', ?, '!', ?, $1000}
$1095 -> {'if', ?, list($1002)}
$1098 -> {'tuple', ?, list($1076)}
$1100 -> {'receive', ?, list($1002)}
$1102 -> {'function', ?, 'pany' | 'worker', ?, list($1002)}
$1119 -> {'case', ?, ?, list($1159)}
$1131 -> {'var', ?, 'L'}
$1152 -> {'generate', ?, ?, $1232}
$1157 -> {'atom', ?, 'false' | 'self' | 'worker'}
$1158 -> {'call', ?, $1157, list($1131 | $1158 | $1161 | $1232)}
$1159 -> {'clause', ?, list($1157 | $1232 | $1233), ?, list(any() | $1119 | $1157 | $1158 | $1164 | $1228 | $1230 | $1235)}
$1160 -> {'clauses', list($1159)}
$1161 -> {'fun', ?, $1160}
$1163 -> {'lc', ?, $1158, list($1152)}
$1164 -> {'match', ?, ?, $1158 | $1163}
$1228 -> {'op', ?, '!', ?, $1157}
$1230 -> {'if', ?, list($1159)}
$1232 -> {'var', ?, 'L' | 'Res' | '_'}
$1233 -> {'tuple', ?, list($1232)}
$1235 -> {'receive', ?, list($1159)}
$1237 -> {'function', ?, 'pany' | 'worker', ?, list($1159)}

Functions:
compiler_options/1 -> fun(list(any() | $0 | $10 | $20 | $32 | $166 | $292 | $399 | $487 | $560 | $650 | $705 | $765 | $807 | $850 | $873 | $900 | $928 | $1102 | $1237) -> list('export_all'))

why are the second elements missing??
why are there so many records that could me merged? are they not "local"?
1237 aliases --> ~400 aliases


should they get merged?:
$0 -> {'function', ?, 'pany', ?, ?}
$10 -> {'function', ?, 'pany', ?, list($3)}
compiler_options/1 -> fun(list(any() | $0 | $10...))
check the algo!
seems like merging is not done across unions, is this ok?
maybe try to wait out the "original" implementation to see how it does
the global section may be O(n^2), so the number of aliases should be as small as possible before it!

STRefs?
- check SPJ paper about higher rank type inference for pointers
Tobin-Hochstadt paper about high performance typechecking
the any()-s are from the flattend unions --> should there something be done about this?
combine could be simplified to use subtyping:
- if EUnknown is present, handle that in combine
- handle parameterized types in combine
- simple types, we take their least upper bound?
extend the type hierarchy so we have tuple(), and a lot of the any() will be tuple() instead


merge aliases: [2,7],resolved: [{'leaf', integer()},{'leaf', integer()} | {'node', $2, $5}], sigma: {'leaf', integer()} | {'node', $2, $5}



$0 -> {'leaf', integer()} 
$1 -> {'leaf', integer()} 
$2 -> {'leaf', integer()}
$3 -> {'leaf', integer()}
$4 -> {'leaf', integer()}
$5 -> {'node', $3, $4}
$6 -> {'node', $2, $5}
$7 -> {'leaf', integer()}
$8 -> {'leaf', integer()}
$9 -> {'leaf', integer()}
$10 -> {'node', $8, $9}
$11 -> {'leaf', integer()}
$12 -> {'node', ?, $11}
$13 -> {'node', $1 | $6, $7 | $10 | $12}



digraph G {

 // start -> a0;
 // start -> b0;
 // a1 -> b3;
 // b2 -> a3;
 // a3 -> a0;
 // a3 -> end;
 // b3 -> end;



 //n0 -> {'leaf', integer()} 
 //n1 -> {'leaf', integer()} 
 //n2 -> {'leaf', integer()}
 //n3 -> {'leaf', integer()}
 //n4 -> {'leaf', integer()}
 //n5 -> {'node', $3, $4}
 n5 -> n3;
 n5 -> n4;
 //n6 -> {'node', $2, $5}
 n6 -> n2;
 n6 -> n5;
 //n7 -> {'leaf', integer()}
 //n8 -> {'leaf', integer()}
 //n9 -> {'leaf', integer()}
 //n10 -> {'node', $8, $9}
 n10 -> n8;
 n10 -> n9;
 //n11 -> {'leaf', integer()}
 //n12 -> {'node', ?, $11}
 n10 -> n11;
 //n13 -> {'node', $1 | $6, $7 | $10 | $12}
  n13 -> n1;
  n13 -> n6;
  n13 -> n7;
  n13 -> n10;
  n13 -> n12;

  n13 [shape=Mdiamond];
//  end [shape=Msquare];
}


digraph G {

 // start -> a0;
 // start -> b0;
 // a1 -> b3;
 // b2 -> a3;
 // a3 -> a0;
 // a3 -> end;
 // b3 -> end;



 //n0 -> {'leaf', integer()} 
 //n1 -> {'leaf', integer()} 
 //n2 -> {'leaf', integer()}
 //n3 -> {'leaf', integer()}
 //n4 -> {'leaf', integer()}
 //n5 -> {'node', $3, $4}
 n5 -> l3;
 n5 -> l4;
 //n6 -> {'node', $2, $5}
 n6 -> l2;
 n6 -> n5;
 //n7 -> {'leaf', integer()}
 //n8 -> {'leaf', integer()}
 //n9 -> {'leaf', integer()}
 //n10 -> {'node', $8, $9}
 n10 -> l8;
 n10 -> l9;
 //n11 -> {'leaf', integer()}
 //n12 -> {'node', ?, $11}
 n10 -> l11;
 //n13 -> {'node', $1 | $6, $7 | $10 | $12}
  n13 -> l1;
  n13 -> n6;
  n13 -> l7;
  n13 -> n10;
  n13 -> n12;

  n13 [shape=Mdiamond];
//  end [shape=Msquare];
}



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

ALSO: there are still one element unions in some places