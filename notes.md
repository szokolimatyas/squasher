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