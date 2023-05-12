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