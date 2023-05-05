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

aliases:
#{"#leaf_0{}" => {tuple,[{atom,leaf},integer]},
  "#leaf_1{}" => {tuple,[{atom,leaf},integer]},
  "#leaf_3{}" => {tuple,[{atom,leaf},integer]},
  "#leaf_4{}" => {tuple,[{atom,leaf},integer]},
  "#leaf_5{}" => {tuple,[{atom,leaf},integer]},
  "#leaf_7{}" => {tuple,[{atom,leaf},integer]},
  "#node_2{}" => {tuple,[{atom,node},unknown,{alias,"#leaf_1{}"}]},
  "#node_6{}" => {tuple,[{atom,node},{alias,"#leaf_4{}"},{alias,"#leaf_5{}"}]},
  "#node_9{}" => {tuple,[{atom,node},{alias,"#leaf_3{}"},{alias,"t_8()"}]},
  "t_10()" =>
      {union,[{alias,"#leaf_0{}"},{alias,"#node_2{}"},{alias,"#node_9{}"}]},
  "t_8()" => {union,[{alias,"#leaf_7{}"},{alias,"#node_6{}"}]}}


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