-module(tree).

-export([t1/0, node_num/1]).

t1() -> {node, {node, {leaf, 1}, {node, {leaf, 2}, {leaf, 3}}}, {leaf, 4}}.

node_num({node, L, R}) ->
    1 + node_num(L) + node_num(R);
node_num(_) -> 1.