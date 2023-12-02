-module(tree).

-export([t1/0, eval/1]).

-compile({parse_transform, squasher_trans}).

t1() -> {add, {num, 2}, {num, 3}}.

eval({add, E1, E2}) -> eval(E1) + eval(E2);
eval({num, N}) -> N.