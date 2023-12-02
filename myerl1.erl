-type num_add() :: {num, integer()} | {add, num_add(), num_add()}.

-spec eval(num_add()) -> integer().

-spec t1() -> num_add().

