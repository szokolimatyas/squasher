
-type re_type() ::
    integer |
    float |
    {atom, atom()} |
    {tuple, [re_type()]} |
    any |
    {union, nonempty_list(re_type())} |
    {function, [re_type()], re_type()} |
    unknown |
    binary |
    bitstring |
    {list, re_type()} |
    {dict, re_type()} |
    {set, re_type()} |
    {gb_set, re_type()} |
    {gb_tree, re_type(), re_type()} |
    gb_empty |
    {array, re_type()} |
    {map, [{re_type(), re_type()}]} |
    pid |
    port |
    reference |
    boolean.

    
-record(tuple_index, {
    key = unknown :: unknown | re_type(),
    index :: integer(), 
    tuple_size :: integer()
}).

-type path() :: [path_part()].
-type path_part() :: 
    {name, string(), integer()} |
    {'dom', Actual :: integer(), Arity :: integer()} | 
    {'rng', Arity :: integer()} | 
    #tuple_index{} |
    list_element |
    dict_element |
    set_element |
    gb_set_element |
    {gb_tree_element, 1 | 2} |
    array_element |
    {map_element, re_type()}.