-module(collect).

-include("squasher.hrl").

-define(DEFAULT_FUEL, 100).
-define(TRACE_TAB, traces).
-define(HORIZONTAL_FUEL, 100).

-export([prepare_trace/0, track/2, track/4, save_traces/0, save_traces/1, save_traces_/1, get_traces/0]).

-export([start_erlang_trace/1,
         stop_erlang_trace/1]).

-export([collect_loop/0]).

put_trace(T, P) -> 
	catch ets:insert(?TRACE_TAB, {T, P}).
get_traces() ->
    case global:whereis_name(squasher_table_owner) of
        Pid when is_pid(Pid) ->
            catch global:send(squasher_table_owner, stop),
            ets:tab2list(?TRACE_TAB);
        _ ->
            error
    end.

save_traces_(_) -> save_traces().

save_traces() -> save_traces("default.bin").
save_traces(FileName) ->
    case global:whereis_name(squasher_table_owner) of
        Pid when is_pid(Pid) ->
            Traces = ets:tab2list(?TRACE_TAB),
            catch global:send(squasher_table_owner, stop),
            NewTraces = 
                case file:read_file(FileName) of
                    {ok, Bin} ->
                        Traces ++ binary_to_term(Bin);
                    _ -> Traces
                end,
            file:write_file(FileName, term_to_binary(lists:uniq(NewTraces)));
        _ ->
            error
    end.

prepare_trace() ->
    Pid = spawn(fun() -> 
        case ets:info(?TRACE_TAB) of
            undefined ->
                ok;
            _ ->
                ets:delete(?TRACE_TAB)
        end,
        ets:new(?TRACE_TAB, [bag, named_table, public, {write_concurrency, auto}]),
        table_loop()
    end),
    global:register_name(squasher_table_owner, Pid),
    ok.

start_erlang_trace(InModule) ->
    prepare_trace(),
    Pid = spawn_link(fun collect_loop/0),
    erlang:trace(all, true, [call, {tracer, Pid}]),
    erlang:trace_pattern({InModule, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
    global:register_name(squasher_tracer, Pid),
	ok.

stop_erlang_trace(FileName) ->
    erlang:trace(all, false, [call]),
	catch global:send(squasher_tracer, stop),
    save_traces(FileName).

table_loop() ->
    receive
        stop -> ok;
        _ -> table_loop()
    end.

collect_loop() -> 
    receive
        stop ->
            ok;
        {trace, _Pid, return_from, {_Module, Function, Arity}, ReturnValue} ->
            case atom_to_list(Function) of
                [$-|_] -> ok;
                _ ->
                    track(ReturnValue, [{rng, Arity}, {name, io_lib:print(Function), Arity}])
            end,
            collect_loop();
        {trace, _Pid, call, {_Module, Function, Arguments}} when is_list(Arguments) ->
            Arity = length(Arguments),
            case atom_to_list(Function) of
                [$-|_] -> ok;
                _ ->
                    try map_with_index(
                        fun(V, I) ->
                            track(V, [{dom, I, Arity}, {name, io_lib:print(Function), Arity}]) 
                        end, Arguments)
                    catch 
                        A:B:C ->
                            io:format("error: ~p~n", [{A,B,C, Arguments}])
                    end
            end,
            collect_loop();
        _Other ->
            collect_loop()
    end.

%%% @doc Data collection is a side effect.
-spec track(Value, Path) -> Instrumented when
    Value :: term(),
    Path :: path(),
    Instrumented :: term().
track(V, P) ->
    track(V, P, ?DEFAULT_FUEL, ?HORIZONTAL_FUEL).

track(V, P, _F, _HF) when is_integer(V) ->
    put_trace(integer, P),
    V;
track(V, P, _F, _HF) when is_float(V) ->
    put_trace(integer, P),
    V;
track(V, P, _F, _HF) when is_atom(V) ->
    put_trace({atom, V}, P),
    V;
track({}, P, _F, _HF) ->
    put_trace({tuple, []}, P),
    {};
track({0, nil}, P, _F, _HF) ->
    put_trace(gb_empty, P),
    {0, nil};
track(Tup, P, F, HF) when is_tuple(Tup) ->
    B1 = array:is_array(Tup),
    B2 = sets:is_set(Tup),
    B3 = (element(1, Tup) == dict) andalso tuple_size(Tup) == 9,
    case {B1, B2, B3} of
        {true, _, _} ->
            put_trace({array, unknown}, P), Tup;
        {_, true, _} ->
            put_trace({set, unknown}, P), Tup;
        {_, _, true} ->
            put_trace({dict, unknown}, P), Tup;
        _ ->
            case is_gb_tree(Tup) of
                true ->
                    put_trace({gb_tree, unknown, unknown}, P), Tup;
                _ ->
                    case is_gb_set(Tup) of
                        true ->
                            put_trace({gb_set, unknown}, P), Tup;
                        _ -> 
                            track_raw_tuple(Tup, P, F, HF)
                    end
                end
    end;
track(V, P, _F, _HF) when is_function(V) ->
    make_proxy(V, P);
track(V, P, _F, _HF) when is_binary(V) ->
    put_trace(binary, P),
    V;
track(V, P, _F, _HF) when is_bitstring(V) ->
    put_trace(binary, P),
    V;
track([], P, _, _) ->
    %% maybe a {list, none} or {list, ?}
    put_trace({list, unknown}, P),
    [];
track(Vals, P, F, HF) when is_list(Vals) ->
    safe_map(
        fun(V) -> 
            track(V, [list_element | P], F - 1, HF)
        end, Vals, HF);
track(Map, P, _, _) when is_map(Map) ->
    put_trace({map, []}, P),
    Map;
track(V, P, _F, _HF) when is_pid(V) ->
    put_trace(pid, P),
    V;
track(V, P, _F, _HF) when is_port(V) ->
    put_trace(port, P),
    V;
track(V, P, _F, _HF) when is_reference(V) ->
    put_trace(binary, P),
    V;
track(V, P, _F, _HF) ->
    put_trace(unknown, P),
    V.

track_raw_tuple(Tup, P, F, HF) ->
    Vals = erlang:tuple_to_list(Tup),
    TupSize = erlang:tuple_size(Tup),
    Vals1 = 
        case Vals of
            [A | Rest] when is_atom(A) ->
                [A | map_with_index(
                        fun(V, I) -> 
                            track(V, [#tuple_index{key = {atom, A}, 
                                                tuple_size = TupSize, index = I + 1} | P], F - 1, HF)
                        end, Rest, 1, HF)];
            _ ->
                map_with_index(
                    fun(V, I) -> 
                        track(V, [#tuple_index{tuple_size = TupSize, index = I} | P], F - 1, HF)
                    end, Vals, 1, HF)
        end,
    erlang:list_to_tuple(Vals1).

map_with_index(F, L) ->
    map_with_index(F, L, 1, ?HORIZONTAL_FUEL).

map_with_index(F, [H | T], I, HFuel) when HFuel > 0 ->
    [F(H, I) | map_with_index(F, T, I + 1, HFuel - 1)];
map_with_index(_, Other, _, _) ->
    Other.

%%% safe_map(F, L) ->
%%%     safe_map(F, L, ?HORIZONTAL_FUEL).

safe_map(F, [H | T], I) when I > 0 ->
    [F(H) | safe_map(F, T, I - 1)];
safe_map(_, Other, _) ->
    Other.

is_gb_set({I, Tree}) when is_integer(I) ->
    is_gb_set_tree(Tree);
is_gb_set(_) -> false.

%%% tailrec?
is_gb_set_tree({_, nil, _}) -> true;
is_gb_set_tree({_, _, nil}) -> true;
is_gb_set_tree({_, T1, T2}) ->
    is_gb_set_tree(T1) orelse is_gb_set_tree(T2);
is_gb_set_tree(_) -> false.


is_gb_tree({I, Tree}) when is_integer(I) ->
    is_gb_tree_tree(Tree);
is_gb_tree(_) -> false.

%%% tailrec?
is_gb_tree_tree({_, nil, _}) -> true;
is_gb_tree_tree({_, _, nil}) -> true;
is_gb_tree_tree({_, _, T1, T2}) ->
    is_gb_tree_tree(T1) orelse is_gb_tree_tree(T2);
is_gb_tree_tree(_) -> false.

%%% do we need to wrap bifs?
%%% we have type info for them, and they should be cheap to use
%%% so we could just emit static information for them or something
make_proxy(F, P) ->
    {arity, A} = erlang:fun_info(F, arity),
    %% not proud of this one
    case A of
        0 ->
            fun() ->
                track(F(), [{rng, 0} | P])
            end;
        1 ->
            fun(A1) -> 
                track(F(track(A1, [{dom, 1, 1} | P])), [{rng, 1} | P]) 
            end;
        2 ->
            fun(A1, A2) -> 
                track(F(track(A1, [{dom, 1, 2} | P]), 
                        track(A2, [{dom, 2, 2} | P])), [{rng, 2} | P]) 
            end;
        3 ->
            fun(A1, A2, A3) -> 
                track(F(track(A1, [{dom, 1, 3} | P]), 
                        track(A2, [{dom, 2, 3} | P]),
                        track(A3, [{dom, 3, 3} | P])), [{rng, 3} | P]) 
            end;
        4 ->
            fun(A1, A2, A3, A4) -> 
                track(F(track(A1, [{dom, 1, 4} | P]), 
                        track(A2, [{dom, 2, 4} | P]),
                        track(A3, [{dom, 3, 4} | P]),
                        track(A4, [{dom, 4, 4} | P])), [{rng, 4} | P]) 
            end;
        5 ->
            fun(A1, A2, A3, A4, A5) -> 
                track(F(track(A1, [{dom, 1, 5} | P]), 
                        track(A2, [{dom, 2, 5} | P]),
                        track(A3, [{dom, 3, 5} | P]),
                        track(A4, [{dom, 4, 5} | P]),
                        track(A5, [{dom, 5, 5} | P])), [{rng, 5} | P]) 
            end;
        6 ->
            fun(A1, A2, A3, A4, A5, A6) -> 
                track(F(track(A1, [{dom, 1, 6} | P]), 
                        track(A2, [{dom, 2, 6} | P]),
                        track(A3, [{dom, 3, 6} | P]),
                        track(A4, [{dom, 4, 6} | P]),
                        track(A5, [{dom, 5, 6} | P]),
                        track(A6, [{dom, 6, 6} | P])), [{rng, 6} | P]) 
            end;
        7 ->
            fun(A1, A2, A3, A4, A5, A6, A7) -> 
                track(F(track(A1, [{dom, 1, 7} | P]), 
                        track(A2, [{dom, 2, 7} | P]),
                        track(A3, [{dom, 3, 7} | P]),
                        track(A4, [{dom, 4, 7} | P]),
                        track(A5, [{dom, 5, 7} | P]),
                        track(A6, [{dom, 6, 7} | P]),
                        track(A7, [{dom, 7, 7} | P])), [{rng, 7} | P])
            end;
        8 ->
            fun(A1, A2, A3, A4, A5, A6, A7, A8) -> 
                track(F(track(A1, [{dom, 1, 8} | P]), 
                        track(A2, [{dom, 2, 8} | P]),
                        track(A3, [{dom, 3, 8} | P]),
                        track(A4, [{dom, 4, 8} | P]),
                        track(A5, [{dom, 5, 8} | P]),
                        track(A6, [{dom, 6, 8} | P]),
                        track(A7, [{dom, 7, 8} | P]),
                        track(A8, [{dom, 8, 8} | P])), [{rng, 8} | P])
            end;
        _ ->
            F
    end.