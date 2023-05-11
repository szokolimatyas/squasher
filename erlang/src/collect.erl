-module(collect).

-include("squasher.hrl").

-define(DEFAULT_FUEL, 100).
-define(TRACE_TAB, traces).

-export([prepare_trace/0, track/2, get_traces/0]).

-export([start_erlang_trace/1,
         stop_erlang_trace/1]).

-export([collect_loop/0]).

put_trace(T, P) -> 
    ets:insert(?TRACE_TAB, {T, P}).

get_traces() ->
    ets:tab2list(?TRACE_TAB).

prepare_trace() ->
    case ets:info(?TRACE_TAB) of
        undefined ->
            ok;
        _ ->
            ets:delete(?TRACE_TAB)
    end,
    ets:new(?TRACE_TAB, [bag, named_table, public]),
    ok.

start_erlang_trace(InModule) ->
    prepare_trace(),
    Pid = spawn_link(fun collect_loop/0),
    erlang:trace(all, true, [call, {tracer, Pid}]),
    erlang:trace_pattern({InModule, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
    Pid.

stop_erlang_trace(Pid) ->
    erlang:trace(all, false, [call]),
    Pid ! stop,
    Traces = get_traces(),
    file:write_file("out.bin", term_to_binary(Traces)).


collect_loop() -> 
    receive
        stop ->
            ok;
        {trace, _Pid, return_from, {_Module, Function, Arity}, ReturnValue} ->
            % Handle function call return message
            track(ReturnValue, [{rng, Arity}, {name, io_lib:print(Function), Arity}]),
            collect_loop();
         %   io:format("Function ~p:~p/~p returned ~p in process ~p~n", [Module, Function, Arity, ReturnValue, Pid]);
        {trace, _Pid, call, {_Module, Function, Arguments}} ->
            Arity = length(Arguments),
            % Handle a call trace message
            %io:format("Function ~p:~p invoked with arguments ~p in process ~p~n", [Module, Function, Arguments, Pid]);
            map_with_index(
                fun(V, I) ->
                    track(V, [{dom, I, Arity}, {name, io_lib:print(Function), Arity}]) 
                end, Arguments),
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
    track(V, P, ?DEFAULT_FUEL).

%%% Investigate if tail recursion is still ok.
%%% Remove trivial tracking scenarios (known at comptime).
%%% Do not track unchanging variables! 
%%% e.g in a replicate(What, N, Acc) function 'What' does not change
%%% optimize for list construction, list iteration!
%%% no need to re investigate the entire list tails every time 
%%% because that could potentially make an O(n) into O(n*n)! :(
%%% We can use proxies for some values if we know they don' leave
%%% the module boundaries (or the boundaries of the changed code)
%%% 
%%% inspiration from space efficient coercion passing -->
%%% trace passing?
%%% 
%%% user input for heuristics? (should I make this a record)
%%% 
%%% refactoring using the results -->
%%% spec functions, convert tups to records

track(V, P, _F) when is_integer(V) ->
    put_trace(integer, P),
    V;
track(V, P, _F) when is_float(V) ->
    put_trace(integer, P),
    V;
track(V, P, _F) when is_atom(V) ->
    put_trace({atom, V}, P),
    V;
track({}, P, _F) ->
    put_trace({tuple, []}, P),
    {};
track(Tup, P, F) when is_tuple(Tup) ->
    Vals = erlang:tuple_to_list(Tup),
    TupSize = erlang:tuple_size(Tup),
    Vals1 = 
        case Vals of
            [A | Rest] when is_atom(A) ->
                [A | map_with_index(
                        fun(V, I) -> 
                            track(V, [#tuple_index{key = {atom, A}, 
                                                   tuple_size = TupSize, index = I + 1} | P], F - 1)
                        end, Rest)];
            _ ->
                map_with_index(
                    fun(V, I) -> 
                        track(V, [#tuple_index{tuple_size = TupSize, index = I} | P], F - 1)
                    end, Vals)
        end,
    erlang:list_to_tuple(Vals1);
track(V, P, _F) when is_function(V) ->
    make_proxy(V, P);
track(V, P, _F) when is_binary(V) ->
    put_trace(binary, P),
    V;
track(V, P, _F) when is_bitstring(V) ->
    put_trace(binary, P),
    V;
%%% really naive implementation, but good for now?
%%% we need to limit some of the recursion!
track([], P, _) ->
    %% maybe a {list, none} or {list, ?}
    put_trace({list, unknown}, P),
    [];
track(Vals, P, F) when is_list(Vals) ->
    lists:map(
        fun(V) -> 
            track(V, [list_element | P], F - 1)
        end, Vals);
%%% really naive implementation, but good for now?
%%% we need to limit some of the recursion!
%%% ALSO: shapemaps?
track(#{}, P, _) ->
    put_trace({map, []}, P),
    #{};
track(Vals, P, F) when is_map(Vals) ->
    maps:map(
        fun(K, V) ->
            track(V, [{map_element, K} | P], F - 1) 
        end, Vals);
track(V, P, _F) when is_pid(V) ->
    put_trace(pid, P),
    V;
track(V, P, _F) when is_port(V) ->
    put_trace(port, P),
    V;
track(V, P, _F) when is_reference(V) ->
    put_trace(binary, P),
    V;
track(V, P, _F) ->
    put_trace(unknown, P),
    V.

map_with_index(F, L) ->
    map_with_index(F, L, 1).
map_with_index(F, [H | T], I) ->
    [F(H, I) | map_with_index(F, T, I + 1)];
map_with_index(_, [], _) ->
    [].

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