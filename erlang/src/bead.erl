-module(bead).
-compile(export_all).
-compile({parse_transform, squasher_trans}).

worker(Pid,F,E) ->
  %  io:format("Worker starts~n"),
  %  timer:sleep(rand:uniform(5)*1000),
    case F(E) of
        true ->
            Pid ! {true,E};
        _   ->
            Pid ! false
    end.


collector(MainPid,FinishedNum,MaxNum)->
    receive
        {true,E} ->
         %   io:format("Found after: ~p~n",[FinishedNum+1]),
            MainPid ! {true,E};
        false ->
            if 
                FinishedNum >= MaxNum -> 
                    MainPid ! false;
                true -> 
                    collector(MainPid,FinishedNum + 1,MaxNum)
            end
    end.


    
pany(F,L) ->
%    rand:seed_s(exsss),
    MyPid = self(),
    Collector = spawn(fun() -> collector(MyPid,0,lists:flatlength(L)) end),
    Pids = [spawn(fun() -> worker(Collector,F,E) end) || E <- L],
    Collector,
    receive
        false ->
            false;
        {true,Res} ->
            [ exit(P,normal) || P <- Pids],  % not very nice I know
            {true,Res}
    end.

    %receive
    %   ->
test(L) ->
    lists:sort(L).