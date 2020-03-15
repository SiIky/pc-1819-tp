-module(gs).
-behavior(application).
-export([
         start/2,
         stop/1
        ]).

start(normal, LPort) ->
    {ok, _LMPid} = lm:start(),
    ok = acc:start(LPort),
    {ok, _MMPid} = mm:start(),
    {ok, spawn(fun gs/0), LPort}.

stop(State) ->
    io:format("Stopping... State: ~p\n", [State]),
    acc:stop(),
    lm:stop(),
    mm:stop(),
    timer:sleep(1000),
    init:stop(),
    ok.

gs() ->
    receive
        Msg ->
            io:format("gs:gs:unexpected: ~p\n", [Msg]),
            gs()
    end.
