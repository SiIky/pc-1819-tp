-module(gs).
-behavior(application).
-export([
         start/2,
         stop/1
        ]).

start(normal, Args) ->
    ok = lm:start(),
    ok = acc:start(),
    ok = mm:start(),
    {ok, spawn(fun gs/0), Args}.

stop(State) ->
    io:format("Stopping... State: ~p\n", [State]),
    acc:stop(),
    lm:stop(),
    mm:stop(),
    ok.

gs() ->
    receive
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            gs()
    end.
