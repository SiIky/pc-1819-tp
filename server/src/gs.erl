-module(gs).
-export([
         % start/stopping
         start/0,
         stop/0
        ]).

start() ->
    ok = lm:start(),
    ok = acc:start(),
    ok = mm:start(),
    Pid = spawn(fun() -> gs() end),
    register(?MODULE, Pid),
    ok.

stop() ->
    ?MODULE ! stop,
    ok.

gs() ->
    receive
        stop ->
            acc:stop(),
            lm:stop(),
            mm:stop(),
            ok;
        Msg ->
            io:format("Unexpected message: ~p", [Msg]),
            gs()
    end.
