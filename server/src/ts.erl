-module(ts).
-export([
         start/0,
         stop/0
        ]).

start() ->
    Pid = spawn(fun() -> ts([]) end),
    register(?MODULE, Pid),
    ok.

stop() ->
    ?MODULE ! stop.

ts(Scores) ->
    receive
        stop -> ok;
        _ -> ts(Scores)
    end.
