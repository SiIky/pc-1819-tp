-module(match).
-export([
         new/2,
         stop/1
        ]).

new(P1, P2) ->
    io:format("Starting a new match: ~p vs ~p\n", [P1, P2]),
    spawn(fun() -> match(P1, P2) end).

match(P1, P2) ->
    receive
        stop ->
            cl:stop(P1),
            cl:stop(P2);
        Msg ->
            io:format("Received message: ~p\n", [Msg])
    after 5000 ->
              match(P1, P2)
    end.

stop(Match) ->
    Match ! stop.
