-module(match).
-export([
         abort/2,

         new/2,
         stop/1
        ]).

new(P1, P2) ->
    io:format("Starting a new match: ~p vs ~p\n", [P1, P2]),
    spawn(fun() -> match({P1, P2}) end).

match({P1, P2}=Ps) ->
    receive
        stop ->
            cl:stop(P1),
            cl:stop(P2);
        {cast, Msg} ->
            match(handle_cast(Ps, Msg));
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            match(Ps)
    end.

handle_cast(Ps, {abort, P}) ->
    io:format("Player ~p left\n", [P]),
    Ps;
handle_cast(Ps, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    Ps.

stop(Match) ->
    srv:stop(Match).

abort(Match, P) ->
    srv:cast(Match, {abort, P}).
