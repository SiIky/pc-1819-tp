-module(match).
-export([
         abort/2,

         new/2,
         stop/1
        ]).

new(P1, P2) ->
    Match = spawn(fun() -> match({P1, P2}) end),
    io:format("Starting a new match (~p): ~p vs ~p\n", [Match, P1, P2]),
    Match.

match({player_left, {P1, P2}, P1}) ->
    cl:leave_match(P2, self()),
    mm:match_over(player_left, P2);
match({player_left, {P1, P2}, P2}) ->
    cl:leave_match(P1, self()),
    mm:match_over(P1, player_left);
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
    {player_left, Ps, P};
handle_cast(Ps, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    Ps.

stop(Match) ->
    srv:stop(Match).

abort(Match, P) ->
    srv:cast(Match, {abort, P}).
