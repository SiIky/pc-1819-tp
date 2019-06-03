-module(match).
-export([
         abort/2,

         new/2,
         stop/1
        ]).

new({P1, Name1}, {P2, Name2}) ->
    Width = 1200,
    Height = 700,
    Map = map:new(Width, Height),
    [Pos1, Pos2] = map:players(Width, Height),

    % integer_to_list/1
    % list_to_integer/1

    Match = spawn(fun() -> match({P1, P2}) end),
    cl:enter_match(P1, Match, Map, Pos1, Pos2, Name2),
    cl:enter_match(P2, Match, Map, Pos2, Pos1, Name1),
    io:format("Starting a new match (~p): ~p vs ~p\n", [Match, P1, P2]),
    Match.

match({player_left, {P1, P2}, P1}) ->
    cl:leave_match(P2, self()),
    mm:match_over(self());
match({player_left, {P1, P2}, P2}) ->
    cl:leave_match(P1, self()),
    mm:match_over(self());
match({P1, P2}=Ps) ->
    receive
        stop ->
            cl:stop(P1),
            cl:stop(P2);
        {cast, Msg} ->
            match(handle_cast(Ps, Msg));
        {call, From, Msg} ->
            match(handle_call(Ps, From, Msg));
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            match(Ps)
    end.

handle_call(Ps, From, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    srv:reply(From, badargs),
    Ps.

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
