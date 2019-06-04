-module(match).
-export([
         abort/2,
         act/3,

         new/2,
         stop/1
        ]).

%%%
%%% Match
%%%

new({P1, Name1}, {P2, Name2}) ->
    Width = 1200,
    Height = 700,
    [_, _, _] = GS = map:new(Width, Height),
    Timer = spawn(fun timer/0),
    Match = spawn(fun() -> match({P1, P2, GS, Timer}) end),
    [SerMap, SerP1, SerP2] = map:to_string(GS),
    cl:enter_match(P1, Match, SerMap, SerP1, SerP2, Name2),
    cl:enter_match(P2, Match, SerMap, SerP2, SerP1, Name1),
    io:format("Starting a new match (~p): ~p vs ~p\n", [Match, P1, P2]),
    Timer ! {match, Match},
    Match.

match({player_left, {P1, P2, _, _}, P1}) ->
    cl:leave_match(P2, self()),
    mm:match_over(self());
match({player_left, {P1, P2, _, _}, P2}) ->
    cl:leave_match(P1, self()),
    mm:match_over(self());
match({times_up, {P1, P2, _, _}}) ->
    mm:match_over(self()),
    cl:leave_match(P1, self()),
    cl:leave_match(P2, self());
match({P1, P2, _, Timer}=St) ->
    receive
        stop ->
            Timer ! stop,
            cl:stop(P1),
            cl:stop(P2);
        {cast, Msg} ->
            match(handle_cast(St, Msg));
        {call, From, Msg} ->
            match(handle_call(St, From, Msg));
        Msg ->
            io:format("match:unexpected ~p\n", [Msg]),
            match(St)
    end.

handle_call(St, From, Msg) ->
    io:format("match:call:unexpected ~p\n", [Msg]),
    srv:reply(From, badargs),
    St.

handle_cast({P1, P2, GS, _}=St, click) ->
    [Map, Player1, Player2] = map:to_string(GS),
    cl:click(P1, Map, Player1, Player2),
    cl:click(P2, Map, Player2, Player1),
    St;
handle_cast(St, times_up) ->
    {times_up, St};
handle_cast(St, {abort, P}) ->
    io:format("Player ~p left\n", [P]),
    {player_left, St, P};
handle_cast({P1, P2, GS, Timer}, {act, Player, Action}) ->
    {P1, P2, update(GS, Player, Action), Timer};
handle_cast(St, Msg) ->
    io:format("match:cast:unexpected ~p\n", [Msg]),
    St.

stop(Match) ->
    srv:stop(Match).

abort(Match, P) ->
    srv:cast(Match, {abort, P}).

%%%
%%% Timer
%%%

timer() ->
    receive
        stop ->
            ok;
        {match, Match} ->
            timer(Match, 0);
        Msg ->
            io:format("timer:unexpected ~p\n", [Msg])
    end.

timer(Match, Passed)
  when Passed > 60000 -> % 60s
    times_up(Match);
timer(Match, Passed) ->
    Int = 1000 / 60, % 1s
    receive
        stop ->
            ok
    after
        Int ->
            click(Match),
            timer(Match, Passed + Int)
    end.

times_up(Match) -> srv:cast(Match, times_up).
click(Match) -> srv:cast(Match, click).

%%%
%%% Game State
%%%

act(Match, Player, Action) ->
    srv:cast(Match, {act, Player, Action}).

update([Map, P1, P2]=GS, Player, Action) ->
    GS.
