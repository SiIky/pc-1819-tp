-module(match).
-export([
         abort/2,
         act/3,
         leave_endgame/2,
         updated_scores/2,

         new/2,
         stop/1
        ]).

%%%
%%% Match
%%%

new({P1, Name1}, {P2, Name2}) ->
    [_, _, _] = GS = map:new(),
    Timer = spawn(fun timer/0),
    Match = spawn(fun() -> match({P1, P2, GS, new_pcs(), Timer, Name1, Name2}) end),
    [SerMap, SerP1, SerP2] = map:to_string(GS),
    cl:enter_match(P1, Match, SerMap, SerP1, SerP2, Name2),
    cl:enter_match(P2, Match, SerMap, SerP2, SerP1, Name1),
    io:format("Starting a new match (~p): ~p vs ~p\n", [Match, P1, P2]),
    Timer ! {match, Match},
    Match.

match({player_left, {P1, P2, Timer, _, S2}, P1}) ->
    mm:match_over(S2),
    Timer ! stop,
    cl:leave_match(P2, self()),
    match({endgame, [ P1 ]});
match({player_left, {P1, P2, Timer, S1, _}, P2}) ->
    mm:match_over(S1),
    Timer ! stop,
    cl:leave_match(P1, self()),
    match({endgame, [ P2 ]});
match({times_up, {P1, P2}, S1, S2}) ->
    mm:match_over(S1, S2),
    cl:leave_match(P1, self()),
    cl:leave_match(P2, self()),
    match({endgame, [ P1, P2 ] });
match({endgame, []}) ->
    mm:leave_endgame(self()),
    ok;
match({endgame, Ps}=St) ->
    receive
        stop ->
            [ cl:stop(P) || P <- Ps ],
            ok;
        {cast, Msg} ->
            match(handle_cast(St, Msg))
    end;
match({P1, P2, _, _, Timer, _, _}=St) ->
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

handle_cast({endgame, Ps}, {leave_endgame, P}) ->
    {endgame, Ps -- [P]};
handle_cast({endgame, Ps}=St, {updated_scores, Score}) ->
    [ cl:updated_scores(P, Score) || P <- Ps ],
    St;
handle_cast({P1, P2, GS, PCs, Timer, Name1, Name2}, click) ->
    {NewMap, NewP1, NewP2, NewFood} = update(GS, PCs),
    % Send only the new food to the client
    [SerMap, SerP1, SerP2] = map:to_string([NewFood, NewP1, NewP2]),
    cl:click(P1, SerMap, SerP1, SerP2),
    cl:click(P2, SerMap, SerP2, SerP1),
    {P1, P2, [NewMap, NewP1, NewP2], PCs, Timer, Name1, Name2};
handle_cast({P1, P2, [_, [_, _, R1], [_, _, R2]], _, _, Name1, Name2}, times_up) ->
    {times_up, {P1, P2}, {R1, Name1}, {R2, Name2}};
handle_cast({P1, P2, [_, [_, _, R1], [_, _, R2]], _, Timer, Name1, Name2}, {abort, P}) ->
    io:format("Player ~p left\n", [P]),
    {player_left, {P1, P2, Timer, {R1, Name1}, {R2, Name2}}, P};
handle_cast({P1, P2, GS, {PC1, PC2}, Timer, Name1, Name2}, {act, Player, Action}) ->
    NewPC = case Player of
                P1 -> {update_pc(PC1, Action), PC2};
                P2 -> {PC1,                    update_pc(PC2, Action)}
            end,
    {P1, P2, GS, NewPC, Timer, Name1, Name2};
handle_cast(St, {updated_scores, _}) ->
    St;
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
            timer(Match, (2 * 60 * 1000) - 7000); % 2min
        Msg ->
            io:format("timer:unexpected ~p\n", [Msg])
    end.

timer(Match, Passed)
  when Passed =< 0 ->
    times_up(Match);
timer(Match, Passed) ->
    Int = 16, % 1000 / 60
    receive
        stop -> ok;
        Msg ->
            io:format("timer:unexpected ~p\n", [Msg])
    after
        Int ->
            click(Match),
            timer(Match, Passed - Int)
    end.

times_up(Match) -> srv:cast(Match, times_up).
click(Match) -> srv:cast(Match, click).

%%%
%%% Game State
%%%

act(Match, Player, Action) ->
    srv:cast(Match, {act, Player, Action}).

update([Map, P1, P2], {PC1, PC2}) ->
    map:update(Map, map:player_move(P1, PC1), map:player_move(P2, PC2)).

leave_endgame(Match, P) ->
    srv:cast(Match, {leave_endgame, P}).

updated_scores(Match, Score) ->
    srv:cast(Match, {updated_scores, Score}).

%%%
%%% Player controls
%%%
new_pcs() ->
    NewPC = {false, false, false, false},
    {NewPC, NewPC}.

update_pc({_,  Down, Left, Right}, press_up)      -> {true,  Down,  Left,  Right};
update_pc({Up, _,    Left, Right}, press_down)    -> {Up,    true,  Left,  Right};
update_pc({Up, Down, _,    Right}, press_left)    -> {Up,    Down,  true,  Right};
update_pc({Up, Down, Left, _},     press_right)   -> {Up,    Down,  Left,  true};
update_pc({_,  Down, Left, Right}, release_up)    -> {false, Down,  Left,  Right};
update_pc({Up, _,    Left, Right}, release_down)  -> {Up,    false, Left,  Right};
update_pc({Up, Down, _,    Right}, release_left)  -> {Up,    Down,  false, Right};
update_pc({Up, Down, Left, _},     release_right) -> {Up,    Down,  Left,  false}.
