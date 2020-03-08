-module(mm).
-export([
         % to be used by players (?)
         leave_queue/1,
         carne_pa_canhao/1,

         % to be used by matches
         leave_endgame/1,
         match_over/1,
         match_over/2,

         updated_scores/1,
         state/0,

         start/0,
         stop/0
        ]).
%% starts the topscore and the mm process
start() ->
    ok = ts:start(),
    Pid = spawn(fun() -> mm(init()) end),
    register(?MODULE, Pid),
    ok.
%%stops the mm process
stop() ->
    srv:stop(?MODULE).
%%stops the topscore and the players in queue
stop(Ps) ->
    ts:stop(),
    [ cl:stop(P) || {P, _} <- Ps ],
    ok.

init() ->
    {[], []}.
%%starts a new match between two players in queue
mm({[P2, P1 | Rest], Matches}) ->
    Match = match:new(P1, P2),
    mm({Rest, [Match|Matches]});
%% If it receives a stop message stops the matches and the players in queue
%% otherwise it handles the calls and casts
mm({Ps, Matches}=State) ->
    receive
        stop ->
            [ match:stop(Match) || Match <- Matches ],
            stop(Ps),
            ok;
        {call, {Pid, Ref}=From, Msg}
          when is_pid(Pid),
               is_reference(Ref) ->
            mm(handle_call(State, From, Msg));
        {cast, Msg} ->
            mm(handle_cast(State, Msg));
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg])
    end.
%%replies with the sate to whoever asks
handle_call(State, From, state) ->
    srv:reply(From, State),
    State;
%%otherwise ignores
handle_call(State, From, _Msg) ->
    srv:reply(From, badargs),
    State.
%%if match tells it to leave endgame screen, match is removed from list.
handle_cast({Ps, Matches}, {leave_endgame, Match}) ->
    {Ps, Matches -- [Match]};
%%at the end of the match, match sends to mm player scores and mm sends to topscores the scores for it to update.
%% once topscores update, sends mm with the updated score.
handle_cast({Ps, Matches}=St, {updated_scores, Score}) ->
    [ cl:updated_scores(P, Score) || {P, _} <- Ps ],
    [ match:updated_scores(M, Score) || M <- Matches ],
    St;
%%if  a player left queue, mm removes it from the player list.
handle_cast({Ps, Matches}, {leave_queue, Xixa}) ->
    {Ps -- [Xixa], Matches};
%%player enters the queue
handle_cast({Ps, Matches}, {carne_pa_canhao, Xixa}) ->
    {[Xixa|Ps], Matches};
%% once the match is over, scores from both players are updated to topscores
handle_cast({Ps, Matches}, {match_over, S1, S2}) ->
    ts:new_score(S1, S2),
    {Ps, Matches};
%%If onle player leaves during the match, the player that stayed has its score updated.
handle_cast({Ps, Matches}, {match_over, S}) ->
    ts:new_score(S),
    {Ps, Matches};
handle_cast(State, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    State.
%%player leaves a queue
leave_queue(Xixa) ->
    srv:cast(?MODULE, {leave_queue, Xixa}).
%player enters a queue
carne_pa_canhao(Xixa) ->
    srv:cast(?MODULE, {carne_pa_canhao, Xixa}).
%%match is over, both scores update
match_over(S1, S2) ->
    srv:cast(?MODULE, {match_over, S1, S2}).
%%match is over because one player left, one score updates
match_over(S) ->
    srv:cast(?MODULE, {match_over, S}).
%%update topscores
updated_scores(Score) ->
    srv:cast(?MODULE, {updated_scores, Score}).
%%leave endgame screen
leave_endgame(Match) ->
    srv:cast(?MODULE, {leave_endgame, Match}).
%%state
state() ->
    srv:recv(srv:call(?MODULE, state)).
