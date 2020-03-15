-module(mm).
-behavior(gen_server).
-export([
         handle_call/3,
         handle_cast/2,
         init/1,
         start/0,
         start_link/0,
         stop/0,
         terminate/2,

         % to be used by players
         leave_queue/1,
         carne_pa_canhao/1,

         % to be used by matches
         leave_endgame/1,
         match_over/1,
         match_over/2,

         updated_scores/1,
         state/0
        ]).

init([]) ->
    {ok, {[], []}}.

start_link() ->
    {ok, _TSPid} = ts:start_link(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    {ok, _TSPid} = ts:start(),
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

terminate(_Reason, {Ps, Matches}) ->
    ts:stop(),
    [ match:stop(Match) || Match <- Matches ],
    [ cl:stop(P) || {P, _} <- Ps ],
    ok.


make_match({[P2, P1 | Rest], Matches}) ->
    {Rest, [match:new(P1, P2)|Matches]};
make_match(State) ->
    State.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(Msg, _From, State) ->
    {reply, {badargs, Msg}, State}.

handle_cast({leave_endgame, Match}, {Ps, Matches}) ->
    {noreply, {Ps, Matches -- [Match]}};
handle_cast({updated_scores, Score}, {Ps, Matches}=St) ->
    [ cl:updated_scores(P, Score) || {P, _} <- Ps ],
    [ match:updated_scores(M, Score) || M <- Matches ],
    {noreply, St};
handle_cast({leave_queue, Xixa}, {Ps, Matches}) ->
    {noreply, {Ps -- [Xixa], Matches}};
handle_cast({carne_pa_canhao, Xixa}, {Ps, Matches}) ->
    {noreply, make_match({[Xixa|Ps], Matches})};
handle_cast({match_over, S1, S2}, {Ps, Matches}) ->
    ts:new_score(S1, S2),
    {noreply, {Ps, Matches}};
handle_cast({match_over, S}, {Ps, Matches}) ->
    ts:new_score(S),
    {noreply, {Ps, Matches}};
handle_cast(Msg, State) ->
    io:format("mm:handle_cast:unexpected: ~p\n", [Msg]),
    {noreply, State}.

leave_queue(Xixa) ->
    gen_server:cast(?MODULE, {leave_queue, Xixa}).

carne_pa_canhao(Xixa) ->
    gen_server:cast(?MODULE, {carne_pa_canhao, Xixa}).

match_over(S1, S2) ->
    gen_server:cast(?MODULE, {match_over, S1, S2}).

match_over(S) ->
    gen_server:cast(?MODULE, {match_over, S}).

updated_scores(Score) ->
    gen_server:cast(?MODULE, {updated_scores, Score}).

leave_endgame(Match) ->
    gen_server:cast(?MODULE, {leave_endgame, Match}).

state() ->
    gen_server:call(?MODULE, state).
