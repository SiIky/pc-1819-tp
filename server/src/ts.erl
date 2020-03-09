-module(ts).
-behavior(gen_server).
-export([
         handle_call/3,
         handle_cast/2,
         init/1,
         start/0,
         start_link/0,
         stop/0,

         new_score/1,
         new_score/2,
         top_score/0,

         scores_to_string/1
        ]).

init([]) ->
    {ok, []}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

handle_call(top_score, _From, Scores) ->
    {reply, Scores, Scores};
handle_call(Msg, _From, Scores) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {reply, {badargs, Msg}, Scores}.

handle_cast({new_score, S1, S2}, Scores) ->
    NewScores = take(5, insert(insert(Scores, S1), S2)),
    mm:updated_scores(scores_to_string(NewScores)),
    {noreply, NewScores};
handle_cast({new_score, S}, Scores) ->
    NewScores = take(5, insert(Scores, S)),
    mm:updated_scores(scores_to_string(NewScores)),
    {noreply, NewScores};
handle_cast(Msg, Scores) ->
    io:format("ts:cast:unexpected: ~p\n", [Msg]),
    {noreply, Scores}.

%%%

new_score(S1, S2) ->
    gen_server:cast(?MODULE, {new_score, S1, S2}).
new_score(S) ->
    gen_server:cast(?MODULE, {new_score, S}).

top_score() ->
    gen_server:call(?MODULE, top_score).

insert([], SP) ->
    [SP];
insert([H|T], SP) ->
    case H >= SP of
        true -> [H|insert(T, SP)];
        false -> [SP,H|T]
    end.

take(N, L) -> lists:sublist(L, N).

scores_to_string(Scores) ->
    lists:join(" ", lists:map(fun({S, N}) -> [ integer_to_list(S), ":", N ] end, Scores)).
