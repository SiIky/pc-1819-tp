-module(ts).
-export([
         insert_score/1,
         top_score/0,

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
        {call, From, Msg} ->
            ts(handle_call(Scores, From, Msg));
        {cast, Msg} ->
            ts(handle_cast(Scores, Msg));
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            ts(Scores)
    end.

handle_call([H|_]=Scores, From, top_score) ->
    srv:reply(From, H),
    Scores;
handle_call([]=Scores, From, top_score) ->
    srv:reply(From, no_top_score),
    Scores;
handle_call(Scores, From, Msg) ->
    srv:reply(From, unexpected),
    io:format("Unexpected message: ~p\n", [Msg]),
    Scores.

handle_cast(Scores, {insert_score, SP}) ->
    take(5, insert(Scores, SP));
handle_cast(Scores, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    Scores.

insert_score(SP) ->
    srv:cast(?MODULE, {insert_score, SP}).

top_score() ->
    srv:call(?MODULE, top_score).

insert([], SP) ->
    [SP];
insert([H|T], SP) ->
    case H > SP of
        true -> [H|insert(T, SP)];
        false -> [SP,H|T]
    end.

take(N, L) -> lists:sublist(L, N).
