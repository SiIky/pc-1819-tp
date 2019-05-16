-module(mm).
-export([
         carne_pa_canhao/1,

         start/0,
         stop/0
        ]).

start() ->
    Pid = spawn(fun() -> mm(init()) end),
    register(?MODULE, Pid),
    ok.

stop() ->
    ?MODULE ! stop.

init() ->
    {{}, []}.

mm({_, Matches}=State) ->
    receive
        stop ->
            [ match:stop(Match) || Match <- Matches ],
            ok;
        {call, {Pid, Ref}=From, Msg}
          when is_pid(Pid),
               is_reference(Ref) ->
            mm(handle_call(State, From, Msg));
        {cast, Msg} ->
            mm(handle_cast(State, Msg))
    end.

handle_call(State, From, _Msg) ->
    srv:reply(From, sup),
    State.

handle_cast({}, {carne_pa_canhao, Xixa}) ->
    {Xixa};
handle_cast({P1}, {carne_pa_canhao, Xixa}) ->
    Match = match:new(P1, Xixa),
    cl:enter_match(P1, Match),
    cl:enter_match(Xixa, Match),
    {};
handle_cast(State, _Msg) ->
    State.

carne_pa_canhao(Xixa) ->
    srv:cast(?MODULE, {carne_pa_canhao, Xixa}).
