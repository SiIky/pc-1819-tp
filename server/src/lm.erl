-module(lm).
-export([
         % start/stopping
         start/0,
         stop/0,

         abort/1,
         new_client/1
        ]).

start() ->
    Pid = spawn(fun() -> lm(init()) end),
    register(?MODULE, Pid),
    ok.

stop() ->
    srv:stop(?MODULE).

stop(Preauth) ->
    [ cl:stop(P) || P <- Preauth ],
    ok.

init() ->
    {dict:new(), []}.

lm({_, Preauth}=St) ->
    receive
        stop ->
            stop(Preauth),
            ok;
        merdas ->
            io:format("Preauth: ~p\n", [Preauth]);
        {call, {Pid, Ref}=From, Msg}
          when
              is_pid(Pid),
              is_reference(Ref) ->
            lm(handle_call(St, From, Msg));
        {cast, Msg} ->
            lm(handle_cast(St, Msg))
    end.

handle_call(St, From, _Msg) ->
    srv:reply(From, sup),
    St.

handle_cast({UPs, Preauth}, {new_client, C}) ->
    {UPs, [C|Preauth]};
handle_cast({UPs, Preauth}, {abort, C}) ->
    {UPs, Preauth -- [C]};
handle_cast(St, _Msg) ->
    St.

abort(C) ->
    srv:cast(?MODULE, {abort, C}).

new_client(C) ->
    srv:cast(?MODULE, {new_client, C}).
