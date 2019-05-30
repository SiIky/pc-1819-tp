-module(lm).
-export([
         % start/stopping
         start/0,
         stop/0,

         abort/1,
         clients_in_preauth/0,
         create_account/2,
         login/2,
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
        {call, {Pid, Ref}=From, Msg}
          when
              is_pid(Pid),
              is_reference(Ref) ->
            lm(handle_call(St, From, Msg));
        {cast, Msg} ->
            lm(handle_cast(St, Msg))
    end.

handle_call({_, Preauth}=St, From, clients_in_preauth) ->
    srv:reply(From, Preauth),
    St;
handle_call({UPs, _}=St, From, {login, Uname, Passwd}) ->
    case dict:find(Uname, UPs) of
        {ok, Passwd} ->
            srv:reply(From, ok),
            St;
        _ ->
            srv:reply(From, invalid),
            St
    end;
handle_call({UPs, Preauth}=St, From, {create_account, Uname, Passwd}) ->
    case dict:is_key(Uname, UPs) of
        true ->
            srv:reply(From, user_exists),
            St;
        false ->
            srv:reply(From, ok),
            {dict:store(Uname, Passwd, UPs), Preauth}
    end;
handle_call(St, From, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    srv:reply(From, unexpected),
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

clients_in_preauth() ->
    srv:recv(srv:call(?MODULE, clients_in_preauth)).

create_account(Uname, Passwd) ->
    srv:recv(srv:call(?MODULE, {create_account, Uname, Passwd})).

login(Uname, Passwd) ->
    srv:recv(srv:call(?MODULE, {login, Uname, Passwd})).
