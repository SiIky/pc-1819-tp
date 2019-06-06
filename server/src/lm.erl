-module(lm).
-export([
         % start/stopping
         start/0,
         stop/0,

         abort/1,
         create_account/2,
         login/2,
         logout/1,
         new_client/1,
         online/0
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
    {dict:new(), [], sets:new()}.

lm({_, Preauth, _}=St) ->
    receive
        stop ->
            stop(Preauth);
        {call, From, Msg} ->
            lm(handle_call(St, From, Msg));
        {cast, Msg} ->
            lm(handle_cast(St, Msg))
    end.

handle_call({UPs, Preauth, Online}=St, From, {login, Uname, Passwd}) ->
    case dict:find(Uname, UPs) of
        {ok, Passwd} ->
            case sets:is_element(Uname, Online) of
                true ->
                    srv:reply(From, already_logged_in),
                    {UPs, Preauth, Online};
                false ->
                    srv:reply(From, ok),
                    {UPs, Preauth -- [srv:from_pid(From)], sets:add_element(Uname, Online)}
            end;
        _ ->
            srv:reply(From, invalid),
            St
    end;
handle_call({UPs, Preauth, Online}=St, From, {create_account, Uname, Passwd}) ->
    case dict:is_key(Uname, UPs) of
        true ->
            srv:reply(From, user_exists),
            St;
        false ->
            srv:reply(From, ok),
            {dict:store(Uname, Passwd, UPs), Preauth -- [srv:from_pid(From)], Online}
    end;
handle_call({_, _, Online}=St, From, online) ->
    srv:reply(From, Online),
    St;
handle_call(St, From, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    srv:reply(From, unexpected),
    St.

handle_cast({UPs, Preauth, Online}, {logout, Uname}) ->
    {UPs, Preauth, sets:del_element(Uname, Online)};
handle_cast({UPs, Preauth, Online}, {new_client, C}) ->
    {UPs, [C|Preauth], Online};
handle_cast({UPs, Preauth, Online}, {abort, C}) ->
    {UPs, Preauth -- [C], Online};
handle_cast(St, _Msg) ->
    St.

abort(C) ->
    srv:cast(?MODULE, {abort, C}).

new_client(C) ->
    srv:cast(?MODULE, {new_client, C}).

create_account(Uname, Passwd) ->
    srv:recv(srv:call(?MODULE, {create_account, Uname, Passwd})).

login(Uname, Passwd) ->
    srv:recv(srv:call(?MODULE, {login, Uname, Passwd})).

logout(Uname) ->
    srv:cast(?MODULE, {logout, Uname}).

online() ->
    srv:recv(srv:call(?MODULE, online)).
