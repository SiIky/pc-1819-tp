-module(lm).
-export([
         % start/stopping
         start/0,
         stop/0,

         abort/1,
         create_account/2,
         login/2,
         logout/1,
         new_client/1
        ]).

start() ->
    Pid = spawn(fun() -> lm(init()) end), %% start new process
    register(?MODULE, Pid), %%register its name
    ok.

%% sends message to lm to stop the manager
stop() ->
    srv:stop(?MODULE).

%% preauth is a list of authenticating clients. this function tells them to stop
stop(Preauth) ->
    [ cl:stop(P) || P <- Preauth ], 
    ok.

%%create initial state
init() ->
    {dict:new(), [], sets:new()}.

%% handles whats going on with the logins. if it receives a stop message, it stops everyone in authenticating process.
%% if it receives a call, it will handle_call and the same for cast.
lm({_, Preauth, _}=St) ->
    receive
        stop ->
            stop(Preauth); 
        {call, From, Msg} ->
            lm(handle_call(St, From, Msg));
        {cast, Msg} ->
            lm(handle_cast(St, Msg))
    end.

%%handles login requests, if an element is a member of the list of online usernames, it replies to server that   
%% user is already logged in. If its not it replies OK, removes the element from preauth and adds it to the already online list.
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

%%handles the creation of accounts, if there's already an username in the map, it sends the message that the user already exists
%% if not, store the user in the map and remove it from the preauth list.
handle_call({UPs, Preauth, Online}=St, From, {create_account, Uname, Passwd}) ->
    case dict:is_key(Uname, UPs) of
        true ->
            srv:reply(From, user_exists),
            St;
        false ->
            srv:reply(From, ok),
            {dict:store(Uname, Passwd, UPs), Preauth -- [srv:from_pid(From)], Online}
    end;

%% wut    
handle_call(St, From, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    srv:reply(From, unexpected),
    St.

%%removes user from online list
handle_cast({UPs, Preauth, Online}, {logout, Uname}) ->
    {UPs, Preauth, sets:del_element(Uname, Online)};

%adds a new cliente to the preauth list.
handle_cast({UPs, Preauth, Online}, {new_client, C}) ->
    {UPs, [C|Preauth], Online};

%%aborts a client in the preauth process
handle_cast({UPs, Preauth, Online}, {abort, C}) ->
    {UPs, Preauth -- [C], Online};
%%wut
handle_cast(St, _Msg) ->
    St.

%%tells the server to abort a client in preauth
abort(C) ->
    srv:cast(?MODULE, {abort, C}).

%% tells the server there's a new client
new_client(C) ->
    srv:cast(?MODULE, {new_client, C}).

%%asks the server to create the account
create_account(Uname, Passwd) ->
    srv:recv(srv:call(?MODULE, {create_account, Uname, Passwd})).
%%asks the server to login
login(Uname, Passwd) ->
    srv:recv(srv:call(?MODULE, {login, Uname, Passwd})).
%%tells the server to logout.
logout(Uname) ->
    srv:cast(?MODULE, {logout, Uname}).
