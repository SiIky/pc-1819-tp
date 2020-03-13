-module(lm).
-behavior(gen_server).
-export([
         handle_call/3,
         handle_cast/2,
         init/1,
         start/0,
         start_link/0,
         stop/0,
         terminate/2,

         abort/1,
         create_account/2,
         login/2,
         logout/1,
         new_client/1,
         online/0
        ]).

init([]) ->
    {ok, {dict:new(), [], sets:new()}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

terminate(_Reason, {_, Preauth, _}) ->
    [ cl:stop(P) || P <- Preauth ],
    ok.

handle_call({login, Uname, Passwd}, From, {UPs, Preauth, Online}=St) ->
    case dict:find(Uname, UPs) of
        {ok, Passwd} ->
            case sets:is_element(Uname, Online) of
                true ->
                    {reply, already_logged_in, {UPs, Preauth, Online}};
                false ->
                    {reply, ok, {UPs, Preauth -- [srv:from_pid(From)], sets:add_element(Uname, Online)}}
            end;
        _ ->
            {reply, invalid, St}
    end;

handle_call({create_account, Uname, Passwd}, From, {UPs, Preauth, Online}=St) ->
    case dict:is_key(Uname, UPs) of
        true ->
            {reply, user_exists, St};
        false ->
            {reply, ok, {dict:store(Uname, Passwd, UPs), Preauth -- [srv:from_pid(From)], Online}}
    end;
handle_call(online, _From, {_, _, Online}=St) ->
    {reply, Online, St};

handle_call(Msg, _From, St) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {reply, badargs, St}.

handle_cast({logout, Uname}, {UPs, Preauth, Online}) ->
    {noreply, {UPs, Preauth, sets:del_element(Uname, Online)}};

handle_cast({new_client, C}, {UPs, Preauth, Online}) ->
    {noreply, {UPs, [C|Preauth], Online}};

handle_cast({abort, C}, {UPs, Preauth, Online}) ->
    {noreply, {UPs, Preauth -- [C], Online}};

handle_cast(Msg, St) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {noreply, St}.

abort(C) ->
    gen_server:cast(?MODULE, {abort, C}).

new_client(C) ->
    gen_server:cast(?MODULE, {new_client, C}).

create_account(Uname, Passwd) ->
    gen_server:call(?MODULE, {create_account, Uname, Passwd}).

login(Uname, Passwd) ->
    gen_server:call(?MODULE, {login, Uname, Passwd}).

logout(Uname) ->
    gen_server:cast(?MODULE, {logout, Uname}).

online() ->
    gen_server:call(?MODULE, online).
