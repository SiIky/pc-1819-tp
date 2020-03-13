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

-record(st, {
          ups=dict:new(),
          preauth=sets:new(),
          online=sets:new()
         }).

init([]) ->
    {ok, #st{}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

terminate(_Reason, St=#st{}) ->
    sets:fold(fun(P, Ret) -> cl:stop(P), Ret end, ok, St#st.preauth).

handle_call({login, Uname, Passwd}, From, St=#st{}) ->
    case dict:find(Uname, St#st.ups) of
        {ok, Passwd} ->
            case sets:is_element(Uname, St#st.online) of
                true ->
                    {reply, already_logged_in, St};
                false ->
                    {reply, ok, St#st{preauth=sets:del_element(srv:from_pid(From), St#st.preauth),
                                      online=sets:add_element(Uname, St#st.online)}}
            end;
        _ ->
            {reply, invalid, St}
    end;

handle_call({create_account, Uname, Passwd}, From, St=#st{}) ->
    case dict:is_key(Uname, St#st.ups) of
        true ->
            {reply, user_exists, St};
        false ->
            {reply, ok, St#st{ups=dict:store(Uname, Passwd, St#st.ups),
                              preauth=sets:del_element(srv:from_pid(From), St#st.preauth)}}
    end;
handle_call(online, _From, St=#st{}) ->
    {reply, St#st.online, St};

handle_call(Msg, _From, St=#st{}) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {reply, badargs, St}.

handle_cast({logout, Uname}, St=#st{}) ->
    {noreply, St#st{online=sets:del_element(Uname, St#st.online)}};

handle_cast({new_client, C}, St=#st{}) ->
    {noreply, St#st{preauth=sets:add_element(C, St#st.preauth)}};

handle_cast({abort, C}, St=#st{}) ->
    {noreply, St#st{preauth=sets:del_element(C, St#st.preauth)}};

handle_cast(Msg, St=#st{}) ->
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
