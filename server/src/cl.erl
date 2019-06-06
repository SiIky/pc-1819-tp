-module(cl).
-export([
         click/4,
         enter_match/6,
         leave_match/2,
         new/1,
         stop/1,
         updated_scores/2
        ]).

%%%
%%% Public interface
%%%

click(P, Map, Player, Adv) ->
    srv:cast(P, {click, [Player, " ", Adv, " ", Map, "\n"]}).

%% @brief Tell a player inqueue that a match is starting
%% @param Player The player
%% @param Match The match
%% @param Map The map with all the edible objects
%% @param Pos The player's position
%% @param PosAdv The adversary's position
%% @param AdvName The adversary's username
%% @returns ok
enter_match(Player, Match, Map, Pos, PosAdv, AdvName) ->
    srv:cast(Player, {enter_match, Match, Map, Pos, PosAdv, AdvName}).

%% @brief Tell a player ingame to leave a match
%% @param Player The player
%% @param Match The match
%% @returns ok
leave_match(Player, Match) ->
    srv:cast(Player, {leave_match, Match}).

%% @brief Start a new client, in the authentication state
%% @param Socket The TCP socket to communicate with the client
%% @returns The newly created client's PID
new(Socket) ->
    spawn(fun() -> auth(Socket) end).

%% @brief Tell a player that the server is going down
%% @param Player The player
%% returns ok
stop(Player) ->
    srv:stop(Player).

updated_scores(Player, Score) ->
    srv:cast(Player, {updated_scores, Score}).

%%%
%%% Private functions
%%%

close_conn(Socket) ->
    gen_tcp:close(Socket),
    ok.

% State switch
switch({F, St}) ->
    F(St).

%%%
%%% Authentication state
%%%

% Messages switch
auth(Socket) ->
    receive
        stop ->
            close_conn(Socket);
        {tcp, Socket, Msg} ->
            switch(handle_tcp_auth(Socket, Msg));
        {tcp_closed, Socket} ->
            lm:abort(self());
        Msg ->
            io:format("cl:auth:unexpected ~p\n", [Msg]),
            auth(Socket)
    end.

% TCP messages handler
handle_tcp_auth(Socket, <<"login ", UPb/binary>>) ->
    UP = binary_to_list(UPb),
    case string:tokens(UP, "\t") of
        [Uname, Passwd] ->
            case lm:login(Uname, Passwd) of
                ok ->
                    mm:carne_pa_canhao({self(), Uname}),
                    gen_tcp:send(Socket, "ok\n"),
                    {fun waiting/1, {Socket, Uname}};
                invalid ->
                    gen_tcp:send(Socket, "invalid\n"),
                    {fun auth/1, Socket};
                already_logged_in ->
                    gen_tcp:send(Socket, "already_logged_in\n"),
                    {fun auth/1, Socket}
            end;
        _ ->
            gen_tcp:send(Socket, "badargs\n"),
            {fun auth/1, Socket}
    end;
handle_tcp_auth(Socket, <<"register ", UPb/binary>>) ->
    UP = binary_to_list(UPb),
    case string:tokens(UP, "\t") of
        [Uname, Passwd] ->
            case lm:create_account(Uname, Passwd) of
                ok ->
                    gen_tcp:send(Socket, "ok\n"),
                    {fun waiting/1, {Socket, Uname}};
                user_exists ->
                    gen_tcp:send(Socket, "user_exists\n"),
                    {fun auth/1, Socket}
            end;
        _ ->
            gen_tcp:send(Socket, "badargs\n"),
            {fun auth/1, Socket}
    end;
handle_tcp_auth(Socket, Msg) ->
    io:format("cl:auth:tcp:unexpected ~p\n", [Msg]),
    {fun auth/1, Socket}.

%%%
%%% Waiting state
%%%

% Messages switch
waiting({Socket, Uname}=St) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            switch(handle_cast_waiting(St, Msg));
        {tcp, Socket, Msg} ->
            switch(handle_tcp_waiting(St, Msg));
        {tcp_closed, Socket} ->
            lm:logout(Uname),
            mm:leave_queue({self(), Uname});
        Msg ->
            io:format("cl:unexpected ~p\n", [Msg]),
            waiting(St)
    end.

handle_tcp_waiting(St, Msg) ->
    io:format("cl:waiting:tcp:unexpected ~p\n", [Msg]),
    {fun waiting/1, St}.

% Casts handler
handle_cast_waiting({_Socket, _}=St, {updated_scores, _Score}) ->
    % TODO: Send the client the updated scores
    %gen_tcp:send(Socket, ["updated_scores ", Score, "\n"])
    {fun waiting/1, St};
handle_cast_waiting({Socket, Uname}, {enter_match, Match, Map, Player, Adv, AdvName}) ->
    gen_tcp:send(Socket, [ "enter_match ", Player, " ", Adv, " ", AdvName, " ", Map, "\n" ]),
    {fun ingame/1, {Socket, Uname, Match}};
handle_cast_waiting(St, Msg) ->
    io:format("cl:waiting:cast:unexpected ~p\n", [Msg]),
    {fun waiting/1, St}.

%%%
%%% Ingame state
%%%

% Messages switch
ingame({Socket, Uname, Match}=St) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            switch(handle_cast_ingame(St, Msg));
        {tcp, Socket, Msg} ->
            switch(handle_tcp_ingame(St, Msg));
        {tcp_closed, Socket} ->
            lm:logout(Uname),
            match:abort(Match, self());
        Msg ->
            io:format("cl:ingame:unexpected ~p\n", [Msg]),
            ingame(St)
    end.

% TCP messages handler
% Key released
handle_tcp_ingame({_, _, Match}=St, <<"W\n">>) -> match:act(Match, self(), release_up),    {fun ingame/1, St};
handle_tcp_ingame({_, _, Match}=St, <<"S\n">>) -> match:act(Match, self(), release_down),  {fun ingame/1, St};
handle_tcp_ingame({_, _, Match}=St, <<"A\n">>) -> match:act(Match, self(), release_left),  {fun ingame/1, St};
handle_tcp_ingame({_, _, Match}=St, <<"D\n">>) -> match:act(Match, self(), release_right), {fun ingame/1, St};

% Key pressed
handle_tcp_ingame({_, _, Match}=St, <<"w\n">>) -> match:act(Match, self(), press_up),      {fun ingame/1, St};
handle_tcp_ingame({_, _, Match}=St, <<"s\n">>) -> match:act(Match, self(), press_down),    {fun ingame/1, St};
handle_tcp_ingame({_, _, Match}=St, <<"a\n">>) -> match:act(Match, self(), press_left),    {fun ingame/1, St};
handle_tcp_ingame({_, _, Match}=St, <<"d\n">>) -> match:act(Match, self(), press_right),   {fun ingame/1, St};

handle_tcp_ingame(St, Msg) ->
    io:format("cl:ingame:tcp:unexpected ~p\n", [Msg]),
    {fun ingame/1, St}.

% Casts handler
handle_cast_ingame({Socket, _, _}=St, {click, GS}) ->
    gen_tcp:send(Socket, GS),
    {fun ingame/1, St};
handle_cast_ingame({Socket, _, Match}=St, {leave_match, Match}) ->
    gen_tcp:send(Socket, "leave_match\n"),
    {fun endgame/1, St};
handle_cast_ingame(St, Msg) ->
    io:format("cl:ingame:cast:unexpected ~p\n", [Msg]),
    {fun ingame/1, St}.

%%%
%%% End game
%%%

endgame({Socket, _, Match}=St) ->
    receive
        {tcp, Socket, Msg} ->
            switch(handle_tcp_endgame(St, Msg));
        {tcp_closed, Socket} ->
            match:leave_endgame(Match, self()),
            ok;
        {cast, Msg} ->
            switch(handle_cast_endgame(St, Msg));
        Msg ->
            io:format("cl:endgame:unexpected ~p\n", [Msg])
    end.

handle_tcp_endgame({Socket, Uname, Match}, <<"enqueue\n">>) ->
    match:leave_endgame(Match, self()),
    mm:carne_pa_canhao({self(), Uname}),
    {fun waiting/1, {Socket, Uname}};
handle_tcp_endgame(St, Msg) ->
    io:format("cl:endgame:tcp:unexpected ~p\n", [Msg]),
    {fun endgame/1, St}.

handle_cast_endgame(St, Msg) ->
    io:format("cl:endgame:cast:unexpected ~p\n", [Msg]),
    {fun endgame/1, St}.
