-module(cl).
-export([
         enter_match/6,
         leave_match/2,
         new/1,
         stop/1
        ]).

%%%
%%% Public interface
%%%

%% @brief Tell a player inqueue that a match is starting
%% @param Player The player
%% @param Match The match
%% @param Map The map with all the edible objects
%% @param Pos The player's position
%% @param PosAdv The adversary's position
%% @param UnameAdv The adversary's username
%% @returns ok
enter_match(Player, Match, Map, Pos, PosAdv, UnameAdv) ->
    srv:cast(Player, {enter_match, Match, Map, Pos, PosAdv, UnameAdv}).

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
            io:format("Unexpected message: ~p\n", [Msg]),
            auth(Socket)
    end.

% TCP messages handler
handle_tcp_auth(Socket, <<"login ", UPb/binary>>) ->
    UP = binary_to_list(UPb),
    case string:tokens(UP, " ") of
        [Uname, Passwd] ->
            case lm:login(Uname, Passwd) of
                ok ->
                    mm:carne_pa_canhao({self(), Uname}),
                    gen_tcp:send(Socket, "ok\n"),
                    {fun waiting/1, {Socket, Uname}};
                invalid ->
                    gen_tcp:send(Socket, "invalid\n"),
                    {fun auth/1, Socket}
            end;
        _ ->
            gen_tcp:send(Socket, "badargs\n"),
            {fun auth/1, Socket}
    end;
handle_tcp_auth(Socket, <<"register ", UPb/binary>>) ->
    UP = binary_to_list(UPb),
    case string:tokens(UP, " ") of
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
    io:format("Unexpected TCP message: ~p\n", [Msg]),
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
        {tcp_closed, Socket} ->
            io:format("waiting: closed\n"),
            mm:leave_queue({self(), Uname});
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            waiting(St)
    end.

% Casts handler
handle_cast_waiting({Socket, Uname}, {enter_match, Match, Map, Pos, PosAdv, UnameAdv}) ->
    gen_tcp:send(Socket, [ "enter_match ", Pos, " ", PosAdv, " ", UnameAdv, " ", Map, "\n" ]),
    {fun ingame/1, {Socket, Uname, Match}};
handle_cast_waiting(St, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun waiting/1, St}.

%%%
%%% Ingame state
%%%

% Messages switch
ingame({Socket, _, Match}=St) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            handle_cast_ingame(St, Msg);
        {tcp, Socket, Msg} ->
            switch(handle_tcp_ingame(St, Msg));
        {tcp_closed, Socket} ->
            io:format("ingame: closed\n"),
            match:abort(Match, self());
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            ingame(St)
    end.

% TCP messages handler
handle_tcp_ingame(St, <<Up:8, Down:8, Left:8, Right:8, " ", Rest/binary>>) ->
    io:format("Got: ~p ~p ~p ~p ~p\n", [Up, Down, Left, Right, Rest]),
    {fun ingame/1, St};
handle_tcp_ingame(St, Msg) ->
    io:format("Unexpected TCP message: ~p\n", [Msg]),
    {fun ingame/1, St}.

% Casts handler
handle_cast_ingame({Socket, Uname, Match}, {leave_match, Match}) ->
    gen_tcp:send(Socket, "leave_match\n"),
    mm:carne_pa_canhao({self(), Uname}),
    {fun waiting/1, {Socket, Uname}};
handle_cast_ingame(St, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun ingame/1, St}.
