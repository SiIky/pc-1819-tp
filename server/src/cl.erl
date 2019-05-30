-module(cl).
-export([
         enter_match/2,
         leave_match/2,
         new/1,
         stop/1
        ]).

new(Socket) ->
    spawn(fun() -> auth(Socket) end).

switch({F, St}) ->
    F(St).

auth(Socket) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            switch(handle_cast_auth(Socket, Msg));
        {tcp, Socket, Msg} ->
            switch(handle_tcp_auth(Socket, Msg));
        {tcp_closed, Socket} ->
            lm:abort(self());
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            auth(Socket)
    end.

waiting(Socket) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            switch(handle_cast_waiting(Socket, Msg));
        {tcp_closed, Socket} ->
            mm:abort(self());
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            waiting(Socket)
    end.

ingame({Socket, Match}=St) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            handle_cast_ingame(St, Msg);
        {tcp_closed, Socket} ->
            match:abort(Match, self());
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            ingame(St)
    end.

handle_tcp_auth(Socket, <<"login:", _Rest/binary>>) ->
    Rest = binary_to_list(_Rest),
    case string:tokens(Rest, "\t") of
        [Uname, Passwd] ->
            case lm:login(Uname, Passwd) of
                ok ->
                    lm:abort(self()),
                    mm:carne_pa_canhao(self()),
                    gen_tcp:send(Socket, "ok\n"),
                    {fun waiting/1, Socket};
                invalid ->
                    gen_tcp:send(Socket, "invalid\n"),
                    {fun auth/1, Socket}
            end;
        _ ->
            gen_tcp:send(Socket, "badargs\n"),
            {fun auth/1, Socket}
    end;
handle_tcp_auth(Socket, <<"register:", _Rest/binary>>) ->
    Rest = binary_to_list(_Rest),
    case string:tokens(Rest, "\t") of
        [Uname, Passwd] ->
            case lm:create_account(Uname, Passwd) of
                ok ->
                    gen_tcp:send(Socket, "ok\n"),
                    {fun waiting/1, Socket};
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

handle_cast_auth(Socket, {auth, ok}) ->
    {fun waiting/1, Socket};
handle_cast_auth(Socket, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun auth/1, Socket}.

handle_cast_waiting(Socket, {enter_match, Match}) ->
    gen_tcp:send(Socket, "enter_match\n"),
    {fun ingame/1, {Socket, Match}};
handle_cast_waiting(Socket, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun waiting/1, Socket}.

handle_cast_ingame({Socket, Match}, {leave_match, Match}) ->
    gen_tcp:send(Socket, "leave_match\n"),
    {fun waiting/1, Socket};
handle_cast_ingame(St, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun ingame/1, St}.

leave_match(Player, Match) ->
    srv:cast(Player, {leave_match, Match}).

enter_match(Player, Match) ->
    srv:cast(Player, {enter_match, Match}).

stop(Player) ->
    srv:stop(Player).

close_conn(Socket) ->
    gen_tcp:close(Socket),
    ok.
