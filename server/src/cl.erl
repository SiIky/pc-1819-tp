-module(cl).
-export([
         enter_match/2,
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
        {tcp_closed, Socket} ->
            io:format("Client closed TCP connection before authenticating\n"),
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
            io:format("Client closed TCP connection before authenticating\n"),
            mm:abort(self());
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            waiting(Socket)
    end.

ingame({Socket, _}=St) ->
    receive
        stop ->
            close_conn(Socket);
        {cast, Msg} ->
            handle_cast_ingame(St, Msg);
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            ingame(St)
    end.

enter_match(Player, Match) ->
    srv:cast(Player, {enter_match, Match}).

handle_cast_auth(Socket, {auth, ok}) ->
    {fun waiting/1, Socket};
handle_cast_auth(Socket, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun auth/1, Socket}.

handle_cast_waiting(Socket, {enter_match, Match}) ->
    {fun ingame/1, {Socket, Match}};
handle_cast_waiting(Socket, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun waiting/1, Socket}.

handle_cast_ingame(St, Msg) ->
    io:format("Unexpected message: ~p\n", [Msg]),
    {fun ingame/1, St}.

stop(Player) ->
    srv:stop(Player).

close_conn(Socket) ->
    gen_tcp:close(Socket),
    ok.
