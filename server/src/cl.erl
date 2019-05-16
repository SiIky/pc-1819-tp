-module(cl).
-export([
         enter_match/2,
         new/1,
         stop/1
        ]).

new(Socket) ->
    spawn(fun() -> auth(Socket) end).

switch({auth, Socket}) ->
    auth(Socket);
switch({waiting_for_match, Socket}) ->
    waiting_for_match(Socket);
switch({ingame, Socket, Match}) ->
    ingame(Socket, Match).

auth(Socket) ->
    receive
        stop -> close_conn(Socket);
        {auth, ok} ->
            switch({waiting_for_match, Socket});
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg])
    end.

waiting_for_match(Socket) ->
    receive
        stop -> close_conn(Socket);
        {cast, {enter_match, Match}} ->
            switch({ingame, Socket, Match});
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg])
    end.

ingame(Socket, Match) ->
    receive
        stop -> close_conn(Socket);
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            switch({ingame, Socket, Match})
    end.

enter_match(Player, Match) ->
    srv:cast(Player, {enter_match, Match}).

stop(Player) ->
    Player ! stop.

close_conn(_Socket) ->
    % TODO: Tell the client the server is going down
    ok.
