-module(cl).
-export([
         auth/1,
         enter_match/2,
         ingame/2,
         stop/1,
         switch/1,
         waiting_for_match/1
        ]).

switch({auth, Socket}) ->
    auth(Socket);
switch({waiting_for_match, Socket}) ->
    waiting_for_match(Socket);
switch({ingame, Socket, Match}) ->
    ingame(Socket, Match).

auth(Socket) ->
    receive
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg])
    end.

waiting_for_match(Socket) ->
    receive
        {cast, {enter_match, Match}} ->
            switch({ingame, Socket, Match});
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg])
    end.

ingame(Socket, Match) ->
    ok.

enter_match(Player, Match) ->
    srv:cast(Player, {enter_match, Match}).

stop(Player) ->
    Player ! stop.
