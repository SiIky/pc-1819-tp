-module(map).
-export([
         new/2,
         players/2
        ]).

new_food(Width, Height) ->
    [
     rand:uniform(Width - 30) + 29,  % X
     rand:uniform(Height - 30) + 29, % Y
     rand:uniform(20) + 10,          % S
     rand:uniform(100) > 70          % Poison?
    ].

food_to_list([X, Y, S, P]) ->
    [
     integer_to_list(X), ":",
     integer_to_list(Y), ":",
     integer_to_list(S), ":",
     atom_to_list(P)
    ].

to_list(Map) ->
    lists:join(" ", lists:map(fun food_to_list/1, Map)).

new(Width, Height) ->
    to_list([ new_food(Width, Height) || _ <- lists:seq(1, 30) ]).

player_to_list(P) ->
    lists:join(":", lists:map(fun(N) -> integer_to_list(N) end, P)).

players(Width, Height) ->
    W2 = floor(Width / 2),
    S = 15,
    Ret = [ [ rand:uniform(W2     - S) + S,
              rand:uniform(Height - S) + S ],
            [ rand:uniform(W2     - S) + S + W2,
              rand:uniform(Height - S) + S ] ],
    lists:map(fun(P) -> player_to_list(P) end, Ret).
