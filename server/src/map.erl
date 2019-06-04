-module(map).
-export([
         new/2,
         to_string/1
        ]).

new(Width, Height) ->
    {P1, P2} = new_players(Width, Height),
    [new_map(Width, Height), P1, P2].

to_string([Map, P1, P2]) ->
    [to_string(Map), player_to_string(P1), player_to_string(P2)];
to_string(Map) ->
    lists:join(" ", lists:map(fun(F) -> food_to_string(F) end, Map)).

new_map(Width, Height) ->
    [ new_food(I, Width, Height) || I <- lists:seq(0, 29) ].

new_players(Width, Height) ->
    W2 = floor(Width / 2),
    S = 15, % player size
    {
     [ rand:uniform(W2     - S) + S,
       rand:uniform(Height - S) + S,
       S
     ],
     [ rand:uniform(W2     - S) + S + W2,
       rand:uniform(Height - S) + S,
       S
     ]
    }.

% lists:map(fun(P) -> player_to_string(P) end, Ret).

new_food(I, Width, Height) ->
    [
     I,
     rand:uniform(Width - 30) + 29,  % X
     rand:uniform(Height - 30) + 29, % Y
     rand:uniform(20) + 10,          % S
     rand:uniform(100) > 70          % Poison?
    ].

food_to_string([I, X, Y, S, P]) ->
    [
     integer_to_list(I), ":",
     integer_to_list(X), ":",
     integer_to_list(Y), ":",
     integer_to_list(S), ":",
     atom_to_list(P)
    ].

player_to_string(P) ->
    lists:join(":", lists:map(fun integer_to_list/1, P)).
