-module(map).
-export([
         new/2,
         to_string/1,

         distance/2,
         distance/4,
         in_eating_range/2,
         in_eating_range/3,
         player_move/2,
         update/3
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

% Player/Food
in_eating_range([Px, Py, Pr], [_, Fx, Fy, Fr, _]) ->
    in_eating_range({Pr, Fr, distance(Px, Py, Fx, Fy)});
% Player/Player
in_eating_range(P1, P2) ->
    Dist = distance(P1, P2),
    in_eating_range(P1, P2, Dist).

in_eating_range([_, _, R1], [_, _, R2], Dist) ->
    in_eating_range({R1, R2, Dist}).

in_eating_range({R1, R2, Dist}) ->
    Dist < ((R1 / 2) + (R2 / 2)).


distance([P1x, P1y, _], [P2x, P2y, _]) ->
    distance(P1x, P1y, P2x, P2y).

distance(X1, Y1, X2, Y2) ->
    P = X2 - X1,
    Q = Y2 - Y1,
    math:sqrt((P * P) + (Q * Q)).

speed(_) ->
    5.

bool2num(true) -> 1;
bool2num(false) -> 0.

player_move(P, {W, S, A, D}) ->
    player_move(P,
                (-1 * bool2num(A)) + bool2num(D),
                (-1 * bool2num(W)) + bool2num(S)).

player_move([X, Y, R], Dx, Dy) ->
    S = speed(R),
    [ X + Dx * S, Y + Dy * S, R].

update(Map, _, _) -> Map.

%update(Map, [_, _, P1r], [_, _, P2r])
%  when P1r >= P2r ->
%
%    .
