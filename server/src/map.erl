-module(map).
-export([
         new/0,
         to_string/1,

         distance/2,
         distance/4,
         in_eating_range/2,
         in_eating_range/3,
         player_move/2,
         update/3
        ]).

min_player_size() -> 30.
max_player_size() -> 200.
width() -> 1200.
height() -> 700.

new() ->
    {P1, P2} = new_players(),
    [new_map(), P1, P2].

to_string([Map, P1, P2]) ->
    [to_string(Map), player_to_string(P1), player_to_string(P2)];
to_string(Map) ->
    lists:join(" ", lists:map(fun(F) -> food_to_string(F) end, Map)).

new_map() ->
    [ new_food(I) || I <- lists:seq(0, 29) ].

new_players() ->
    W2 = floor(width() / 2),
    S = min_player_size(), % player size
    {
     [ rand:uniform(W2     - S) + S,
       rand:uniform(height() - S) + S,
       S
     ],
     [ rand:uniform(W2     - S) + S + W2,
       rand:uniform(height() - S) + S,
       S
     ]
    }.

new_food(I) ->
    [
     I, % Index
     rand:uniform(width() - 30) + 29,  % X
     rand:uniform(height() - 30) + 29, % Y
     rand:uniform(20) + 10,            % S
     rand:uniform(100) > 70            % Poison?
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

%absdiff(X, X)            -> 0;
%absdiff(X, Y) when X > Y -> X - Y;
%absdiff(X, Y) when X < Y -> Y - X.

%%% in_eating_range(A, B) -> is B in eating range of A ?

in_eating_range([Px, Py, Pr], [_, Fx, Fy, Fr, _]) -> % Player/Food
    in_eating_range({Pr, Fr, distance(Px, Py, Fx, Fy)});
in_eating_range(P1, P2) -> % Player/Player
    in_eating_range(P1, P2, distance(P1, P2)).

% Player/Player/Distance
in_eating_range([_, _, R1], [_, _, R2], Dist) ->
    in_eating_range({R1, R2, Dist}).

% Radius/Radius/Distance
in_eating_range({R1, R2, Dist}) ->
    Dist < ((R1 / 2) + (R2 / 2)).
%R1 > R2 andalso Dist < absdiff(R1, R2).

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

% Assume food is in range
player_eat([P1x, P1y, P1r], [_, _, P2r]) ->
    NP2x = rand:uniform(width() - min_player_size()) + min_player_size(),
    NP2y = rand:uniform(height() - min_player_size()) + min_player_size(),
    NewP1 = [ P1x, P1y, lists:min([P1r + ceil(P2r / 4), max_player_size()]) ],
    NewP2 = [ NP2x, NP2y, lists:max([P2r - ceil(P2r / 4), min_player_size()]) ],
    {NewP1, NewP2};
player_eat([_, _, _, Fr, true],  [Px, Py, Pr]) -> [Px, Py, lists:max([Pr - Fr, min_player_size()])];
player_eat([_, _, _, Fr, false], [Px, Py, Pr]) -> [Px, Py, lists:min([Pr + Fr, max_player_size()])].

player_eat_player([_, _, R]=P1, [_, _, R]=P2) ->
    {P1, P2};
player_eat_player([_, _, P1r]=P1, [_, _, P2r]=P2)
  when P1r > P2r ->
    case in_eating_range(P1, P2) of
        true -> player_eat(P1, P2);
        false -> {P1, P2}
    end;
player_eat_player([_, _, P1r]=P1, [_, _, P2r]=P2)
  when P2r > P1r ->
    case in_eating_range(P2, P1) of
        true ->
            {NewP2, NewP1} = player_eat(P2, P1),
            {NewP1, NewP2};
        false -> {P1, P2}
    end.

update(Map, [_, _, P1r]=P1, [_, _, P2r]=P2)
  when P1r >= P2r -> % Give the loosing player (P2) a hand
    {Eaten2, NotEaten2} = lists:partition(fun(F) -> in_eating_range(P2, F) end, Map),
    {Eaten1, NotEaten1} = lists:partition(fun(F) -> in_eating_range(P1, F) end, NotEaten2),
    AteP1 = lists:foldl(fun player_eat/2, P1, Eaten1),
    AteP2 = lists:foldl(fun player_eat/2, P2, Eaten2),
    EatenIdxs = [ I || [I|_] <- Eaten1 ++ Eaten2],
    NewFood = [ new_food(I) || I <- EatenIdxs ],
    NewMap = NewFood ++ NotEaten1,
    {NewP1, NewP2} = player_eat_player(AteP1, AteP2),
    {NewMap, NewP1, NewP2, NewFood};

update(Map, [_, _, P1r]=P1, [_, _, P2r]=P2)
  when P1r < P2r -> % Give the loosing player (P1) a hand
    {Eaten1, NotEaten1} = lists:partition(fun(F) -> in_eating_range(P1, F) end, Map),
    {Eaten2, NotEaten2} = lists:partition(fun(F) -> in_eating_range(P2, F) end, NotEaten1),
    AteP1 = lists:foldl(fun player_eat/2, P1, Eaten1),
    AteP2 = lists:foldl(fun player_eat/2, P2, Eaten2),
    EatenIdxs = [ I || [I|_] <- Eaten1 ++ Eaten2],
    NewFood = [ new_food(I) || I <- EatenIdxs ],
    NewMap = NewFood ++ NotEaten2,
    {NewP1, NewP2} = player_eat_player(AteP1, AteP2),
    {NewMap, NewP1, NewP2, NewFood}.
