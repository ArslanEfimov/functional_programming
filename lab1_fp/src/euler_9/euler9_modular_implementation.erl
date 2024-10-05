-module(euler9_modular_implementation).
-export([find_triplet/0]).


find_triplet() ->
  Triplets = generate_triplets(),
  PythagoreanTriplets = filter_pythagorean_triplets(Triplets),
  {A, B, C} = find_product(PythagoreanTriplets),
  A * B * C.


generate_triplets() ->
  lists:foldl(fun(A, Acc1) ->
                  lists:foldl(fun(B, Acc2) ->
                      C = 1000 - A - B,
                      [{A, B, C} | Acc2]
                    end, Acc1, lists:seq(A + 1, 999))
    end, [], lists:seq(1, 998)).

filter_pythagorean_triplets(Triplets) ->
  lists:filter(fun({A, B, C}) -> A * A + B * B =:= C * C end, Triplets).

find_product([{A, B, C} | _]) ->
  {A, B, C}.