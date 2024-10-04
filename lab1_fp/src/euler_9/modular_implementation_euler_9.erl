%%%-------------------------------------------------------------------
%%% @author arslanefimov

-module(modular_implementation_euler_9).
-author("arslanefimov").


-export([find_triplet/0]).


find_triplet() ->
  Triplets = generate_triplets(),
  PythagoreanTriplets = filter_pythagorean_triplets(Triplets),
  {A, B, C} = find_product(PythagoreanTriplets),
  io:format("Triplet found: A=~p, B=~p, C=~p, Product=~p~n", [A, B, C, A * B * C]),
  A * B * C.


generate_triplets() ->
  lists:foldl(fun(A, Acc_a) ->
                  lists:foldl(fun(B, Acc_b) ->
                      C = 1000 - A - B,
                      [{A, B, C} | Acc_b]
                    end, Acc_a, lists:seq(A + 1, 999))
    end, [], lists:seq(1, 998)).

filter_pythagorean_triplets(Triplets) ->
  lists:filter(fun({A, B, C}) -> A * A + B * B =:= C * C end, Triplets).

find_product([{A, B, C} | _]) ->
  {A, B, C}.