%%%-------------------------------------------------------------------
%%% @author arslanefimov
%%% @copyright (C) 2024
%%% @doc
%%%
%%% @end
%%% Created : 29. сент. 2024 17:20
%%%-------------------------------------------------------------------
-module(map_euler_9).
-author("arslanefimov").

%% API
-export([find_triplet/0]).

find_triplet() ->
  Triplets = generate_triplets(),
  PythagoreanTriplets = filter_pythagorean_triplets(Triplets),
  {A, B, C} = find_product(PythagoreanTriplets),
  io:format("Product: ~p~n", [A * B * C]),
  A * B * C.

generate_triplets() ->
  lists:flatten(
    lists:map(fun(A) ->
      lists:map(fun(B) ->
        C = 1000 - A - B,
        {A, B, C}
                end, lists:seq(A+1, 999))
              end, lists:seq(1, 498))).

filter_pythagorean_triplets(Triplets) ->
  lists:filter(fun({A, B, C}) -> A * A + B * B =:= C * C end, Triplets).

find_product([{A, B, C} | _]) ->
  {A, B, C}.
