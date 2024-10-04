%%%-------------------------------------------------------------------
%%% @author arslanefimov

-module(tail_recursion_euler_9).
-author("arslanefimov").


-export([find_triplet/0]).


find_triplet() ->
  find_triplet(1, 2, 997).

find_triplet(A, B, C) when A + B + C =:= 1000 ->
  case A * A + B * B =:= C * C of
    true ->
      io:format("Triplet found: A=~p, B=~p, C=~p, Product=~p~n", [A, B, C, A * B * C]),
      A * B * C;
    false ->
      find_next(A, B, C)

  end;

find_triplet(A, B, C) ->
  find_next(A, B, C).

find_next(A, B, C) when B < C - 1 ->
  find_triplet(A, B + 1, C -1);

find_next(A, _, _) when A < 999 ->
  find_triplet(A + 1, A + 2, 1000 - (A + 1 + (A + 2)));

find_next(_, _, _) ->
  io:format("No triplet found~n"),
  error.


