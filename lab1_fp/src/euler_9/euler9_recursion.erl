-module(euler9_recursion).
-export([find_triplet/0]).

find_triplet() ->
  find_triplet(1, 2, 997).

find_triplet(A, B, C) ->
  case A + B + C of
    1000 ->
      case A * A + B * B =:= C * C of
        true -> A * B * C;
        false -> find_next(A, B, C)
      end;
    _ ->
      find_next(A, B, C)
  end.

find_next(A, B, C) ->
  case {B < C - 1, A < 999} of
    {true, _} ->
      find_triplet(A, B + 1, C - 1);
    {false, true} ->
      find_triplet(A + 1, A + 2, 1000 - (A + 1 + (A + 2)));
    {false, false} ->
      error
  end.
