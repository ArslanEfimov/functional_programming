%%%-------------------------------------------------------------------
%%% @author arslanefimov

-module(recursion_euler_9).

-author("arslanefimov").

-export([find_triplet/0]).

find_triplet() ->
  find_triplet(1, 2, 997).

find_triplet(A, B, C) ->
  case A + B + C of
    1000 ->
      if A*A + B*B =:= C*C ->
        io:format("Triplet found: A=~p, B=~p, C=~p, Product=~p~n", [A, B, C, A * B * C]),
        A * B * C;
        true -> find_next(A, B, C)
      end;
    _ ->
      find_next(A, B, C)
  end.

find_next(A, B, C) ->
  if
    B < C - 1 -> find_triplet(A, B + 1, C - 1);
    A < 999 -> find_triplet(A + 1, A + 2, 1000 - (A + 1 + (A + 2)));
    true -> io:format("No triplet found~n"), error
  end.
