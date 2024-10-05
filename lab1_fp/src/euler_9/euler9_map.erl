-module(euler9_map).
-export([find_triplet/0]).

find_triplet() ->
    Triplets = generate_triplets(),
    PythagoreanTriplets = filter_pythagorean_triplets(Triplets),
    {A, B, C} = find_product(PythagoreanTriplets),
    A * B * C.

generate_triplets() ->
    lists:flatten(
        lists:map(
            fun(A) ->
                lists:map(
                    fun(B) ->
                        C = 1000 - A - B,
                        {A, B, C}
                    end,
                    lists:seq(A + 1, 999)
                )
            end,
            lists:seq(1, 498)
        )
    ).

filter_pythagorean_triplets(Triplets) ->
    lists:filter(fun({A, B, C}) -> A * A + B * B =:= C * C end, Triplets).

find_product([{A, B, C} | _]) ->
    {A, B, C}.
