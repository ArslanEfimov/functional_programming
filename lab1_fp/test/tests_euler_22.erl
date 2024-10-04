%%%-------------------------------------------------------------------
%%% @author arslanefimov

-module(tests_euler_22).
-author("arslanefimov").

-include_lib("eunit/include/eunit.hrl").

-define(assertEqual, macro_body).

recursion_euler_22_test() ->
    ?assertEqual(871198282, recursion_euler_22:main()).

tail_recursion_euler_22_test() ->
    ?assertEqual(871198282, tail_recursion_euler_22:main()).

modular_implementation_euler_22_test() ->
    ?assertEqual(871198282, modular_implementation_euler_22:main()).

map_euler_22_test() ->
    ?assertEqual(871198282, map_euler_22:main()).
