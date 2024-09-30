%%%-------------------------------------------------------------------
%%% @author arslanefimov
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. сент. 2024 19:53
%%%-------------------------------------------------------------------
-module(recursion_tests_euler_9).
-author("arslanefimov").

-include_lib("eunit/include/eunit.hrl").


-define(assertEqual, macro_body).

recursion_euler_9_test() ->
  ?assertEqual(31875000, recursion_euler_9:find_triplet()).

tail_recursion_euler_9_test() ->
  ?assertEqual(31875000, tail_recursion_euler_9:find_triplet()).

modular_implementation_euler_9_test() ->
  ?assertEqual(31875000, modular_implementation_euler_9:find_triplet()).

map_euler_9_test() ->
  ?assertEqual(31875000, map_euler_9:find_triplet()).