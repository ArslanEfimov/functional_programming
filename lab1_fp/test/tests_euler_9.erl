%%%-------------------------------------------------------------------
%%% @author arslanefimov
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. сент. 2024 19:53
%%%-------------------------------------------------------------------
-module(tests_euler_9).
-author("arslanefimov").

-include_lib("eunit/include/eunit.hrl").


-define(assertEqual, macro_body).

euler9_recursion_test() ->
  ?assertEqual(31875000, euler9_recursion:find_triplet()).

euler9_tail_recursion_test() ->
  ?assertEqual(31875000, euler9_tail_recursion:find_triplet()).

euler9_modular_implementation_test() ->
  ?assertEqual(31875000, euler9_modular_implementation:find_triplet()).

euler9_map_test() ->
  ?assertEqual(31875000, euler9_map:find_triplet()).