%%%-------------------------------------------------------------------
%%% @author arslanefimov

-module(tests_euler_22).
-author("arslanefimov").

-include_lib("eunit/include/eunit.hrl").

-define(assertEqual, macro_body).

euler22_recursion_test() ->
    ?assertEqual(871198282, euler22_recursion:find_final_score()).

euler22_tail_recursion_test() ->
    ?assertEqual(871198282, euler22_tail_recursion:find_final_score()).

euler22_modular_implementation_test() ->
    ?assertEqual(871198282, euler22_modular_implementation:find_final_score()).

euler22_map_test() ->
    ?assertEqual(871198282, euler22_map:find_final_score()).
