-module(hashmap_set_unit_tests).
-author("arslanefimov").

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/records.hrl").

-define(assertEqual, macro_body).
-define(assert, macro_body).

new_set_test() ->
    SetDefaultCapacity = hashmap_set:new(),
    SetWithUserCapacity = hashmap_set:new(5),
    ?assertEqual(16, SetDefaultCapacity#hash_set.capacity),
    ?assertEqual(5, SetWithUserCapacity#hash_set.capacity).

insert_element_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(3, Set0),
    Set2 = hashmap_set:insert(5, Set1),
    Set3 = hashmap_set:insert(8, Set2),
    ?assertEqual(3, Set3#hash_set.size),
    ?assertEqual(true, hashmap_set:has_key(Set3, 3)),
    ?assertEqual(true, hashmap_set:has_key(Set3, 5)).

has_no_identify_elements_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(5, Set0),
    Set2 = hashmap_set:insert(5, Set1),
    ?assertEqual(1, Set2#hash_set.size).

remove_element_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(9, Set0),
    Set2 = hashmap_set:insert(3, Set1),
    Set3 = hashmap_set:insert(1, Set2),
    Set4 = hashmap_set:remove(9, Set3),
    ?assertEqual(2, Set4#hash_set.size),
    ?assertEqual(false, hashmap_set:has_key(Set4, 9)).

filter_set_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(9, Set0),
    Set2 = hashmap_set:insert(8, Set1),
    Set3 = hashmap_set:insert(7, Set2),
    Set4 = hashmap_set:insert(10, Set3),
    Set5 = hashmap_set:filter(fun(X) -> X > 7 end, Set4),
    ?assertEqual(3, Set5#hash_set.size),
    ?assertEqual(false, hashmap_set:has_key(Set5, 7)).

foldl_set_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(3, Set0),
    Set2 = hashmap_set:insert(7, Set1),
    Set3 = hashmap_set:insert(22, Set2),
    Set4 = hashmap_set:insert(15, Set3),
    SumKeys = hashmap_set:foldl(fun(X, Acc) -> Acc + X end, 0, Set4),
    ?assertEqual(47, SumKeys).

foldr_set_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(3, Set0),
    Set2 = hashmap_set:insert(7, Set1),
    Set3 = hashmap_set:insert(22, Set2),
    Set4 = hashmap_set:insert(15, Set3),
    MulKeys = hashmap_set:foldr(fun(X, Acc) -> Acc * X end, 1, Set4),
    ?assertEqual(3 * 7 * 22 * 15, MulKeys).

map_set_test() ->
    Set0 = hashmap_set:new(),
    Set1 = hashmap_set:insert(3, Set0),
    Set2 = hashmap_set:insert(7, Set1),
    Set3 = hashmap_set:insert(22, Set2),
    Set4 = hashmap_set:insert(15, Set3),
    Set5 = hashmap_set:map(fun(X) -> X * X end, Set4),
    % 3^2 = 9
    ?assert(hashmap_set:has_key(Set5, 9)),
    % 7^2 = 49
    ?assert(hashmap_set:has_key(Set5, 49)),
    % 22^2 = 484
    ?assert(hashmap_set:has_key(Set5, 484)),
    % 15,2 = 225
    ?assert(hashmap_set:has_key(Set5, 225)).

has_a_neutral_element_test() ->
    NeutralElement = hashmap_set:new(),
    SetAdd = hashmap_set:insert(5, NeutralElement),
    ?assertEqual(SetAdd, hashmap_set:insert(5, NeutralElement)).

has_associativity_test() ->
    A = hashmap_set:insert(1, hashmap_set:new()),
    B = hashmap_set:insert(2, hashmap_set:new()),
    AB = hashmap_set:insert(2, A),
    ABC_SUM1 = hashmap_set:insert(3, AB),
    BC = hashmap_set:insert(3, B),
    ABC_SUM2 = hashmap_set:insert(1, BC),
    ?assertEqual(ABC_SUM1, ABC_SUM2).

union_set_test() ->
    Set1 = hashmap_set:new(100),
    Set2 = hashmap_set:new(100),

    Set1Updated = lists:foldl(
        fun(Elem, AccSet) ->
            hashmap_set:insert(Elem, AccSet)
        end,
        Set1,
        lists:seq(1, 500)
    ),

    Set2Updated = lists:foldl(
        fun(Elem, AccSet) ->
            hashmap_set:insert(Elem, AccSet)
        end,
        Set2,
        lists:seq(501, 1000)
    ),

    UnionSet = hashmap_set:union(Set1Updated, Set2Updated),

    ?assertEqual(1000, UnionSet#hash_set.size),

    lists:foreach(
        fun(Elem) ->
            ?assertEqual(true, hashmap_set:has_key(UnionSet, Elem))
        end,
        lists:seq(1, 1000)
    ),

    UnionWithSelf = hashmap_set:union(Set1Updated, Set1Updated),
    ?assertEqual(500, UnionWithSelf#hash_set.size).
