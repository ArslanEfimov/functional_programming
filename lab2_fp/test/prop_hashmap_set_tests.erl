-module(prop_hashmap_set_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("../src/records.hrl").
-define(FORALL, macro_body).

%% Проверка ассоциативности объединения
prop_associativity_test() ->
  ?FORALL(
    {Keys1, Keys2, Keys3},
    {list(int()), list(int()), list(int())},
    begin
      Set1 = hashmap_set:new(),
      FilledSet1 = fill_set(Set1, Keys1),
      Set2 = hashmap_set:new(),
      FilledSet2 = fill_set(Set2, Keys2),
      Set3 = hashmap_set:new(),
      FilledSet3 = fill_set(Set3, Keys3),

      Union1 = hashmap_set:union(
        hashmap_set:union(FilledSet1, FilledSet2), FilledSet3
      ),
      Union2 = hashmap_set:union(
        hashmap_set:union(FilledSet2, FilledSet3), FilledSet1
      ),
      is_equal(Union1, Union2) == true
    end
  ).

%% Проверка нейтрального элемента
prop_neutral_element_test() ->
  ?FORALL(
    {Key},
    {int()},
    begin
      Empty = hashmap_set:new(), % нейтральный элемент
      Set = hashmap_set:new(),
      FilledSet = hashmap_set:insert(Key, Set),

      Union1 = hashmap_set:union(Empty, FilledSet),
      Union2 = hashmap_set:union(FilledSet, Empty),
      is_equal(Union1, Union2),
      is_equal(Union1, FilledSet)
    end
  ).

%% Проверка удаления элемента
prop_remove_test() ->
  ?FORALL(
    {Key},
    {int()},
    begin
      Set = hashmap_set:new(),
      UpdatedSet = hashmap_set:insert(Key, Set),
      RemovedSet = hashmap_set:remove(Key, UpdatedSet),
      case hashmap_set:has_key(RemovedSet, Key) of
        false -> true;
        true -> false
      end
    end
  ).

%% Заполнение множества
fill_set(Set, []) -> Set;
fill_set(Set, [H | T]) -> fill_set(hashmap_set:insert(H, Set), T).

%% Проверка равенства множеств
is_equal(Set1, Set2) ->
  lists:all(fun(E) -> hashmap_set:has_key(Set1, E) end, hashmap_set:to_list(Set2)) andalso
    lists:all(fun(E) -> hashmap_set:has_key(Set2, E) end, hashmap_set:to_list(Set1)).
