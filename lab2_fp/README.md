# Лабораторная работа №2 (OpenAddress HashMap Set)

---

* Студент: `Ефимов Арслан Альбертович`
* Группа: `P3332`
* ИСУ: `368162`
* Функциональный язык: `Erlang🔥🔥🔥`

--- 

## Требования

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

--- 

## Ключевые элементы реализации

Добавление и удаление элементов:

```erlang
insert(Key, #hash_set{size = Size, capacity = Capacity, table = Table} = Set) ->
    case Size >= Capacity of
        true ->
            resize_and_insert(Key, Set);
        false ->
            Index = hash(Key, Capacity),
            case put_in_table(Index, Key, Table, Capacity) of
                {ok, NewTable} ->
                    Set#hash_set{
                        size = Size + 1,
                        table = NewTable
                    };
                exists ->
                    Set
            end
    end.

remove(Key, #hash_set{size = Size, capacity = Capacity, table = Table} = Set) ->
    Index = hash(Key, Capacity),
    case delete_from_table(Index, Key, Table, Capacity) of
        {ok, NewTable} ->
            Set#hash_set{
                size = Size - 1,
                table = NewTable
            };
        bad_key ->
            error("Didn't find key " ++ Key)
    end.

```

Фильтрация (filter):

```erlang
filter(Pred, #hash_set{table = Table} = Set) ->
    {FilteredTable, NewSize} = helper_filter_and_count(Pred, Table, 0),
    Set#hash_set{table = FilteredTable, size = NewSize}.

helper_filter_and_count(_Fun, [], Count) ->
    {[], Count};
helper_filter_and_count(Fun, [undefined | T], Count) ->
    {FilteredTail, NewCount} = helper_filter_and_count(Fun, T, Count),
    {[undefined | FilteredTail], NewCount};
helper_filter_and_count(Fun, [H | T], Count) ->
    case Fun(H) of
        true ->
            {FilteredTail, NewCount} = helper_filter_and_count(Fun, T, Count + 1),
            {[H | FilteredTail], NewCount};
        false ->
            {FilteredTail, NewCount} = helper_filter_and_count(Fun, T, Count),
            {[undefined | FilteredTail], NewCount}
    end.

```

Отображение (map):

```erlang
map(Fun, #hash_set{capacity = Capacity} = Set) ->
    NewSet = new(Capacity),
    foldl(
        fun(Key, AccSet) ->
            case Key =:= undefined of
                true ->
                    AccSet;
                false ->
                    NewKey = Fun(Key),
                    insert(NewKey, AccSet)
            end
        end,
        NewSet,
        Set
    ).
```

Свертки (левая и правая):

```erlang
helper_foldl(_, Acc, []) ->
    Acc;
helper_foldl(Fun, Acc, [undefined | T]) ->
    helper_foldl(Fun, Acc, T);
helper_foldl(Fun, Acc, [H | T]) ->
    AccNew = Fun(H, Acc),
    helper_foldl(Fun, AccNew, T).

helper_foldr(Fun, Acc, Table) ->
    lists:reverse(Table),
    helper_foldl(Fun, Acc, Table).

foldl(Fun, Acc, #hash_set{table = Table}) ->
    AccNew = helper_foldl(Fun, Acc, Table),
    AccNew.

foldr(Fun, Acc, #hash_set{table = Table}) ->
    AccNew = helper_foldr(Fun, Acc, Table),
    AccNew.
```

## Соответствие свойству моноида

- Нейтральным элементом будет являться пустой `HashSet` - new()

- Определил бинарную операцию union:

```erlang
union(
    #hash_set{size = Size1, capacity = Capacity1} = Set1,
    #hash_set{size = Size2, capacity = Capacity2} = Set2
) ->
    CrossingKeysCount = foldl(
        fun(Key, Acc) ->
            case has_key(Set2, Key) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Set1
    ),

    NewCapacity = max(max(Capacity1, Capacity2), Size1 + Size2 - CrossingKeysCount),

    NewSet = new(NewCapacity),

    SetWithSet1 = foldl_set_helper(NewSet, Set1),

    SetWithSet1AndSet2 = foldl_set_helper(SetWithSet1, Set2),

    SetWithSet1AndSet2.

has_key(#hash_set{table = Table, capacity = Capacity}, Key) ->
    Index = hash(Key, Capacity),
    search_key_in_table(Index, Key, Table, Capacity, Index).

search_key_in_table(Index, Key, Table, Capacity, StartIndex) ->
    case get_element(Index, Table) of
        undefined ->
            false;
        Key ->
            true;
        _Other ->
            NextIndex = (Index + 1) rem Capacity,
            case NextIndex =:= StartIndex of
                true -> false;
                false -> search_key_in_table(NextIndex, Key, Table, Capacity, StartIndex)
            end
    end.

```

---

## Тестирование

В данной лабораторной работе я использовал два инструмента для тестирования:

- EUnit - для модульного тестирования
- Proper - для тестирования свойств (property-based)

---

## Выводы

В результате выполнения данной лабораторной работы я познакомился 
с записями `records` в Erlang, также реализовал структуру OpenAddress HashMap Set, используя для
решения коллизий линейное пробирование, протестировал корректность структуры с помощью модульных тестов и
тестов свойств предикатов.