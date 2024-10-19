-module(hashmap_set).
-export([
    test/0,
    remove/2,
    insert/2,
    foldl/3,
    foldr/3,
    filter/2,
    map/2,
    new/1, new/0,
    get_element/2,
    has_key/2,
    union/2,
    to_list/1
]).
-include("records.hrl").

new(Capacity) ->
    #hash_set{
        size = 0,
        capacity = Capacity,
        table = lists:duplicate(Capacity, undefined)
    }.

new() ->
    new(?DEFAULT_CAPACITY).

hash(Key, Capacity) ->
    erlang:phash2(Key, Capacity).

get_element(Index, Table) ->
    lists:nth(Index + 1, Table).

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

put_in_table(Index, Key, Table, Capacity) ->
    case get_element(Index, Table) of
        undefined ->
            {ok, replace_nth(Index + 1, Key, Table)};
        K when K == Key ->
            exists;
        _Other ->
            put_in_table((Index + 1) rem Capacity, Key, Table, Capacity)
    end.

delete_from_table(Index, Key, Table, Capacity) ->
    case get_element(Index, Table) of
        undefined ->
            bad_key;
        K when K == Key ->
            {ok, replace_nth(Index + 1, undefined, Table)};
        _Other ->
            delete_from_table((Index + 1) rem Capacity, Key, Table, Capacity)
    end.

replace_nth(1, Elem, [_ | Tail]) ->
    [Elem | Tail];
replace_nth(N, Elem, [Head | Tail]) when N > 1 ->
    [Head | replace_nth(N - 1, Elem, Tail)];
replace_nth(_, _, []) ->
    [].

resize_and_insert(Key, #hash_set{capacity = Capacity} = Set) ->
    NewCapacity = Capacity * 2,
    ResizedSet = new(NewCapacity),
    ResizedSetUpdated = foldl_set_helper(ResizedSet, Set),
    insert(Key, ResizedSetUpdated).

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

foldl_set_helper(Set1, Set2) ->
    foldl(
        fun(K, Acc) ->
            case K =:= undefined of
                true -> Acc;
                false -> insert(K, Acc)
            end
        end,
        Set1,
        Set2
    ).

%% Преобразование множества в список
to_list(#hash_set{table = Table}) ->
    to_list_helper(Table, []).

to_list_helper([], Acc) ->
    % Возвращаем список в обратном порядке
    lists:reverse(Acc);
to_list_helper([undefined | T], Acc) ->
    % Пропускаем элементы, равные undefined
    to_list_helper(T, Acc);
to_list_helper([H | T], Acc) ->
    % Добавляем элемент в аккумулятор
    to_list_helper(T, [H | Acc]).

test() ->
    Set = new(),
    Set1 = insert(10, Set),
    Set2 = insert(9, Set1),
    Set3 = insert(8, Set2),
    Set4 = insert(7, Set3),
    Set5 = filter(fun(X) -> X =:= 7 end, Set4),
    Set5#hash_set.size.
