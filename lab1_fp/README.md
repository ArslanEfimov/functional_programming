# Лабораторная работа №1

- Выполнил: `Ефимов Арслан Альбертович`
- Группа: `P3332`
- Ису: `368162`
- Язык: `Erlang 🔥`

## Задача Эйлера №9

### Описание:
Тройка Пифагора — три натуральных числа `a < b < c`, для которых выполняется равенство:

                        `a² + b² = c²`

Например, `3² + 4² = 9 + 16 = 25 = 5²`.

Существует только одна тройка Пифагора, для которой `a + b + c = 1000`.
Найдите произведение `abc`.

### Решение задачи:

__1. Решение с помощью рекурсии__

---
Для того, чтобы найти нужные тройки, вместо циклов (которых нет в `Erlang`) используем 
рекурсивные вызовы, в которых проходимся по значениям `A` и `B`, вычисляем `C` и подбираем
значения. Рекурсия здесь нужна для вызовов функции с разными параметрами и нахождения следующих.
```erlang
find_triplet() ->
    find_triplet(1, 2, 997).

find_triplet(A, B, C) ->
    case A + B + C of
        1000 ->
            case A * A + B * B =:= C * C of
                true -> A * B * C;
                false -> find_next(A, B, C)
            end;
        _ ->
            find_next(A, B, C)
    end.

find_next(A, B, C) ->
    case {B < C - 1, A < 999} of
        {true, _} ->
            find_triplet(A, B + 1, C - 1);
        {false, true} ->
            find_triplet(A + 1, A + 2, 1000 - (A + 1 + (A + 2)));
        {false, false} ->
            error
    end.
```
***

__2. Решение с помощью хвостовой рекурсии__

___
Логика остается такой же, как и с обычной рекурсией, но теперь вызов функции осуществляется в самом конце,
что позволяет не переполнять стек, потому что нам не нужно запомнинать состояние и адрес возврата предыдущего 
вызова для дальнейших вычислений.
```erlang
ind_triplet() ->
    find_triplet(1, 2, 997).

find_triplet(A, B, C) when A + B + C =:= 1000 ->
    case A * A + B * B =:= C * C of
        true ->
            A * B * C;
        false ->
            find_next(A, B, C)
    end;
find_triplet(A, B, C) ->
    find_next(A, B, C).

find_next(A, B, C) when B < C - 1 ->
    find_triplet(A, B + 1, C - 1);
find_next(A, _, _) when A < 999 ->
    find_triplet(A + 1, A + 2, 1000 - (A + 1 + (A + 2)));
find_next(_, _, _) ->
    error.

```
***

__3. Решение с помощью модульной реализации с использованием (`fold, filter`)__
Здесь мы используем сверстку, чтобы создать массив из троек `A`, `B`, `C`, которые
в сумме дают 1000, а фильтром находим ту, которая выполняет условие. 
___
```erlang
find_triplet() ->
    Triplets = generate_triplets(),
    PythagoreanTriplets = filter_pythagorean_triplets(Triplets),
    {A, B, C} = find_product(PythagoreanTriplets),
    A * B * C.

generate_triplets() ->
    lists:foldl(
        fun(A, Acc1) ->
            lists:foldl(
                fun(B, Acc2) ->
                    C = 1000 - A - B,
                    [{A, B, C} | Acc2]
                end,
                Acc1,
                lists:seq(A + 1, 999)
            )
        end,
        [],
        lists:seq(1, 998)
    ).

filter_pythagorean_triplets(Triplets) ->
    lists:filter(fun({A, B, C}) -> A * A + B * B =:= C * C end, Triplets).

find_product([{A, B, C} | _]) ->
    {A, B, C}.
```
***

__4. Решение с помощью отображения `map`__

___
Гениальное использование отображение `map`, делаем все то же самое, находим тройки и фильтруем.
```erlang
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
```
***

## Задача Эйлера №22

### Описание:

Используйте [names.txt](src/euler_22/names.txt) (щелкнуть правой кнопкой мыши и выбрать 'Save Link/Target As...'), текстовый файл размером ___46 КБ___, содержащий более пяти тысяч имен. Начните с сортировки в алфавитном порядке. Затем подсчитайте алфавитные значения каждого имени и умножьте это значение на порядковый номер имени в отсортированном списке для получения количества очков имени.

Например, если список отсортирован по алфавиту, имя `COLIN` (алфавитное значение которого `3 + 15 + 12 + 9 + 14 = 53`) является ___938-м___ в списке. Поэтому, имя `COLIN` получает `938 × 53 = 49714` очков.

**Какова сумма очков имен в файле?**

### Решение задачи:

__1. Решение с помощью рекурсии__

___
Здесь же я использую демонстрацию рекурсии, написав быструю сортировку, которая сортирует список имен, 
а также использую для составления массива очков для имен
```erlang
find_final_score() ->
    {ok, Binary} = file:read_file("src/euler_22/names.txt"),
    Names = format_names(binary_to_list(Binary)),
    SortedNames = quicksort(Names),
    Scores = calculate_scores(SortedNames, 1),
    TotalScore = lists:sum(Scores),
    TotalScore.

quicksort([]) ->
    [];
quicksort([Pivot | Rest]) ->
    Less = [X || X <- Rest, X =< Pivot],
    Greater = [X || X <- Rest, X > Pivot],
    quicksort(Less) ++ [Pivot] ++ quicksort(Greater).

format_names(Content) ->
    NameList = string:tokens(Content, "\",\""),
    NameList.

calculate_scores([], _) ->
    [];
calculate_scores([Name | Rest], Index) ->
    Score = name_value(Name) * Index,
    [Score | calculate_scores(Rest, Index + 1)].

name_value(Name) ->
    lists:sum([char_value(Char) || Char <- Name, Char >= $A, Char =< $Z]).

char_value(Char) ->
    Char - $A + 1.
```
***

__2. Решение с помощью хвостовой рекурсии__

___
Хвостовую рекурсию использую для составления массива очков для имен
```erlang
find_final_score() ->
    {ok, Binary} = file:read_file("src/euler_22/names.txt"),
    Names = format_names(binary_to_list(Binary)),
    SortedNames = quicksort(Names),
    Scores = calculate_scores(SortedNames),
    TotalScore = lists:sum(Scores),
    TotalScore.

quicksort([]) ->
    [];
quicksort([Pivot | Rest]) ->
    Less = [X || X <- Rest, X =< Pivot],
    Greater = [X || X <- Rest, X > Pivot],
    quicksort(Less) ++ [Pivot] ++ quicksort(Greater).

format_names(Content) ->
    NameList = string:tokens(Content, "\",\""),
    NameList.

calculate_scores(Names) ->
    calculate_scores(Names, 1, []).

calculate_scores([], _, Acc) ->
    lists:reverse(Acc);
calculate_scores([Name | Rest], Index, Acc) ->
    Score = name_value(Name) * Index,
    calculate_scores(Rest, Index + 1, [Score | Acc]).

name_value(Name) ->
    lists:sum([char_value(Char) || Char <- Name, Char >= $A, Char =< $Z]).

char_value(Char) ->
    Char - $A + 1.
```
***

__3. Решение с помощью модульной реализации с использованием (`fold, filter`)__
Снова же логика та же, но теперь использую для составления массива очков сверстку, а для проверки условия, что символ не выходит
за рамки алфавита использую фильтры
___
```erlang
find_final_score() ->
    {ok, Binary} = file:read_file("src/euler_22/names.txt"),
    Names = format_names(binary_to_list(Binary)),
    SortedNames = generate_sequence(Names),
    FilteredNames = filter_names(SortedNames),
    Scores = calculate_scores(FilteredNames),
    TotalScore = lists:sum(Scores),
    TotalScore.

generate_sequence(Names) ->
    lists:sort(Names).

filter_names(Names) ->
    lists:filter(fun name_valid/1, Names).

name_valid(Name) ->
    lists:all(fun char_uppercase/1, Name).

char_uppercase(Char) ->
    Char >= $A andalso Char =< $Z.

calculate_scores(Names) ->
    lists:foldl(
        fun({Index, Name}, Acc) ->
            Score = name_value(Name) * Index,
            [Score | Acc]
        end,
        [],
        lists:zip(lists:seq(1, length(Names)), Names)
    ).

name_value(Name) ->
    lists:foldl(fun(Char, Acc) -> Acc + (Char - $A + 1) end, 0, Name).

format_names(Content) ->
    NameList = string:tokens(Content, "\",\""),
    NameList.
```
***

__4. Решение с помощью отображения `map`__
Логика та же, что и со сверсткой, только теперь использую `map`
___
```erlang
find_final_score() ->
    {ok, Binary} = file:read_file("src/euler_22/names.txt"),
    Names = format_names(binary_to_list(Binary)),
    SortedNames = generate_sequence(Names),
    FilteredNames = filter_names(SortedNames),
    Scores = calculate_scores(FilteredNames),
    TotalScore = lists:sum(Scores),
    TotalScore.

generate_sequence(Names) ->
    lists:sort(Names).

filter_names(Names) ->
    lists:filter(fun name_valid/1, Names).

name_valid(Name) ->
    lists:all(fun char_uppercase/1, Name).

char_uppercase(Char) ->
    Char >= $A andalso Char =< $Z.

calculate_scores(Names) ->
    lists:map(
        fun({Name, Index}) ->
            name_value(Name) * Index
        end,
        lists:zip(Names, lists:seq(1, length(Names)))
    ).

name_value(Name) ->
    lists:foldl(fun(Char, Acc) -> Acc + (Char - $A + 1) end, 0, Name).

format_names(Content) ->
    NameList = string:tokens(Content, "\",\""),
    NameList.
```
***

## Выводы:

В данной лабораторной работой я овладел базовыми навыками, чтобы писать на языке `Erlang`, познакомился с его 
синтаксисом и в целом лучше узнал функциональное программирование.
Мне удалось познакомиться с очень мощными инструментами, такими как:
- `Генераторы последовательностей`
- `Сопоставление с образцом или pattern matching`
- `Сверстки (fold), фильтры (filter), отображения (map)`

Нельзя не упомянуть рекурсию, с помощью которой реализуются циклы в `Erlang`, а также её используют вышеупомянутые инструменты. 
Также есть и хвостовая рекурсия, `TCO` (tail call optimization), которая позволяет правильно работать с памятью, не переполняя стек. 
Ну и на последок упомянем анонимные функции, которые интегрируются с фильтрами, сверстками, отображениями и так далее. Анонимные функции позволяет гибко и компактно работать с данными.

В заключение хочется сказать, что `Erlang` имеет очень мощные инструменты по работе с данными, писать небольшой и читаемый код,
который будет разделен на функции, а также в данном языке все процессы изолированы и не имеют доступ к памяти других процессов, что упрощает жизнь программистам.