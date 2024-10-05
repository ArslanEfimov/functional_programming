-module(euler22_tail_recursion).
-export([find_final_score/0]).

find_final_score() ->
  {ok, Binary} = file:read_file("src/euler_22/names.txt"),
  Names = format_names(binary_to_list(Binary)),
  SortedNames = quicksort(Names),
  Scores = calculate_scores(SortedNames),
  TotalScore = lists:sum(Scores),
  TotalScore.

quicksort([]) -> [];
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
