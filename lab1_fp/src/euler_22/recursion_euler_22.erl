-module(recursion_euler_22).
-export([main/0]).

main() ->
  {ok, Binary} = file:read_file("/Users/arslanefimov/IdeaProjects/functional_programming/lab1_fp/src/euler_22/names.txt"),
  Names = format_names(binary_to_list(Binary)),
  SortedNames = quicksort(Names),
  Scores = calculate_scores(SortedNames, 1),
  TotalScore = lists:sum(Scores),
  io:format("Total score: ~p~n", [TotalScore]),
  TotalScore.

quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
  Less = [X || X <- Rest, X =< Pivot],
  Greater = [X || X <- Rest, X > Pivot],
  quicksort(Less) ++ [Pivot] ++ quicksort(Greater).


format_names(Content) ->
  NameList = string:tokens(Content, "\",\""),
  NameList.

calculate_scores([], _) -> [];
calculate_scores([Name | Rest], Index) ->
  Score = name_value(Name) * Index,
  [Score | calculate_scores(Rest, Index + 1)].


name_value(Name) ->
  lists:sum([char_value(Char) || Char <- Name, Char >= $A, Char =< $Z]).

char_value(Char) ->
  Char - $A + 1.