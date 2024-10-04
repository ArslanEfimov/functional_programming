%%%-------------------------------------------------------------------
%%% @author arslanefimov

-module(map_euler_22).
-author("arslanefimov").

%% API
-export([main/0]).

main() ->
  {ok, Binary} = file:read_file("/Users/arslanefimov/IdeaProjects/functional_programming/lab1_fp/src/euler_22/names.txt"),
  Names = format_names(binary_to_list(Binary)),

  SortedNames = generate_sequence(Names),

  FilteredNames = filter_names(SortedNames),

  Scores = calculate_scores(FilteredNames),

  TotalScore = lists:sum(Scores),
  io:format("Total score: ~p~n", [TotalScore]),
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
  lists:map(fun({Name, Index}) ->
                      name_value(Name) * Index end, lists:zip(Names, lists:seq(1, length(Names)))).


name_value(Name) ->
  lists:foldl(fun(Char, Acc) -> Acc + (Char - $A + 1) end, 0, Name).

format_names(Content) ->
  NameList = string:tokens(Content, "\",\""),
  NameList.


