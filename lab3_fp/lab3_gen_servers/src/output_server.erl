-module(output_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).
-export([print_result/1, round_up/2]).

%% Starts the output server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Callbacks
init([]) ->
  {ok, []}.

handle_cast({ok, Type, Result}, State) when Type == linear; Type == lagrange ->
  io:fwrite("~n~s интерполяция: ~n", [(erlang:atom_to_list(Type))]),
  print_result(Result),
  {noreply, State};
handle_cast({error, Method, Msg}, State) ->
  error_logger:error_msg("\e[31mОшибка в методе ~p -- ~p\e[0m~n", [Method, Msg]),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State}.
handle_call(_, _From, State) ->
  {reply, ok, State}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

print_result([GeneratedDots, InterpolatedValues]) ->
  io:fwrite("Сгенерированные точки:~n"),
  lists:foreach(
    fun(Dot) -> io:fwrite("~8.3f ", [float(Dot)]) end,
    GeneratedDots
  ),
  io:fwrite("~nИнтерполированные значения:~n"),
  lists:foreach(
    fun(Value) -> io:fwrite("~8.3f ", [float(round_up(Value, 3))]) end,
    InterpolatedValues
  ),
  io:fwrite("~n").

round_up(Number, Digits) ->
  Factor = math:pow(10, Digits),
  % Ensure zero remains as 0.000 when rounding
  erlang:round(Number * Factor) / Factor.
