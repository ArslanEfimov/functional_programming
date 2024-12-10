%%%-------------------------------------------------------------------
%%% @author arslanefimov
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. дек. 2024 18:45
%%%-------------------------------------------------------------------
-module(interpolations).
-author("arslanefimov").

%% API
-export([start/1, init/1, interpolation_loop/2, points_generator/3]).

%% Точка входа модуля
start(InitialValues) ->
  register(interpolationpid, self()),
  init(InitialValues).

%% Инициализация: создание процессов для каждого метода
init(InitialValues) ->
  Methods = element(3, InitialValues),

  Procs = lists:map(
    fun(X) ->
      case X of
        linear ->
          Pid = linear_interpolation:start_linear(element(1, InitialValues), self()),
          {Pid, linear};
        lagrange ->
          Pid = lagrange_interpolation:start_lagrange(
            self(), element(1, InitialValues), element(2, InitialValues)
          ),
          {Pid, lagrange};
        Method ->
          io:fwrite("Незнакомый метод: ~p, пропущен~n", [Method]),
          undefined
      end
    end,
    Methods
  ),

  ValidProcs = lists:filter(fun(X) -> X =/= undefined end, Procs),
  interpolation_loop(ValidProcs, InitialValues).

%% Основной цикл обработки сообщений
interpolation_loop(Procs, InitialValues) ->
  receive
  %% Обработка запроса на интерполяцию
    {interpolate, Data} ->
      [Pid ! {data, Data} || {Pid, _} <- Procs],
      interpolation_loop(Procs, InitialValues);

  %% Получен результат от одного из процессов
    {ok, Method, Result} ->
      opid ! {ok, Method, Result},
      interpolation_loop(Procs, InitialValues);

  %% Получена ошибка от одного из процессов
    {error, Method} ->
      opid ! {error, Method, "Интерполяция закончилась неудачей"},
      interpolation_loop(Procs, InitialValues)
  end.

%% Генерация списка точек
points_generator(Step, X, Y) when Step > 0, X - Step =< Y ->
  [X | points_generator(Step, X + Step, Y)];
points_generator(_, _, _) ->
  [].

