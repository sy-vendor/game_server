%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 核心application
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(game).
-author("sy").

%% API
-export([start/2, stop/1]).

%% --------------------------------------------------------------------
%% application callback
%% --------------------------------------------------------------------
%% @doc 启动系统
start(_Type, _Args) ->
  inets:start(),

  case init:get_plain_arguments() of
    [H | T] ->
      Mod = list_to_atom("sup_" ++ H),
      {ok, Pid} = Mod:start_link(T),

      {ok, Pid};
    _ ->
      {error, args_error}
  end.

%% 关闭系统
stop(_State) ->
  ok.