%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% mysql数据库API封装
%%% @end
%%% Created : 21. 五月 2019
%%%-------------------------------------------------------------------
-module(db).
-author("sy").

%% API
-export([start/8, get_row/2, execute/2, get_all/2, get_one/2, transaction/2]).


%% mysql链接初始化
start(PoolId, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNum) ->
  mysql:start_link(PoolId, DbHost, DbPort, DbUser, DbPass, DbName),
  util:for(1, DbConnNum,
    fun(_I) ->
      mysql:connect(PoolId, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, true)
    end
  ),
  ok.

get_row(PoolId, Sql) ->
  case catch mysql:fetch(PoolId, Sql) of
    {data, {_, _, [R], _, _, _, _, _}} -> R;
    {data, {_, _, [], _, _, _, _, _}} -> [];
    _Err -> {error, _Err}
  end.

execute(PoolId, Sql) ->
  case catch mysql:fetch(PoolId, Sql) of
    {error, Err} -> {error, Err};
    {update, _} -> ok;
    {data, _} -> ok;
    {updated, _} -> ok
  end.

get_all(PoolId, Sql) ->
  case catch mysql:fetch(PoolId, Sql) of
    {data, {_, _, R, _, _, _, _, _}} -> R;
    _Err -> {error, _Err}
  end.

get_one(PoolId, Sql) ->
  case catch mysql:fetch(PoolId, Sql) of
    {data, {_, _, [[R]], _, _, _, _, _}} -> R;
    {data, {_, _, [], _, _, _, _, _}} -> null;
    _Err -> {error, _Err}
  end.

transaction(PooId, Fun) ->
  case catch mysql:transaction(PooId, Fun) of
    {atomic, Result} -> {ok, Result};
    {aborted, {Reason, {rollback_result, _Result}}} -> {error, Reason};
    Err -> {error, Err}
  end.
