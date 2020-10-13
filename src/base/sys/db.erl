%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 9月 2019 18:22
%%%-------------------------------------------------------------------
-module(db).
-author("sy").

%% API
-export([start/8, execute/2, get_all/2, get_row/2, get_one/2]).

-ifdef(debug_sql).
-define(DB_START, mysql:start_link(DbPool, DbHost, DbPort, DbUser, DbPass, DbName)).
-else.
-define(DB_START, mysql:start_link(DbPool, DbHost, DbPort, DbUser, DbPass, DbName, fun(_, _, _, _) -> ok end, DbEncode)).
-endif.

%% @doc
start(DbPool, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNum) ->
  ?DB_START,
  for(1, DbConnNum,
    fun(_I) ->
      mysql:connect(DbPool, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, true)
    end
  ),
  ok.

%% @spec execute(Sql) -> {ok, Affected} | {error, bitstring()}
%% Sql = iolist()
%% Affected = integer()
%% @doc 执行一个SQL查询,返回影响的行数
execute(DbPool, Sql) ->
  case catch mysql:fetch(DbPool, Sql) of
    {updated, {_, _, _, R, _}} -> R;
    {error, {_, _, _, _, Err}} -> {error, Err};
    Err -> {error, Err}
  end.

%% @spec get_all(Sql) -> {ok, term()} | {error, bitstring()}
%% Sql = iolist()
%% @doc 取出查询结果中的所有行
get_all(DbPool, Sql) ->
  case catch mysql:fetch(DbPool, Sql) of
    {data, {_, _, R, _, _}} -> R;
    {error, {_, _, _, _, Err}} -> {error, Err};
    Err -> {error, Err}
  end.

%% @spec get_row(Sql) -> {ok, term()} | {error, bitstring()}
%% Sql = iolist()
%% @doc 取出查询结果中的第一行
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{error, undefined}</div>
get_row(DbPool, Sql) ->
  case catch mysql:fetch(DbPool, Sql) of
    {data, {_, _, [R], _, _}} -> R;
    {data, {_, _, [], _, _}} -> [];
    {error, {_, _, _, _, Err}} ->{error, Err};
    Err -> {error, Err}
  end.

%% @spec get_one(Sql) -> {ok, term()} | {error, bitstring()}
%% Sql = iolist()
%% @doc 取出查询结果中的第一行第一列(不带格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
get_one(DbPool, Sql) ->
  case catch mysql:fetch(DbPool, Sql) of
    {data, {_, _, [R], _, _}} -> R;
    {data, {_, _, [], _, _}} -> [];
    {error, {_, _, _, _, Err}} -> {error, Err};
    Err -> {error, Err}
  end.

%% @spec for(Begin::integer(), End::integer(), Fun::function()) -> ok
%% @doc 模拟for循环
for(End, End, Fun) ->
  Fun(End),
  ok;
for(Begin, End, Fun) when Begin < End ->
  Fun(Begin),
  for(Begin + 1, End, Fun).