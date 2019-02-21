%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% mysql数据库API封装
%%% @end
%%% Created : 07. 十二月 2018 13:49
%%%-------------------------------------------------------------------
-module(db).
-author("suyang").
-export([init_mysql/1,
    execute/1,
    execute/2,
    execute/3,
    insert_batch/2,
    select_limit/3,
    select_limit/4,
    get_one/1,
    get_one/2,
    get_one/3,
    get_row/1,
    get_row/2,
    get_row/3,
    get_all/1,
    get_all/2,
    format_sql/2,
    tx/1,
    format_sql_batch/2
]
).

-define(DB, mysql_conn_poll).
-include("common.hrl").

-ifdef(debug_sql).
-define(DB_START, mysql:start_link(?DB, DbHost, DbPort, DbUser, DbPass, DbName)).
-else.
-define(DB_START, mysql:start_link(?DB, DbHost, DbPort, DbUser, DbPass, DbName, fun(_, _, _, _) -> ok end, DbEncode)).
-endif.

%% MYSQL数据库连接初始化
init_mysql(Cfg) ->
    case init_args(Cfg) of
        [DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, DbConnNum] ->
            ?DB_START,
            util_math:for(1, DbConnNum,
                fun(_I) ->
                    mysql:connect(?DB, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, true)
                end
            ),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% 初始化参数
init_args(Args) ->
    init_args([db_host, db_port, db_user, db_pass, db_name, db_encode, db_conn_num], Args, []).

init_args([], _Args, Back) ->
    lists:reverse(Back);
init_args([Type | T], Args, Back) ->
    case proplists:get_value(Type, Args) of
        undefined ->
            {error, util:fbin(<<"错误的参数配置: ~w">>, [Type])};
        Val ->
            init_args(T, Args, [Val | Back])
    end.

%% @spec execute(Sql) -> Affected | {error, bitstring()}
%% Sql = iolist()
%% Affected = integer()
%% @doc 执行一个SQL查询,返回影响的行数
execute(Sql) ->
    case catch mysql:fetch(?DB, Sql) of
        {updated, {_, _, _, R, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} ->
            ?ERROR_MSG("===============Sql error Err:~w", [Err]),
            format_error(Sql, Err);
        Err ->
            ?ERROR_MSG("===============Sql Err:~w", [Err]),
            {error, Err}
    end.

%% @spec execute(Sql, Args) -> Affected | {error, bitstring()}
%% Sql = iolist()
%% Args = list()
%% Affected = integer()
%% @doc 执行一个带格式化参数的SQL查询,返回影响的行数
execute(Sql, Args) when is_atom(Sql) ->
    case catch mysql:execute(?DB, Sql, Args) of
        {updated, {_, _, _, R, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end;
execute(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query) of
        {updated, {_, _, _, R, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.
execute(Sql, Args, Timeout) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query, Timeout) of
        {updated, {_, _, _, R, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec insert_batch(Sql, Args) -> Affected | {error, bitstring()}
%% Sql = iolist()
%% Args = [list() | ...]
%% Affected = integer()
%% @doc 执行一个带格式化参数的SQL查询,返回影响的行数
insert_batch(Sql, Args) ->
    Query = format_sql_batch(Sql, Args),
    case catch mysql:fetch(?DB, Query) of
        {updated, {_, _, _, R, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec select_limit(Sql, Offset, Num) -> list() | {error, bitstring()}
%% Sql = iolist() | string()
%% Offset = integer()
%% Num = integer()
%% @doc 执行分页查询，返回结果中的所有行
select_limit(Sql, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, integer_to_list(Offset), <<", ">>, integer_to_list(Num)]),
    case catch mysql:fetch(?DB, S) of
        {data, {_, _, R, _, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec select_limit(Sql, Args, Offset, Num) -> list() | {error, bitstring()}
%% Sql = iolist() | string()
%% Args = list()
%% Offset = integer()
%% Num = integer()
%% @doc 执行分页查询(带格式化参数)，返回结果中的所有行
select_limit(Sql, Args, Offset, Num) ->
    S = list_to_binary([Sql, <<" limit ">>, list_to_binary(integer_to_list(Offset)), <<", ">>, list_to_binary(integer_to_list(Num))]),
    mysql:prepare(s, S),
    case catch mysql:execute(?DB, s, Args) of
        {data, {_, _, R, _, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec get_one(Sql) -> {ok, term()} | {error, bitstring()}
%% Sql = iolist()
%% @doc 取出查询结果中的第一行第一列(不带格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
get_one(Sql) ->
    case catch mysql:fetch(?DB, Sql) of
        {data, {_, _, [[R]], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec get_one(Sql, Args) -> term() | {error, bitstring()}
%% Sql = iolist() | string()
%% Args = list()
%% @doc 取出查询结果中的第一行第一列(带有格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
get_one(Sql, Args) when is_atom(Sql) ->
    case catch mysql:execute(?DB, Sql, Args) of
        {data, {_, _, [[R]], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end;
get_one(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query) of
        {data, {_, _, [[R]], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.
get_one(Sql, Args, TimeOut) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query, TimeOut) of
        {data, {_, _, [[R]], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec get_row(Sql) -> term() | {error, bitstring()}
%% Sql = iolist()
%% @doc 取出查询结果中的第一行
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{error, undefined}</div>
get_row(Sql) ->
    case catch mysql:fetch(?DB, Sql) of
        {data, {_, _, [R], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec get_row(Sql, Args) -> term() | {error, bitstring()}
%% Sql = iolist() | string()
%% @doc 取出查询结果中的第一行(带有格式化参数)
%% <div>注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{error, undefined}</div>
get_row(Sql, Args) when is_atom(Sql) ->
    case catch mysql:execute(?DB, Sql, Args) of
        {data, {_, _, [R], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end;
get_row(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query) of
        {data, {_, _, [R], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.
get_row(Sql, Args, TimeOut) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query, TimeOut) of
        {data, {_, _, [R], _, _}} -> {ok, R};
        {data, {_, _, [], _, _}} -> {error, undefined};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec get_all(Sql) -> term() | {error, bitstring()}
%% Sql = iolist()
%% @doc 取出查询结果中的所有行
get_all(Sql) ->
    case catch mysql:fetch(?DB, Sql) of
        {data, {_, _, R, _, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec get_all(Sql, Args) -> term() | {error, bitstring()}
%% Sql = iolist() | string()
%% @doc 取出查询结果中的所有行
get_all(Sql, Args) when is_atom(Sql) ->
    case catch mysql:execute(?DB, Sql, Args) of
        {data, {_, _, R, _, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end;
get_all(Sql, Args) ->
    Query = format_sql(Sql, Args),
    case catch mysql:fetch(?DB, Query) of
        {data, {_, _, R, _, _}} -> {ok, R};
        {error, {_, _, _, _, Err}} -> format_error(Sql, Err);
        Err -> {error, Err}
    end.

%% @spec tx(Fun) -> {ok, Result} | {error, Reason}
%% Fun = function()
%% Result = term()
%% Reason = atom()
%% @doc 执行一个事务操作
tx(Fun) -> tx(Fun, undefined).

%% @spec tx(Fun, Timeout) -> {ok, Result} | {error, Reason}
%% Fun = function()
%% Timeout = integer()
%% Result = term()
%% Reason = atom()
%% @doc 执行一个事务操作(带超时设定)
tx(Fun, Timeout) ->
    case catch mysql:transaction(?DB, Fun, Timeout) of
        {atomic, Result} -> {ok, Result};
        {aborted, {Reason, {rollback_result, _Result}}} -> {error, Reason};
        Err -> {error, Err}
    end.

%% @spec format_sql(Sql, Args) -> iolist()
%% Sql = string() | iolist()
%% Args = list()
%% @doc 格式化sql语句
format_sql(Sql, Args) when is_list(Sql) ->
    S = re:replace(Sql, "\\?", "~s", [global, {return, list}]),
    L = [mysql:encode(A) || A <- Args],
    list_to_bitstring(io_lib:format(S, L));
format_sql(Sql, Args) when is_bitstring(Sql) ->
    format_sql(bitstring_to_list(Sql), Args).

%% 格式化SQL错误
format_error(Sql, Error) ->
    Emsg = util:fbin(<<"执行SQL语句出错:~n[SQL] ~s~n[ERR] ~s">>, [Sql, Error]),
    {error, Emsg}.

%% 格式化批量插入参数
format_sql_batch(Sql, Args = [H | _T]) when is_list(Sql) ->
    L = do_format_sql_args(Args, length(H)),
    list_to_bitstring(Sql ++ " values " ++ L);
format_sql_batch(Sql, Args) when is_bitstring(Sql) ->
    format_sql_batch(bitstring_to_list(Sql), Args).

do_format_sql_args(Args = [H | T], Length) when is_list(Args) ->
    First = "(" ++ gen_args(Length) ++ ")",
    NotFirst = "," ++ First,
    lists:concat([do_format(H, First) | lists:map(fun(A) -> do_format(A, NotFirst) end, T)]).

do_format(Args, Pattern) ->
    L = [mysql:encode(A) || A <- Args],
    io_lib:format(Pattern, L).

gen_args(Length) ->
    do_gen_args("~s", Length - 1).

do_gen_args(Back, L) when L =< 0 ->
    Back;
do_gen_args(Back, L) ->
    do_gen_args(Back ++ ", ~s", L - 1).


