%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 工具包
%%% @end
%%% Created : 07. 十二月 2018 12:24
%%%-------------------------------------------------------------------
-module(util).
-author("suyang").

%% API
-export([fbin/1,
    fbin/2,
    ip2bin/1,
    get_ets/2,
    get_ets/3,
    del_ets/2,
    put_ets/3,
    cls_ets/1,
    get_ip/1,
    sleep/1,
    page_data/3,
    cancel_timer/1,
    cn/2
]).

-export([info/1,
    info/2,
    info/4,
    error/1,
    error/2,
    error/4,
    debug/1,
    debug/2,
    debug/4,
    errlog/2]).

-export([batch_insert/4,
    batch_insert/5,
    batch_insert/6,
    insert_values/3,
    insert_values/4,
    insert_values/5,
    format_values/2,
    implode/2,
    implode/3
    ]).

-include("common.hrl").
%% ----------------------------------------------------------
%% 格式化相关
%% ----------------------------------------------------------
%% @doc 输出系统信息到控制台
info(Msg) ->
    info(Msg, []).
info(Format, Args) ->
    info(Format, Args, null, null).
info(Format, Args, Mod, Line) ->
    Msg = format("info", Format, Args, Mod, Line),
    io:format("~ts", [Msg]).

%% @doc 输出错误信息到控制台
error(Msg) ->
    ?MODULE:error(Msg, []).
error(Format, Args) ->
    ?MODULE:error(Format, Args, null, null).
error(Format, Args, Mod, Line) ->
    Msg = format("error", Format, Args, Mod, Line),
    io:format("~ts", [Msg]).

%% @spec format(T, F, A, Mod, Line) -> list()
%% T = list()
%% F = list()
%% A = list()
%% Mod = list()
%% Line = int()
%% @doc 格式化日志信息
format(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([Y, "/", M, "/", D, "_", H, ":", I, ":", S]),
    case Line of
        null -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~s ", F, "~n"]), [Date] ++ A));
        _ -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## ", T, " ~s [~w:~w] ", F, "~n"]), [Date, Mod, Line] ++ A))
    end.

%% @doc 输出调试信息到控制台
debug(Msg) ->
    debug(Msg, []).
debug(Format, Args) ->
    debug(Format, Args, null, null).
debug(Format, Args, Mod, Line) ->
    IsDebug = case catch sys_env:get(debug_module) of
                  DebugModules when is_list(DebugModules) ->
                      lists:member(Mod, DebugModules);
                  _ ->
                      true
              end,
    case IsDebug of
        true ->
            Msg = case Line of
                      null -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## debug ", Format, "~n"]), Args));
                      _ -> erlang:iolist_to_binary(io_lib:format(lists:concat(["## debug [~w:~w] ", Format, "~n"]), [Mod, Line] ++ Args))
                  end,
            io:format("~ts", [Msg]);
        _ ->
            ok
    end.

%% @log 日志打印
errlog(Format, Args) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    File =
        case get("errlog") of
            % 首次打开
            undefined ->
                FileName = io_lib:format("errlog_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = sys_env:get(log_path) ++ "/" ++ util_type:unicode_string(FileName),
                {ok, File1} = file:open(PathName, [write, append]),
                put("errlog", {File1, {Year, Month, Day}}),
                File1;
            % 已经打开
            {File2, {Year, Month, Day}} ->
                File2;
            % 需要切换
            {File2, _} ->
                file:close(File2),
                FileName = io_lib:format("errlog_~p_~p_~p.txt", [Year, Month, Day]),
                PathName = sys_env:get(log_path) ++ "/" ++ util_type:unicode_string(FileName),
                {ok, File3} = file:open(PathName, [write, append]),
                put("errlog", {File3, {Year, Month, Day}}),
                File3
        end,
    Format1 = util_type:unicode_string("#error" ++ " ~p-~p-~p ~p:~p:~p \r\n" ++ Format ++ "\r\n~n"),
    io:format(File, Format1, [Year, Month, Day, Hour, Min, Sec] ++ Args).


%% @spec fbin(Bin, Args) -> binary()
%% Bin = binary()
%% Args = list()
%% @doc 返回格式化的二进制字符串
%% <ul>
%% <li>Bin: 待格式化的二进制字符串</li>
%% <li>Args: 格式化参数</li>
%% </ul>
fbin(Bin, Args) ->
    list_to_binary(io_lib:format(Bin, Args)).

fbin(Bin) ->
    fbin(Bin, []).

%% @spec cn(F, A) -> ok
%% F = list()
%% A = list()
%% @doc 在控制台显示带中文的字符串
%% <ul>
%% <li>F: 待显示的中文字符串（可带格式化参数）</li>
%% <li>A: 格式化参数</li>
%% </ul>
cn(F, A) ->
    io:format("~ts", [iolist_to_binary(io_lib:format(F, A))]).

%% @doc IP元组转字符
ip2bin(IP) when is_list(IP) ->
    case inet:parse_address(IP) of
        {ok, _IP} -> ip2bin(_IP);
        _ -> "256.256.256.256"
    end;
ip2bin({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D)];
ip2bin(_) ->
    "256.256.256.256".

%% ----------------------------------------------------------
%% ETS相关
%% ----------------------------------------------------------

%% @doc 获取键值
get_ets(Table, Key) ->
    case ets:lookup(Table, Key) of
        [{Key, Val}] -> Val;
        _ -> undefined
    end.
get_ets(Table, Key, Def) ->
    case ets:lookup(Table, Key) of
        [{Key, Val}] -> Val;
        _ -> Def
    end.

%% @doc 存储键值
put_ets(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).

%% @doc 删除键值
del_ets(Table, Key) ->
    ets:delete(Table, Key).

%% @doc 清空表
cls_ets(Table) ->
    ets:delete_all_objects(Table).

%% @doc 获取客户端ip
get_ip(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, _Port}} -> Ip;
        {error, _Reason} -> {0, 0, 0, 0}
    end.


%% @spec sleep(T) -> ok
%% T = integer()
%% @doc 程序暂停执行时长(单位:毫秒)
sleep(T) ->
    receive
    after T ->
        true
    end.

%% @doc 分页取数据
%% @param:: List: 所有数据（列表）
%%          Page: 第几页数据（大于总页数则默认最后页）
%%          PageNum: 每一页数量
%% @return:: {总条数, 当前页, 当前页数据}
page_data(List, Page, Count) ->
    Len = length(List),
    Start = max(Page - 1, 0) * Count + 1,
    if Start > Len ->
        {Len, Page, []};
        true ->
            {Len, Page, lists:sublist(List, Start, Count)}
    end.

%% 取消定时器
cancel_timer(Timer) ->
    case is_reference(Timer) of
        true ->
            erlang:cancel_timer(Timer);
        false ->
            skip
    end,
    [].

%% ----------------------------------------------------------
%% 数据库辅助
%% ----------------------------------------------------------

%% @doc 批量插入 [可控制]
batch_insert(0, Fmt1, Fmt2, List) ->
    insert_values(Fmt1, Fmt2, List);
batch_insert(_Size, _Fmt1, _Fmt2, []) ->
    ok;
batch_insert(Size, Fmt1, Fmt2, List) ->
    {Batch, Left} = util_list:split_list(Size, List),
    insert_values(Fmt1, Fmt2, Batch),
    batch_insert(Size, Fmt1, Fmt2, Left).

batch_insert(0, Fmt1, Fmt2, Fun, List) ->
    insert_values(Fmt1, Fmt2, Fun, List);
batch_insert(_Size, _Fmt1, _Fmt2, _Fun, []) ->
    ok;
batch_insert(Size, Fmt1, Fmt2, Fun, List)
    when is_integer(Size), Size > 0, ?VALID_STR(Fmt1), ?VALID_STR(Fmt2), is_function(Fun, 1), is_list(List) ->
    {Batch, Left} = util_list:split_list(Size, List),
    insert_values(Fmt1, Fmt2, Fun, Batch),
    batch_insert(Size, Fmt1, Fmt2, Fun, Left).

batch_insert(0, Fmt1, Fmt2, Args, Fun, List) -> %% 一次性插入
    insert_values(Fmt1, Fmt2, Args, Fun, List);
batch_insert(_Size, _Fmt1, _Fmt2, _Args, _Fun, []) ->
    ok;
batch_insert(Size, Fmt1, Fmt2, Args, Fun, List)
    when is_integer(Size), Size > 0, ?VALID_STR(Fmt1), ?VALID_STR(Fmt2), is_list(Args), is_function(Fun, 1), is_list(List) ->
    {Batch, Left} = util_list:split_list(Size, List),
    insert_values(Fmt1, Fmt2, Args, Fun, Batch),
    batch_insert(Size, Fmt1, Fmt2, Args, Fun, Left).

%% @doc 批量插入 [一次性]
insert_values(_Fmt1, _Fmt2, []) ->
    ok;
insert_values(Fmt1, Fmt2, List) ->
    insert_values2(Fmt1, Fmt2, [], List).

insert_values(Fmt1, Fmt2, Fun, List) when is_function(Fun, 1), is_list(List) ->
    insert_values(Fmt1, Fmt2, [], Fun, List);
insert_values(Fmt1, Fmt2, Args, List) when is_list(Args) ->
    insert_values2(Fmt1, Fmt2, Args, List).

insert_values(Fmt1, Fmt2, Args, Fun, List) when is_list(Args), is_function(Fun, 1), is_list(List) ->
    RankList = lists:map(Fun, List),
    insert_values2(Fmt1, Fmt2, Args, RankList).

insert_values2(_Fmt1, _Fmt2, _Args, []) ->
    ok;
insert_values2(Fmt1, Fmt2, Args, List) ->
    Values = format_values(Fmt2, List),
    SQL = io_lib:format(Fmt1, Args ++ [Values]),
    db:execute(SQL),
    ok.

%% @doc
format_values(Temp, List) ->
    FinList = format_values(List, Temp, []),
    implode(",", FinList, []).

format_values([], _Temp, Acc) ->
    lists:reverse(Acc);
format_values([L | T], Temp, Acc) ->
    F = io_lib:format(Temp, L),
    format_values(T, Temp, [F | Acc]).


%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([util_type:to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [util_type:to_list(H) | NList],
    implode(S, T, [S | L]).
