%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 玩家每日数据
%%% @end
%%% Created : 19. 十二月 2018 16:38
%%%-------------------------------------------------------------------
-module(lib_daily).
-author("suyang").

%% API
-export([
    init/2,
    get_all/1,
    get_count/2,
    set_count/3,
    add_count/3,
    del_count/3,
    add_and_get_count/3,
    get_count_list/2,
    add_count_fun/6,
    daily_clear/2,
    save/1,
    set_special_info/3,
    get_special_info/2
]).

-export([get_all_db/1,
    get_count_db/2,
    get_count_list_db/2,
    add_count_db/3,
    add_and_get_count_db/3,
    add_count_fun_db/6,
    set_count_db/3,
    del_count_db/3,
    clear_count_db/2,
    daily_clear_db/2,
    truncate_role_daily_counter/2]).

-include("common.hrl").
-include("role.hrl").

%% 玩家每日数据宏标识定义
-define(ROLE_DAILY_DATA(RoleId), {role_daily_data, RoleId}).
-define(ROLE_SPECIAL_INFO(RoleId, Key), {role_special_info, RoleId, Key}).

%%%===================================================================
%%% API 在线处理相关
%%%===================================================================
%% @doc 初始化玩家数据
init(NowTime, RoleId) ->
    Data = daily_load(NowTime, RoleId),
    erlang:put(?ROLE_DAILY_DATA(RoleId), Data).

%% @doc 获取所有计数信息
get_all(RoleId) ->
    [Dict, _Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    dict:to_list(Dict).

%% @doc 获取计数
get_count(RoleId, Type) ->
    [Dict, _Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    get_count2(Dict, Type).

%% @doc 设置计数
set_count(RoleId, Type, Num) ->
    [Dict, Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    NewDict = set_count2(Dict, Type, Num),
    erlang:put(?ROLE_DAILY_DATA(RoleId), [NewDict, Time]).

%% @doc 计数增加
add_count(RoleId, Type, Num) ->
    [Dict, Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    NewDict = add_count2(Dict, Type, Num),
    erlang:put(?ROLE_DAILY_DATA(RoleId), [NewDict, Time]).

%% @doc 计数减少
del_count(RoleId, Type, Num) ->
    [Dict, Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    NewDict = del_count2(Dict, Type, Num),
    erlang:put(?ROLE_DAILY_DATA(RoleId), [NewDict, Time]).

%% @doc 增加计数并返回增加后的计数
add_and_get_count(RoleId, Type, Num) ->
    [Dict, Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    NewDict = add_count2(Dict, Type, Num),
    erlang:put(?ROLE_DAILY_DATA(RoleId), [NewDict, Time]),
    get_count2(NewDict, Type).

%% @doc 获取计数列表
get_count_list(RoleId, TypeList) ->
    [Dict, _Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    CounterList = [{T, get_count2(Dict, T)} || T <- TypeList],
    CounterList.

%% @doc 计数增加(回调函数)
add_count_fun(RoleId, Type, Num, M, F, Arg) ->
    [Dict, Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    NewDict = add_count2(Dict, Type, Num),
    AllNum = get_count2(NewDict, Type),
    M:F(Arg ++ [AllNum]),
    erlang:put(?ROLE_DAILY_DATA(RoleId), [NewDict, Time]).

%% @doc 每日清空缓存
daily_clear(RoleId, Time) ->
    erlang:put(?ROLE_DAILY_DATA(RoleId), [dict:new(), Time]).

%% @doc 数据持久化
save(RoleId) ->
    [Dict, Time] = erlang:get(?ROLE_DAILY_DATA(RoleId)),
    daily_save(RoleId, Dict, Time).

%% @doc 存键值数据
set_special_info(RoleId, Key, Value) ->
    erlang:put(?ROLE_SPECIAL_INFO(RoleId, Key), Value).

%% @doc 取键值数据
get_special_info(RoleId, Key) ->
    erlang:get(?ROLE_SPECIAL_INFO(RoleId, Key)).

%%%===================================================================
%%% API 离线处理相关
%%%===================================================================
%% @doc 离线获取全部次数
get_all_db(RoleId) ->
    [Dict, _Time] = daily_load(RoleId),
    dict:to_list(Dict).

%% @doc 离线获取每日次数
get_count_db(RoleId, Type) ->
    [Dict, _Time] = daily_load(RoleId),
    get_count2(Dict, Type).

%% @doc 离线获取每日次数列表
get_count_list_db(RoleId, TypeList) ->
    [Dict, _Time] = daily_load(RoleId),
    [{T, get_count2(Dict, T)} || T <- TypeList].

%% @doc 离线增加每日次数
add_count_db(RoleId, Type, Num) ->
    [Dict, Time] = daily_load(RoleId),
    NewDict = add_count2(Dict, Type, Num),
    daily_save(RoleId, NewDict, Time).

%% @doc 离线增加并返回增加后的每日次数
add_and_get_count_db(RoleId, Type, Num) ->
    [Dict, Time] = daily_load(RoleId),
    NewDict = add_count2(Dict, Type, Num),
    daily_save(RoleId, NewDict, Time),
    get_count2(NewDict, Type).

%% @doc 离线增加每日次数,增加回调函数
add_count_fun_db(RoleId, Type, Num, M, F, Arg) ->
    [Dict, Time] = daily_load(RoleId),
    NewDict = add_count2(Dict, Type, Num),
    daily_save(RoleId, NewDict, Time),
    AllNum = get_count2(NewDict, Type),
    M:F(Arg ++ [AllNum]).

%% @doc 离线设置每日次数
set_count_db(RoleId, Type, Num) ->
    [Dict, Time] = daily_load(RoleId),
    NewDict = set_count2(Dict, Type, Num),
    daily_save(RoleId, NewDict, Time).

%% @doc 离线减少每日次数
del_count_db(RoleId, Type, Num) ->
    [Dict, Time] = daily_load(RoleId),
    NewDict = del_count2(Dict, Type, Num),
    daily_save(RoleId, NewDict, Time).

%% @doc 离线清空每日次数（按类型）
clear_count_db(RoleId, Type) ->
    [Dict, Time] = daily_load(RoleId),
    NewDict = dict:store(Type, 0, Dict),
    daily_save(RoleId, NewDict, Time).

%% @doc 离线清空所有数据
daily_clear_db(RoleId, Today) ->
    daily_save(RoleId, dict:new(), Today).

%% @doc 清空玩家计数数据库数据
-define(SQL_ROLE_DAILY_DATA_UPDATE, <<"update `role_daily_data` set `data`='~s', `time`=~p">>).
truncate_role_daily_counter(Num, _Today) when Num =< 0 -> skip;
truncate_role_daily_counter(Num, Today) ->
    DictBin = util_type:term_to_bitstring([]),
    SQL = io_lib:format(?SQL_ROLE_DAILY_DATA_UPDATE, [DictBin, Today]),
    case catch db:execute(SQL) of
        {'EXIT', Err} ->
            % 运行报错10s后再次运行
            ?ERROR_MSG("truncate_role_daily_counter Error ~p", [Err]),
            % 5s后再执行
            util:sleep(5000),
            truncate_role_daily_counter(Num - 1, Today);
        _ -> skip
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 加载数据
-define(SQL_ROLE_DAILY_DATA_GET, <<"select `data`, `time` from `role_daily_data` where `role_id`=~p">>).
-define(SQL_ROLE_DAILY_DATA_INSERT, <<"insert into `role_daily_data` (`role_id`, `data`, `time`) values(~p, '~s', ~p)">>).
daily_load(RoleId) ->
    daily_load(util_time:unixtime(), RoleId).
daily_load(NowTime, RoleId) ->
    Today = util_time:unixdate(NowTime),
    Sql = io_lib:format(?SQL_ROLE_DAILY_DATA_GET, [RoleId]),
    case db:get_row(Sql) of
        {ok, [DataBin, Time]} ->
            case util_time:is_same_day(NowTime, Time) of
                true ->
                    [dict:from_list(util_type:bitstring_to_term(DataBin)), Time];
                false ->
                    [dict:new(), Today]
            end;
        _ ->
            DictBin = util_type:term_to_bitstring([]),
            SQL = io_lib:format(?SQL_ROLE_DAILY_DATA_INSERT, [RoleId, DictBin, Today]),
            db:execute(SQL),
            [dict:new(), Today]
    end.

%% 保存数据
-define(SQL_ROLE_DAILY_DATA_UPDATE_BY_ID_AND_TIME, <<"update `role_daily_data` set `data`='~s', `time`=~p where `role_id`=~p">>).
daily_save(RoleId, Dict, Time) ->
    DictBin = util_type:term_to_bitstring(dict:to_list(Dict)),
    SQL = io_lib:format(?SQL_ROLE_DAILY_DATA_UPDATE_BY_ID_AND_TIME, [DictBin, Time, RoleId]),
    db:execute(SQL).

%% 获取计数
get_count2(Dict, Type) ->
    case dict:find(Type, Dict) of
        error -> 0;
        {ok, Value} -> Value
    end.

%% 增加计数
add_count2(Dict, Type, Num) ->
    dict:update_counter(Type, Num, Dict).

%% 设置计数
set_count2(Dict, Type, Num) ->
    dict:store(Type, Num, Dict).

%% 减少计数
del_count2(Dict, Type, Num) ->
    dict:store(Type, max(0, get_count2(Dict, Type) - Num), Dict).