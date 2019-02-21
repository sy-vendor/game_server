%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 玩家每日数据对外接口
%%% @end
%%% Created : 19. 十二月 2018 16:50
%%%-------------------------------------------------------------------
-module(lib_daily_api).
-author("suyang").

%% API
-export([
    init/3,                %% 初始化数据
    save/2,                %% 数据持久化接口
    get_all/1,             %% 获取全部次数
    get_count/3,           %% 获取每日次数
    get_count/2,           %% 获取每日次数
    get_count_list/2,      %% 获取每日次数列表
    add_and_get_count/2,   %% 增加并返回增加后的次数
    add_and_get_count/3,   %% 增加并返回增加后的次数
    add_count/3,           %% 增加每日次数
    add_count_fun/6,       %% 增加次数和回调函数
    increment/2,           %% 递增每日次数
    set_count/3,           %% 设置每日次数
    decrement/2,           %% 递减每日次数
    del_count/3,           %% 减少每日次数
    clear_count/2,         %% 按类型清空每日次数
    daily_reset/2,         %% 清空所有每日次数
    daily_clear/0,         %% 凌晨0点清空所有玩家数据
    set_special_info/3,    %% 存特殊键值数据
    get_special_info/2     %% 取特殊键值数据
]).

-include("common.hrl").
-include("role.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @spec 初始化数据
%% Role:        玩家数据
%% @return:     ok
%% @end
init(NowTime, _LastLogOutTime, Role) ->
    #role{role_id = RoleId} = Role,
    lib_daily:init(NowTime, RoleId).

%% @spec 获取全部次数
%% Role:        玩家数据
%% @return:     每日次数列表[{Type, Count}, ...]
%% @end
get_all(Role) when is_record(Role, role) ->
    get_all(Role#role.role_id);
get_all(RoleId) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:get_all(RoleId);
                false -> svr_role:apply_call(RolePid, lib_daily, get_all, [RoleId])
            end;
        false ->
            lib_daily:get_all_db(RoleId)
    end.

%% @spec 获取每日次数
%% Role:        玩家数据
%% Type:        类型
%% @return:     每日次数
%% @end
get_count(Role, Type, Default) when is_record(Role, role) ->
    case get_count(Role#role.role_id, Type) of
        0 -> Default;
        Data -> Data
    end;
get_count(RoleId, Type, Default) when is_integer(RoleId) ->
    case get_count(RoleId, Type) of
        0 -> Default;
        Data -> Data
    end.
get_count(Role, Type) when is_record(Role, role) ->
    get_count(Role#role.role_id, Type);
get_count(RoleId, Type) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:get_count(RoleId, Type);
                false -> svr_role:apply_call(RolePid, lib_daily, get_count, [RoleId, Type])
            end;
        false ->
            lib_daily:get_count_db(RoleId, Type)
    end.

%% @spec 获取每日次数列表
%% Role:        玩家数据
%% Type:        类型
%% @return:     每日次数列表[{Type, Count}, ...]
%% @end
get_count_list(Role, TypeList) when is_record(Role, role) ->
    get_count_list(Role#role.role_id, TypeList);
get_count_list(RoleId, TypeList) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:get_count_list(RoleId, TypeList);
                false -> svr_role:apply_call(RolePid, lib_daily, get_count_list, [RoleId, TypeList])
            end;
        false ->
            lib_daily:get_count_list_db(RoleId, TypeList)
    end.

add_and_get_count(Role, Type) when is_record(Role, role) ->
    add_and_get_count(Role#role.role_id, Type, 1).
add_and_get_count(Role, Type, Num) when is_record(Role, role) ->
    add_and_get_count(Role#role.role_id, Type, Num);
add_and_get_count(RoleId, Type, Num) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:add_and_get_count(RoleId, Type, Num);
                false -> svr_role:apply_call(RolePid, lib_daily, add_and_get_count, [RoleId, Type, Num])
            end;
        false ->
            lib_daily:add_and_get_count_db(RoleId, Type, Num)
    end.

%% @spec 增加每日次数
%% Role:        玩家数据
%% Type:        类型
%% Num:         增加数量
%% @return:     ok
%% @end
add_count(Role, Type, Num) when is_record(Role, role) ->
    add_count(Role#role.role_id, Type, Num);
add_count(RoleId, Type, Num) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:add_count(RoleId, Type, Num);
                false -> svr_role:apply_cast(RolePid, lib_daily, add_count, [RoleId, Type, Num])
            end;
        false ->
            lib_daily:add_count_db(RoleId, Type, Num)
    end.

%% @spec 增加每日次数
%%       会把每日次数值和arg一起传进M:F(Arg ++ Num)
%% Role:        玩家数据
%% Type:        类型
%% Num:         增加数量
%% M:           模块
%% F:           函数名
%% Arg:         参数列表
%% @return:     ok
%% @end
add_count_fun(Role, Type, Num, M, F, Arg) when is_record(Role, role) ->
    add_count_fun(Role#role.role_id, Type, Num, M, F, Arg);
add_count_fun(RoleId, Type, Num, M, F, Arg) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:add_count_fun(RoleId, Type, Num, M, F, Arg);
                false -> svr_role:apply_cast(RolePid, lib_daily, add_count_fun, [RoleId, Type, Num, M, F, Arg])
            end;
        false ->
            lib_daily:add_count_fun_db(RoleId, Type, Num, M, F, Arg)
    end.

%% @doc 递增接口
increment(Role, Type) when is_record(Role, role) ->
    add_count(Role, Type, 1);
increment(RoleId, Type) when is_integer(RoleId) ->
    add_count(RoleId, Type, 1).

%% @doc Set数据接口
set_count(Role, Type, Num) when is_record(Role, role) ->
    set_count(Role#role.role_id, Type, Num);
set_count(RoleId, Type, Num) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:set_count(RoleId, Type, Num);
                false -> svr_role:apply_cast(RolePid, lib_daily, set_count, [RoleId, Type, Num])
            end;
        false ->
            lib_daily:set_count_db(RoleId, Type, Num)
    end.

%% @doc 递减接口
decrement(Role, Type) when is_record(Role, role) ->
    del_count(Role, Type, 1);
decrement(RoleId, Type) when is_integer(RoleId) ->
    del_count(RoleId, Type, 1).

%% @spec 减少每日次数
%% Role:        玩家数据
%% Type:        类型
%% Num:         减少数量
%% @return:     ok
%% @end
del_count(Role, Type, Num) when is_record(Role, role) ->
    del_count(Role#role.role_id, Type, Num);
del_count(RoleId, Type, Num) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:del_count(RoleId, Type, Num);
                false -> svr_role:apply_cast(RolePid, lib_daily, del_count, [RoleId, Type, Num])
            end;
        false ->
            lib_daily:del_count_db(RoleId, Type, Num)
    end.

%% @spec 按类型清空
%% Role:      玩家数据
%% @return:   ok
%% @end
clear_count(Role, Type) when is_record(Role, role) ->
    clear_count(Role#role.role_id, Type);
clear_count(RoleId, Type) when is_integer(RoleId) ->
    set_count(RoleId, Type, 0).

%% @doc 凌晨0点清空
daily_clear() ->
    % 清数据库
    Today = util_time:unixtime(today),
    spawn(fun() -> lib_daily:truncate_role_daily_counter(5, Today) end).

%% @spec 每日清空
%% Role:      玩家数据
%% @return:   ok
%% @end
daily_reset(Today, Role) when is_record(Role, role) ->
    daily_reset(Today, Role#role.role_id);
daily_reset(Today, RoleId) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:daily_clear(RoleId, Today);
                false -> svr_role:apply_cast(RolePid, lib_daily, daily_clear, [RoleId, Today])
            end;
        false ->
            lib_daily:daily_clear_db(RoleId, Today)
    end.

%% @spec 数据持久化接口
%% Role:      玩家数据
%% @end
save(NowTime, Role) when is_record(Role, role) ->
    save(NowTime, Role#role.role_id);
save(_NowTime, RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:save(RoleId);
                false -> svr_role:apply_cast(RoleId, lib_daily, save, [RoleId])
            end;
        false ->
            ignore
    end.

%% @spec 特殊键值数据存取
%% Role:        玩家数据
%% Key:         键
%% Value:       值
%% @return:     ok
%% @end

%% @doc 存数据
set_special_info(Role, Key, Value) when is_record(Role, role) ->
    set_special_info(Role#role.role_id, Key, Value);
set_special_info(RoleId, Key, Value) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:set_special_info(RoleId, Key, Value);
                false -> svr_role:apply_cast(RolePid, lib_daily, set_special_info, [RoleId, Key, Value])
            end;
        false ->
            ignore
    end.

%% @doc 取数据
get_special_info(Role, Key) when is_record(Role, role) ->
    get_special_info(Role#role.role_id, Key);
get_special_info(RoleId, Key) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case util_dist:is_process_alive(RolePid) of
        true ->
            case RolePid =:= self() of
                true -> lib_daily:get_special_info(RoleId, Key);
                false -> svr_role:apply_call(RolePid, lib_daily, get_special_info, [RoleId, Key])
            end;
        false ->
            0
    end.