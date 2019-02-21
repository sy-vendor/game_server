%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%  玩家数据对外接口
%%% @end
%%% Created : 19. 十二月 2018 17:12
%%%-------------------------------------------------------------------
-module(lib_data_api).
-author("suyang").

%% API
-export([
    get_value/2,     % 查询
    get_value/3,     % 查询
    put_value/3,     % 保存
    add_value/3,     % 增值
    save/1           % 数据持久化接口
]).

-include("role.hrl").
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @spec 查询
%% Role:        玩家数据
%% Key:         键
%% @return:     值
%% @end
get_value(Role, Key) when is_record(Role, role) ->
    get_value(Role#role.role_id, Key);
get_value(RoleId, Key) when is_integer(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case RolePid =:= self() of
        true -> lib_data:get_from_dict(RoleId, Key);
        false ->
            case util_dist:is_process_alive(RolePid) of
                true -> svr_role:apply_call(RolePid, lib_data, get_from_dict, [RoleId, Key]);
                false -> lib_data:get_from_db(RoleId, Key)
            end
    end.
%% 查询(带默认值)
get_value(Role, Key, Default) ->
    case get_value(Role, Key) of
        undefined -> Default;
        Data -> Data
    end.

%% @spec 保存
%% Role:        玩家数据
%% Key:         键
%% Value:       值
%% @return:     ok
%% @end
put_value(Role, Key, Value) when is_record(Role, role) ->
    put_value(Role#role.role_id, Key, Value);
put_value(RoleId, Key, Value) when is_integer(RoleId) ->
    case catch put_value2(RoleId, Key, Value) of
        ok -> ok;
        Error -> ?ERROR_MSG("lib_data_api put_value Error:~p ~n", [Error])
    end;
put_value(_, _, _) -> ok.
put_value2(RoleId, Key, Value) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case RolePid =:= self() of
        true -> lib_data:put_to_dict(RoleId, Key, Value);
        false ->
            case util_dist:is_process_alive(RolePid) of
                true -> svr_role:apply_cast(RolePid, lib_data, put_to_dict, [RoleId, Key, Value]);
                false -> lib_data:put_to_db(RoleId, Key, Value)
            end
    end,
    ok.

%% @spec 增值
%% Role:        玩家数据
%% Key:         键
%% Value:       值
%% @return:     ok
%% @end
add_value(Role, Key, Value) when is_record(Role, role) ->
    add_value(Role#role.role_id, Key, Value);
add_value(RoleId, Key, Value) when is_integer(RoleId) ->
    case catch add_value2(RoleId, Key, Value) of
        ok -> ok;
        Error -> ?ERROR_MSG("lib_data_api add_value Error:~p ~n", [Error])
    end;
add_value(_, _, _) -> ok.
add_value2(RoleId, Key, Value) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case RolePid =:= self() of
        true -> lib_data:add_to_dict(RoleId, Key, Value);
        false ->
            case util_dist:is_process_alive(RolePid) of
                true -> svr_role:apply_cast(RolePid, lib_data, add_to_dict, [RoleId, Key, Value]);
                false -> lib_data:add_to_db(RoleId, Key, Value)
            end
    end,
    ok.

%% @spec 数据持久化接口
%% Role:        玩家数据
%% @end
save(Role) when is_record(Role, role) ->
    save(Role#role.role_id);
save(RoleId) when is_integer(RoleId) ->
    case catch save2(RoleId) of
        ok -> ok;
        Error -> ?ERROR_MSG("lib_data_api save Error:~p ~n", [Error])
    end;
save(_) -> ok.
save2(RoleId) ->
    RolePid = util_dist:get_role_process_pid(RoleId),
    case RolePid =:= self() of
        true -> lib_data:data_save(RoleId);
        false ->
            case util_dist:is_process_alive(RolePid) of
                true -> svr_role:apply_cast(RolePid, lib_data, data_save, [RoleId]);
                false -> ok
            end
    end,
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
