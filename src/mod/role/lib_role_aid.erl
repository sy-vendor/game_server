%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 玩家相关函数
%%% @end
%%% Created : 19. 十二月 2018 15:55
%%%-------------------------------------------------------------------
-module(lib_role_aid).
-author("suyang").

%% API
-export([is_online/1,
    is_online2/1]).

-include("common.hrl").
-include("role.hrl").
-include("ets.hrl").
-include("logic.hrl").


%% @doc 检测某个玩家是否在线
is_online(RoleId) when is_integer(RoleId) ->
    case ets:lookup(?ETS_ONLINE, RoleId) of
        [] -> false;
        _ -> true
    end;
is_online(_) -> false.

%% @doc 检测某个玩家是否在线(返回pid)
is_online2(RoleId) when is_integer(RoleId) ->
    case ets:lookup(?ETS_ONLINE, RoleId) of
        [] ->
            undefined;
        _ ->
            case util_dist:get_role_process_pid(RoleId) of
                RolePid when is_pid(RolePid) ->
                    RolePid;
                _ ->
                    undefined
            end
    end;
is_online2(_) -> undefined.