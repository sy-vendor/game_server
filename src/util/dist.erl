%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 进程标识处理函数
%%% @end
%%% Created : 14. 五月 2019
%%%-------------------------------------------------------------------
-module(dist).
-include("common.hrl").
-include("scene.hrl").

-export([
    whereis_name/2,
    register/3,
    unregister/2,
    is_process_alive/1,
    role_process_name/1,
    get_role_process_pid/1,
    role_send_process_name/1,
    get_role_send_process_pid/1,
    scene_process_name/2,
    is_scene_process_alive/2,
    mark_process_name/1,
    get_child_count/1,
    pg2_get_members/1,
    get_scene_process_pid/2,
    get_scene_process_pid/3
]).

%% @doc 获取进程ID
whereis_name(local, Atom) ->
    erlang:whereis(Atom);
whereis_name(global, Term) ->
    global:whereis_name(Term).

%% @doc 注册进程ID
register(local, Name, Pid) ->
    erlang:register(Name, Pid);
register(global, Name, Pid) ->
    global:re_register_name(Name, Pid).

%% @doc 解绑进程ID
unregister(local, Name) ->
    erlang:unregister(Name);
unregister(global, Name) ->
    global:unregister_name(Name).

%% @doc 判断进程是否存活
is_process_alive(Pid) ->
    try
        if
            is_pid(Pid) ->
                case node(Pid) =:= node() of
                    true ->
                        erlang:is_process_alive(Pid);
                    false ->
                        case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                            {badrpc, _Reason}  -> false;
                            Res -> Res
                        end
                end;
            true -> false
        end
    catch
        _:_ -> false
    end.

%% @doc 获取玩家进程名称
role_process_name(RoleId) ->
    {svr_role, RoleId}.

%% @doc 获取玩家进程ID
get_role_process_pid(RoleId) ->
    RegName = role_process_name(RoleId),
    dist:whereis_name(global, RegName).

%% @doc 获取玩家消息进程名称
role_send_process_name(RoleId) ->
    {mls, RoleId}.

%% @doc 获取玩家消息进程ID
get_role_send_process_pid(RoleId) ->
    RegName = role_send_process_name(RoleId),
    dist:whereis_name(global, RegName).

%% @doc 获取场景进程名称
scene_process_name(SceneId, LineId) ->
    type:list_to_atom(lists:concat(["svr_scene_agent_", SceneId, "_", LineId])).

%% @doc 判断场景进程是否存活
is_scene_process_alive(SceneId, LineId) ->
    RegName = dist:scene_process_name(SceneId, LineId),
    dist:whereis_name(local, RegName).

%% @doc 场景区域信息进程名称
mark_process_name(SceneId) ->
    type:list_to_atom(lists:concat(["mark_process_name_", SceneId])).

%% @doc 获取子进程数量
get_child_count(Atom) ->
    case whereis_name(local, Atom) of
        undefined ->
            0;
        _ ->
            [_,{active, ChildCount},_,_] = supervisor:count_children(Atom),
            ChildCount
    end.

pg2_get_members(Pg2_name) ->
    L = case pg2:get_members(Pg2_name) of
            {error, _} ->
                timer:sleep(100),
                pg2:get_members(Pg2_name);
            Other when is_list(Other) ->
                Other
        end,
    if  not is_list(L) -> [];
        true -> lists:usort(L)
    end.

%% @doc 获取场景进程ID
get_scene_process_pid(SceneId, LineId) ->
    get_scene_process_pid(SceneId, LineId, []).
get_scene_process_pid(SceneId, LineId, ExtraArgs) ->
    RegName = dist:scene_process_name(SceneId, LineId),
    case dist:whereis_name(local, RegName) of
        Pid when is_pid(Pid) ->
            case dist:is_process_alive(Pid) of
                true -> Pid;
                false ->
                    dist:unregister(local, RegName),
                    exit(Pid, kill),
                    svr_scene_mgr:create_scene(SceneId, LineId, ExtraArgs)
            end;
        _ ->
            case conf_scene:get(SceneId) of
                [] ->
                    undefined;
                Scene when is_record(Scene, scene) ->
                    #scene{type = SceneType} = Scene,
                    IsNeedCreate = case SceneType of
                                       ?SCENE_KF -> ?iif(util:is_kf_server(), true, kfcenter);
                                       _ -> true
                                   end,
                    case IsNeedCreate of
                        true -> svr_scene_mgr:create_scene(SceneId, LineId, ExtraArgs);
                        kfcenter -> kfcenter
                    end
            end
    end.



