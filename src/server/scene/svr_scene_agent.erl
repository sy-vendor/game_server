%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 场景处理进程
%%% @end
%%% Created : 24. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_scene_agent).
-author("sy").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([start_link/1]).

-export([i/2, p/2, call/2, call/3, call/4, cast/2, cast/3, cast/4, send_after/3, send_after/4, send_after/5, apply_call/4, apply_call/5, apply_call/6,
    apply_call2/4, apply_call2/5, apply_call2/6, apply_cast/4, apply_cast/5, apply_cast/6, apply_cast2/4, apply_cast2/5, apply_cast2/6]).

-export([create_scene/3, close_scene/2, clear_scene/2, clear_scene/3, del_obj/4]).

-export([role_join/1, role_leave/1, role_move/4, role_update/2, handle_battle_msg/4, handle_battle_msg/5]).

-export([sync_create_mon/4, async_create_mon/4, get_all_scene_mon/4, get_scene_mon_by_mid/4, get_scene_mon_by_mtype_id/4, get_scene_mon_by_owner/4,
    clear_all_scene_mon/4, clear_scene_mon_by_mid/4, clear_scene_mon_by_mtype_id/4, clear_scene_mon_by_owner/4, update_mon_record_by_mtype_id/4]).

-export([sync_create_robot/4, async_create_robot/4, get_all_scene_robot/4, get_scene_robot_by_rid/4, get_scene_robot_by_rtype_id/4, clear_all_scene_robot/4,
    clear_scene_robot_by_rid/4, clear_scene_robot_by_rtype_id/4, update_robot_record_by_rtype_id/4]).

-export([send_to_area_role/2, send_to_area_role/4, send_to_local_scene/2, send_to_local_scene/4, move_broadcast/2, move_broadcast/4]).

-export([get_mon_pos/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("role.hrl").
-include("scene.hrl").
-include("battle.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 进程信息
i(SceneId, LineId) ->
    call(SceneId, LineId, info).

%% @doc 进程ID
p(SceneId, LineId) ->
    get_scene_pid(SceneId, LineId).

%% @doc call函数
call(Pid, Msg) ->
    case is_pid(Pid) of
        true -> gen_server:call(Pid, Msg);
        false -> ignore
    end.
call(SceneId, LineId, Msg) ->
    call(SceneId, LineId, ?COPY_0, Msg).
call(SceneId, LineId, CopyId, Msg) ->
    case p(SceneId, LineId) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        kfcenter ->
            case lib_kf:get_copy_node(CopyId) of
                KfNode when KfNode =/= none, KfNode =/= 0, KfNode =/= undefined ->
                    lib_kfclient:apply_call(KfNode, ?MODULE, call, [SceneId, LineId, Msg]);
                _ ->
                    ignore
            end;
        _ ->
            {error, ?ERR_COMMON_SERVICE_NOT_START}
    end.

%% @doc cast函数
cast(Pid, Msg) ->
    case is_pid(Pid) of
        true -> gen_server:cast(Pid, Msg);
        false -> ignore
    end.
cast(SceneId, LineId, Msg) ->
    cast(SceneId, LineId, ?COPY_0, Msg).
cast(SceneId, LineId, CopyId, Msg) ->
    case p(SceneId, LineId) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        kfcenter ->
            case lib_kf:get_copy_node(CopyId) of
                KfNode when KfNode =/= none, KfNode =/= 0, KfNode =/= undefined ->
                    lib_kfclient:apply_cast(KfNode, ?MODULE, cast, [SceneId, LineId, Msg]);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

%% @doc send_after函数
send_after(Pid, DelayTime, Msg) ->
    case is_pid(Pid) of
        true -> erlang:send_after(DelayTime, Pid, Msg);
        false -> ignore
    end.
send_after(SceneId, LineId, DelayTime, Msg) ->
    send_after(SceneId, LineId, ?COPY_0, DelayTime, Msg).
send_after(SceneId, LineId, CopyId, DelayTime, Msg) ->
    case p(SceneId, LineId) of
        Pid when is_pid(Pid) ->
            erlang:send_after(DelayTime, Pid, Msg);
        kfcenter ->
            case lib_kf:get_copy_node(CopyId) of
                KfNode when KfNode =/= none, KfNode =/= 0, KfNode =/= undefined ->
                    lib_kfclient:apply_cast(KfNode, ?MODULE, send_after, [SceneId, LineId, DelayTime, Msg]);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

%% @doc CALL发送场景消息接口
apply_call(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    call(SceneId, LineId, CopyId, {apply_call, [M, F, A]});
apply_call(SceneRole, M, F, A) when is_record(SceneRole, scene_role) ->
    #scene_role{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = SceneRole,
    call(SceneId, LineId, CopyId, {apply_call, [M, F, A]});
apply_call(ScenePid, M, F, A) when is_pid(ScenePid) ->
    call(ScenePid, {apply_call, [M, F, A]});
apply_call(_, _M, _F, _A) -> ignore.
apply_call(SceneId, LineId, M, F, A) ->
    call(SceneId, LineId, {apply_call, [M, F, A]}).
apply_call(SceneId, LineId, CopyId, M, F, A) ->
    call(SceneId, LineId, CopyId, {apply_call, [M, F, A]}).

%% @doc CALL发送场景消息接口（带STATE参数）
apply_call2(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    call(SceneId, LineId, CopyId, {apply_call2, [M, F, A]});
apply_call2(SceneRole, M, F, A) when is_record(SceneRole, scene_role) ->
    #scene_role{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = SceneRole,
    call(SceneId, LineId, CopyId, {apply_call2, [M, F, A]});
apply_call2(ScenePid, M, F, A) when is_pid(ScenePid) ->
    call(ScenePid, {apply_call2, [M, F, A]});
apply_call2(_, _M, _F, _A) -> ignore.
apply_call2(SceneId, LineId, M, F, A) ->
    call(SceneId, LineId, {apply_call2, [M, F, A]}).
apply_call2(SceneId, LineId, CopyId, M, F, A) ->
    call(SceneId, LineId, CopyId, {apply_call2, [M, F, A]}).

%% @doc CAST发送场景消息接口
apply_cast(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {apply_cast, [M, F, A]});
apply_cast(SceneRole, M, F, A) when is_record(SceneRole, scene_role) ->
    #scene_role{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = SceneRole,
    cast(SceneId, LineId, CopyId, {apply_cast, [M, F, A]});
apply_cast(ScenePid, M, F, A) when is_pid(ScenePid) ->
    cast(ScenePid, {apply_cast, [M, F, A]});
apply_cast(_, _M, _F, _A) -> ignore.
apply_cast(SceneId, LineId, M, F, A) ->
    cast(SceneId, LineId, {apply_cast, [M, F, A]}).
apply_cast(SceneId, LineId, CopyId, M, F, A) ->
    cast(SceneId, LineId, CopyId, {apply_cast, [M, F, A]}).

%% @doc CAST发送场景消息接口（带STATE参数）
apply_cast2(RoleState, M, F, A) when is_record(RoleState, role_state) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {apply_cast2, [M, F, A]});
apply_cast2(SceneRole, M, F, A) when is_record(SceneRole, scene_role) ->
    #scene_role{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = SceneRole,
    cast(SceneId, LineId, CopyId, {apply_cast2, [M, F, A]});
apply_cast2(ScenePid, M, F, A) when is_pid(ScenePid) ->
    cast(ScenePid, {apply_cast2, [M, F, A]});
apply_cast2(_, _M, _F, _A) -> ignore.
apply_cast2(SceneId, LineId, M, F, A) ->
    cast(SceneId, LineId, {apply_cast2, [M, F, A]}).
apply_cast2(SceneId, LineId, CopyId, M, F, A) ->
    cast(SceneId, LineId, CopyId, {apply_cast2, [M, F, A]}).

%% -----------------------------------------------------------------------------
%% 场景数据相关处理
%% -----------------------------------------------------------------------------

%% @doc 创建新场景
create_scene(SceneId, LineId, ExtraArgs) ->
    case conf_scene:get(SceneId) of
        [] ->
            undefined;
        Scene when is_record(Scene, scene) ->
            {ok, NewScenePid} = sup_scene_mgr:start_child([Scene, LineId, ExtraArgs]),
            NewScenePid
    end.

%% @doc 关闭场景
close_scene(SceneId, LineId) ->
    cast(SceneId, LineId, {close_scene}).

%% @doc 清理场景资源
clear_scene(SceneId, LineId) ->
    clear_scene(SceneId, LineId, []).
clear_scene(SceneId, LineId, CopyId) ->
    cast(SceneId, LineId, CopyId, {clear_scene, CopyId}).

%% @doc 删除场景主体
del_obj(SceneId, LineId, CopyId, ObjArgs) ->
    cast(SceneId, LineId, CopyId, {del_obj, ObjArgs}).

%% -----------------------------------------------------------------------------
%% 玩家操作相关处理
%% -----------------------------------------------------------------------------

%% @doc 玩家加入场景
role_join(RoleState) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    SceneRole = lib_scene_role:make_scene_role(RoleState),
    cast(SceneId, LineId, CopyId, {role_join, [SceneRole]}).

%% @doc 玩家离开场景
role_leave(RoleState) ->
    #role_state{role_id = RoleId, platform = Platform, server_num = ServerNum, scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {role_leave, [[RoleId, Platform, ServerNum]]}).

%% @doc 玩家场景移动
role_move(ToX, ToY, MoveType, RoleState) ->
    #role_state{role_id = RoleId, platform = Platform, server_num = ServerNum, scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {role_move, [[RoleId, Platform, ServerNum], ToX, ToY, MoveType]}).

%% @doc 玩家更新场景属性
role_update(Args, RoleState) ->
    #role_state{role_id = RoleId, platform = Platform, server_num = ServerNum, scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {role_update, [Args, [RoleId, Platform, ServerNum]]}).

%% @doc 处理战斗消息
handle_battle_msg(SceneId, LineId, CopyId, BattleMsg) ->
    cast(SceneId, LineId, CopyId, {handle_battle_msg, BattleMsg}).

%% @doc 处理战斗消息
handle_battle_msg(SceneId, LineId, CopyId, BattleMsg, NowTimeMs) ->
    cast(SceneId, LineId, CopyId, {handle_battle_msg, BattleMsg, NowTimeMs}).

%% -----------------------------------------------------------------------------
%% 怪物操作相关处理
%% -----------------------------------------------------------------------------

%% @doc 同步创建怪物列表
sync_create_mon(SceneId, LineId, CopyId, MonArgs) ->
    call(SceneId, LineId, CopyId, {sync_create_mon, MonArgs}).

%% @doc 异步创建怪物列表
async_create_mon(SceneId, LineId, CopyId, MonArgs) ->
    cast(SceneId, LineId, CopyId, {async_create_mon, MonArgs}).

%% @doc 获取场景中所有怪物
get_all_scene_mon(SceneId, LineId, CopyId, MonArgs) ->
    call(SceneId, LineId, CopyId, {get_all_scene_mon, MonArgs}).

%% @doc 获取怪物(通过怪物唯一ID)
get_scene_mon_by_mid(SceneId, LineId, CopyId, MonArgs) ->
    call(SceneId, LineId, CopyId, {get_scene_mon_by_mid, MonArgs}).

%% @doc 获取怪物(通过怪物类型ID)
get_scene_mon_by_mtype_id(SceneId, LineId, CopyId, MonArgs) ->
    call(SceneId, LineId, CopyId, {get_scene_mon_by_mtype_id, MonArgs}).

%% @doc 获取怪物(通过归属玩家KEY)
get_scene_mon_by_owner(SceneId, LineId, CopyId, MonArgs) ->
    call(SceneId, LineId, CopyId, {get_scene_mon_by_owner, MonArgs}).

%% @doc 清理场景所有怪物
clear_all_scene_mon(SceneId, LineId, CopyId, MonArgs) ->
    cast(SceneId, LineId, CopyId, {clear_all_scene_mon, MonArgs}).

%% @doc 清理怪物(通过怪物唯一ID)
clear_scene_mon_by_mid(SceneId, LineId, CopyId, MonArgs) ->
    cast(SceneId, LineId, CopyId, {clear_scene_mon_by_mid, MonArgs}).

%% @doc 清理怪物(通过怪物类型ID)
clear_scene_mon_by_mtype_id(SceneId, LineId, CopyId, MonArgs) ->
    cast(SceneId, LineId, CopyId, {clear_scene_mon_by_mtype_id, MonArgs}).

%% @doc 清理怪物(通过归属玩家KEY)
clear_scene_mon_by_owner(SceneId, LineId, CopyId, MonArgs) ->
    cast(SceneId, LineId, CopyId, {clear_scene_mon_by_owner, MonArgs}).

%% @doc 更新怪物字段信息
update_mon_record_by_mtype_id(SceneId, LineId, CopyId, MonArgs) ->
    cast(SceneId, LineId, CopyId, {update_mon_record_by_mtype_id, MonArgs}).

%% -----------------------------------------------------------------------------
%% 机器人操作相关处理
%% -----------------------------------------------------------------------------

%% @doc 同步创建机器人列表
sync_create_robot(SceneId, LineId, CopyId, RobotArgs) ->
    call(SceneId, LineId, CopyId, {sync_create_robot, RobotArgs}).

%% @doc 异步创建机器人列表
async_create_robot(SceneId, LineId, CopyId, RobotArgs) ->
    cast(SceneId, LineId, CopyId, {async_create_robot, RobotArgs}).

%% @doc 获取场景中所有机器人
get_all_scene_robot(SceneId, LineId, CopyId, RobotArgs) ->
    call(SceneId, LineId, CopyId, {get_all_scene_robot, RobotArgs}).

%% @doc 获取机器人(通过机器人唯一ID)
get_scene_robot_by_rid(SceneId, LineId, CopyId, RobotArgs) ->
    call(SceneId, LineId, CopyId, {get_scene_robot_by_rid, RobotArgs}).

%% @doc 获取机器人(通过机器人类型ID)
get_scene_robot_by_rtype_id(SceneId, LineId, CopyId, RobotArgs) ->
    call(SceneId, LineId, CopyId, {get_scene_robot_by_rtype_id, RobotArgs}).

%% @doc 清理场景所有机器人
clear_all_scene_robot(SceneId, LineId, CopyId, RobotArgs) ->
    cast(SceneId, LineId, CopyId, {clear_all_scene_robot, RobotArgs}).

%% @doc 清理机器人(通过机器人唯一ID)
clear_scene_robot_by_rid(SceneId, LineId, CopyId, RobotArgs) ->
    cast(SceneId, LineId, CopyId, {clear_scene_robot_by_rid, RobotArgs}).

%% @doc 清理机器人(通过机器人类型ID)
clear_scene_robot_by_rtype_id(SceneId, LineId, CopyId, RobotArgs) ->
    cast(SceneId, LineId, CopyId, {clear_scene_robot_by_rtype_id, RobotArgs}).

%% @doc 更新机器人字段信息
update_robot_record_by_rtype_id(SceneId, LineId, CopyId, RobotArgs) ->
    cast(SceneId, LineId, CopyId, {update_robot_record_by_rtype_id, RobotArgs}).

%% -----------------------------------------------------------------------------
%% 消息广播相关处理
%% -----------------------------------------------------------------------------

%% @doc 全场景广播
send_to_local_scene(RoleState, MsgArgs) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {send_to_local_scene, MsgArgs}).
send_to_local_scene(SceneId, LineId, CopyId, MsgArgs) ->
    cast(SceneId, LineId, CopyId, {send_to_local_scene, MsgArgs}).

%% @doc 九宫格广播
send_to_area_role(RoleState, MsgArgs) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {send_to_area_role, MsgArgs}).
send_to_area_role(SceneId, LineId, CopyId, MsgArgs) ->
    cast(SceneId, LineId, CopyId, {send_to_area_role, MsgArgs}).

%% @doc 移动九宫格广播
move_broadcast(RoleState, MsgArgs) ->
    #role_state{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = RoleState,
    cast(SceneId, LineId, CopyId, {move_broadcast, MsgArgs}).
move_broadcast(SceneId, LineId, CopyId, MsgArgs) ->
    cast(SceneId, LineId, CopyId, {move_broadcast, MsgArgs}).

%% -----------------------------------------------------------------------------
%% 各功能相关处理
%% -----------------------------------------------------------------------------
%% @doc 获取怪物位置
get_mon_pos(SceneId, LineId, CopyId, MsgArgs) ->
    cast(SceneId, LineId, CopyId, {get_mon_pos, MsgArgs}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 创建场景进程
start_link([Scene, LineId, ExtraArgs]) ->
    #scene{scene_id = SceneId} = Scene,
    RegName = dist:scene_process_name(SceneId, LineId),
    gen_server:start({local, RegName}, ?MODULE, [Scene, LineId, ExtraArgs], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Scene, LineId, ExtraArgs]) ->
    process_flag(trap_exit, true),
    % 加载场景动态信息
    erlang:put(?SCENE_OBJ_ID, 1),
    SceneN0 = lib_scene_mgr:init_scene_timer(Scene),
    CopyId = ?iif(util:is_kf_server() =:= true, {node(), ?COPY_0}, ?COPY_0),
    SceneN = lib_scene_mgr:copy_scene(SceneN0, LineId, CopyId, ExtraArgs, 1),
    {ok, SceneN}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% CALL到进程执行函数
handle_call({apply_call, [Module, Fun, Args]}, _From, State) ->
    Res = case catch apply(Module, Fun, Args) of
              {'EXIT', Info} ->
                  ?ERROR_MSG("svr_scene_agent apply_call error ~p~n", [[Module, Fun, Args, Info]]),
                  {error, Info};
              Ret ->
                  Ret
          end,
    {reply, Res, State};

%% CALL到进程执行函数（带STATE参数）
handle_call({apply_call2, [Module, Fun, Args]}, _From, State) ->
    {Res, NewState} = case catch apply(Module, Fun, Args ++ [State]) of
                          {'EXIT', Info} ->
                              ?ERROR_MSG("svr_scene_agent apply_call2 error ~p~n", [[Module, Fun, Args, Info]]),
                              {error, State};
                          NewStateT when is_record(NewStateT, scene) ->
                              {ok, NewStateT};
                          {ok, NewStateT} when is_record(NewStateT, scene) ->
                              {ok, NewStateT};
                          {Ret, NewStateT} when is_record(NewStateT, scene) ->
                              {Ret, NewStateT};
                          {ok, Ret, NewStateT} when is_record(NewStateT, scene) ->
                              {Ret, NewStateT};
                          ok ->
                              {ok, State};
                          _ ->
                              {ignore, State}
                      end,
    {reply, Res, NewState};

%% -----------------------------------------------------------------------------
%% 怪物操作相关
%% -----------------------------------------------------------------------------
%% 异步创建怪物列表
handle_call({sync_create_mon, [MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args]}, _From, State) ->
    Res = lib_mon_agent:create_mon(MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args),
    {reply, Res, State};

%% 获取场景中所有怪物
handle_call({get_all_scene_mon, [CopyId, RetForm]}, _From, State) ->
    Res = lib_scene_mon:get_all_scene_mon(CopyId, RetForm),
    {reply, Res, State};

%% 获取怪物(通过怪物唯一ID)
handle_call({get_scene_mon_by_mid, [CopyId, MonIdList, RetForm]}, _From, State) ->
    Res = lib_scene_mon:get_scene_mon_by_mid(CopyId, MonIdList, RetForm),
    {reply, Res, State};

%% 获取怪物(通过怪物类型ID)
handle_call({get_scene_mon_by_mtype_id, [CopyId, MTypeIdList, RetForm]}, _From, State) ->
    Res = lib_scene_mon:get_scene_mon_by_mtype_id(CopyId, MTypeIdList, RetForm),
    {reply, Res, State};

%% 获取怪物(通过归属玩家KEY)
handle_call({get_scene_mon_by_owner, [CopyId, RoleKey, RetForm]}, _From, State) ->
    Res = lib_scene_mon:get_scene_mon_by_owner(CopyId, RoleKey, RetForm),
    {reply, Res, State};

%% -----------------------------------------------------------------------------
%% 机器人操作相关
%% -----------------------------------------------------------------------------
%% 异步创建机器人列表
handle_call({sync_create_robot, [MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args]}, _From, State) ->
    Res = lib_robot_agent:create_robot(MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args),
    {reply, Res, State};

%% 获取场景中所有机器人
handle_call({get_all_scene_robot, [CopyId, RetForm]}, _From, State) ->
    Res = lib_scene_robot:get_all_scene_robot(CopyId, RetForm),
    {reply, Res, State};

%% 获取机器人(通过机器人唯一ID)
handle_call({get_scene_robot_by_rid, [CopyId, RobotIdList, RetForm]}, _From, State) ->
    Res = lib_scene_robot:get_scene_robot_by_rid(CopyId, RobotIdList, RetForm),
    {reply, Res, State};

%% 获取机器人(通过机器人类型ID)
handle_call({get_scene_robot_by_rtype_id, [CopyId, RTypeIdList, RetForm]}, _From, State) ->
    Res = lib_scene_robot:get_scene_robot_by_rtype_id(CopyId, RTypeIdList, RetForm),
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% -----------------------------------------------------------------------------
%% 工具调用函数
%% -----------------------------------------------------------------------------
%% CAST到进程执行函数
handle_cast({apply_cast, [Module, Fun, Args]}, State) ->
    case catch apply(Module, Fun, Args) of
        {'EXIT', Info} ->
            ?ERROR_MSG("svr_scene_agent apply_cast error ~p~n", [[Module, Fun, Args, Info]]);
        _ ->
            ignore
    end,
    {noreply, State};

%% CAST到进程执行函数（带STATE参数）
handle_cast({apply_cast2, [Module, Fun, Args]}, State) ->
    NewState = case catch apply(Module, Fun, Args ++ [State]) of
                   {'EXIT', Info} ->
                       ?ERROR_MSG("svr_scene_agent apply_cast2 error ~p~n", [[Module, Fun, Args, Info]]),
                       State;
                   NewStateT when is_record(NewStateT, scene) ->
                       NewStateT;
                   {_, NewStateT} when is_record(NewStateT, scene) ->
                       NewStateT;
                   _ ->
                       State
               end,
    {noreply, NewState};

%% -----------------------------------------------------------------------------
%% 场景数据相关
%% -----------------------------------------------------------------------------
%% 发送场景待处理消息
handle_cast({push_cmd, Msg}, State) ->
    lib_scene_agent:push_cmd(Msg),
    {noreply, State};

%% 场景数据定时更新处理
handle_cast({update_timer}, #scene{update_timer = UpdateTimer, last_update_time = LastUpdateTime} = State) when UpdateTimer =/= [] ->
    NowTimeMs = time:unixtime_ms(),
    Interval = NowTimeMs - LastUpdateTime,
    % 更新玩家列表
    RoleList = lib_scene_agent:get_keys(?BATTLE_ROLE),
    lib_scene_agent:update_obj(?BATTLE_ROLE, RoleList, NowTimeMs, Interval),
    % 更新怪物列表
    MonList = lib_scene_agent:get_keys(?BATTLE_MON),
    lib_scene_agent:update_obj(?BATTLE_MON, MonList, NowTimeMs, Interval),
    % 重置定时器
    NewUpdateTimer = erlang:send_after(?SCENE_UPDATE_OBJ_INTERVAL, self(), {update_timer}),
    NewState = State#scene{update_timer = NewUpdateTimer, last_update_time = NowTimeMs},
    {noreply, NewState};

%% 场景消息定时处理
handle_cast({handle_timer}, #scene{handle_timer = HandleTimer} = State) when HandleTimer =/= [] ->
    NowTimeMs = time:unixtime_ms(),
    % 处理CMD消息
    CmdQueue = lists:reverse(erlang:get(?SCENE_HANDLE_CMD)),
    erlang:put(?SCENE_HANDLE_CMD, []),
    lib_scene_agent:handle_cmd(CmdQueue, NowTimeMs),
    % 处理DELAY消息
    DelayQueue = erlang:get(?SCENE_HANDLE_DELAY),
    {UpdateList, WaitList} = lists:partition(fun({Time, _, _}) -> Time =< NowTimeMs end, DelayQueue),
    lib_scene_agent:handle_delay(lists:reverse(UpdateList), NowTimeMs),
    erlang:put(?SCENE_HANDLE_DELAY, WaitList),
    % 重置定时器
    NewHandleTimer = erlang:send_after(?SCENE_HANDLE_CMD_INTERVAL, self(), {handle_timer}),
    NewState = State#scene{handle_timer = NewHandleTimer, last_handle_time = NowTimeMs},
    {noreply, NewState};

%% 关闭场景
handle_cast({close_scene}, State) ->
    #scene{handle_timer = HandleTimer, update_timer = UpdateTimer} = State,
    % 删除怪物
    catch lib_scene_mon:clear_all_scene_mon([], 1),
    catch lib_scene_robot:clear_all_scene_robot([], 1),
    % 清理进程字典数据
    lib_scene_agent:clear_all_process_dict(),
    % 清理定时器
    HandleTimerN = util:cancel_timer(HandleTimer),
    UpdateTimerN = util:cancel_timer(UpdateTimer),
    {stop, normal, State#scene{handle_timer = HandleTimerN, update_timer = UpdateTimerN}};

%% 清理场景资源
handle_cast({clear_scene, CopyId}, State) ->
    % 删除怪物
    catch lib_scene_mon:clear_all_scene_mon(CopyId, 1),
    catch lib_scene_robot:clear_all_scene_robot(CopyId, 1),
    % 清除九宫格怪物
    lib_scene_agent:del_all_area(?BATTLE_MON, CopyId),
    {noreply, State};

%% 删除场景主体
handle_cast({del_obj, [ObjKey]}, State) ->
    lib_scene_agent:del_obj(ObjKey),
    {noreply, State};

%% -----------------------------------------------------------------------------
%% 玩家操作相关
%% -----------------------------------------------------------------------------
%% 玩家进入场景
handle_cast({role_join, [SceneRole]}, State) ->
    % 初始化状态机数据
    StateList = [{?FSM_IDLE, lib_fsm_role_idle}, {?FSM_MOVE, lib_fsm_role_move}, {?FSM_ATTACK, lib_fsm_role_attack}, {?FSM_COLLECT, lib_fsm_role_collect}, {?FSM_DIE, lib_fsm_role_die}],
    {ok, SceneRoleN0} = lib_fsm:init_state_dict(StateList, #scene_role.fsm_info, SceneRole),
    SceneRoleN1 = lib_fsm:change(?FSM_IDLE, [], #scene_role.fsm_info, SceneRoleN0),
    #scene_role{scene_id = SceneId, line_id = LineId} = SceneRoleN1,

    %% TODO 后续根据需求操作补全 并广播场景所有玩家
    SceneRoleN = SceneRoleN1,

    % 保存玩家数据
    lib_scene_agent:save_obj(SceneRoleN),
    % 增加场景人数
    lib_scene_api:add_line_role(SceneId, LineId),
    {noreply, State};

%% 玩家离开场景
handle_cast({role_leave, [RoleKey]}, State) ->
    case lib_scene_agent:get_obj(RoleKey) of
        [] ->
            {noreply, State};
        SceneRole when is_record(SceneRole, scene_role)  ->
            % 删除玩家场景数据
            lib_scene_agent:del_obj(RoleKey),
            #scene_role{scene_id = SceneId, line_id = LineId, copy_id = CopyId} = SceneRole,
            %% TODO 后续根据需求操作补全 并广播场景所有玩家

            % 删除场景人数
            lib_scene_api:del_line_role(SceneId, LineId),
            % 清除场景所属怪物
            lib_scene_mon:clear_scene_mon_by_owner(CopyId, RoleKey, 1),
            % 清理战斗信息
            lib_battle_api:clear_battle_msg(?BATTLE_ROLE, RoleKey),
            {noreply, State}
    end;

%% 玩家场景移动
handle_cast({role_move, [RoleKey, ToX, ToY, _MoveType]}, State) ->
    case lib_scene_agent:get_obj(RoleKey) of
        [] ->
            {noreply, State};
        SceneRole when is_record(SceneRole, scene_role)  ->
            %%  TODO 场景玩家改变 需广播相关协议

            % 保存玩家数据
            SceneRoleN = SceneRole#scene_role{x = ToX, y = ToY},
            lib_scene_agent:save_obj(SceneRoleN),
            {noreply, State}
    end;

%% 玩家场景属性更新操作
handle_cast({role_update, [Args, RoleKey]}, State) ->
    lib_scene_role:update_info(Args, RoleKey),
    {noreply, State};

%% 处理战斗消息
handle_cast({handle_battle_msg, [Msg, Args]}, State) ->
    lib_battle_api:handle_battle_msg(Msg, Args),
    {noreply, State};

%% 处理战斗消息
handle_cast({handle_battle_msg, [Msg, Args], NowTimeMs}, State) ->
    lib_battle_api:handle_battle_msg(Msg, Args, NowTimeMs),
    {noreply, State};

%% -----------------------------------------------------------------------------
%% 怪物操作相关
%% -----------------------------------------------------------------------------
%% 异步创建怪物列表
handle_cast({async_create_mon, [MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args]}, State) ->
    lib_mon_agent:create_mon(MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args),
    {noreply, State};

%% 清理场景所有怪物
handle_cast({clear_all_scene_mon, [CopyId, IsBroadCast]}, State) ->
    lib_scene_mon:clear_all_scene_mon(CopyId, IsBroadCast),
    {noreply, State};

%% 清理怪物(通过怪物唯一ID)
handle_cast({clear_scene_mon_by_mid, [CopyId, MonIdList, IsBroadCast]}, State) ->
    lib_scene_mon:clear_scene_mon_by_mid(CopyId, MonIdList, IsBroadCast),
    {noreply, State};

%% 清理怪物(通过怪物类型ID)
handle_cast({clear_scene_mon_by_mtype_id, [CopyId, MTypeIdList, IsBroadCast]}, State) ->
    lib_scene_mon:clear_scene_mon_by_mtype_id(CopyId, MTypeIdList, IsBroadCast),
    {noreply, State};

%% 清理怪物(通过归属玩家KEY)
handle_cast({clear_scene_mon_by_owner, [CopyId, RoleKey, IsBroadCast]}, State) ->
    lib_scene_mon:clear_scene_mon_by_owner(CopyId, RoleKey, IsBroadCast),
    {noreply, State};

%% 更新怪物字段信息
handle_cast({update_mon_record_by_mtype_id, [CopyId, MTypeIdList, Args]}, State) ->
    lib_scene_mon:update_mon_record_by_mtype_id(CopyId, MTypeIdList, Args),
    {noreply, State};

%% -----------------------------------------------------------------------------
%% 机器人操作相关
%% -----------------------------------------------------------------------------
%% 异步创建机器人列表
handle_cast({async_create_robot, [MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args]}, State) ->
    lib_robot_agent:create_robot(MonList, SceneId, LineId, CopyId, IsBroadCast, CreateTime, Args),
    {noreply, State};

%% 清理场景所有机器人
handle_cast({clear_all_scene_robot, [CopyId, IsBroadCast]}, State) ->
    lib_scene_robot:clear_all_scene_robot(CopyId, IsBroadCast),
    {noreply, State};

%% 清理机器人(通过机器人唯一ID)
handle_cast({clear_scene_robot_by_rid, [CopyId, MonIdList, IsBroadCast]}, State) ->
    lib_scene_robot:clear_scene_robot_by_rid(CopyId, MonIdList, IsBroadCast),
    {noreply, State};

%% 清理机器人(通过机器人类型ID)
handle_cast({clear_scene_robot_by_rtype_id, [CopyId, MTypeIdList, IsBroadCast]}, State) ->
    lib_scene_robot:clear_scene_robot_by_rtype_id(CopyId, MTypeIdList, IsBroadCast),
    {noreply, State};

%% 更新机器人字段信息
handle_cast({update_robot_record_by_rtype_id, [CopyId, MTypeIdList, Args]}, State) ->
    lib_scene_robot:update_robot_record_by_rtype_id(CopyId, MTypeIdList, Args),
    {noreply, State};

%% -----------------------------------------------------------------------------
%% 消息广播相关
%% -----------------------------------------------------------------------------
%% 全场景广播
handle_cast({send_to_local_scene, [CopyId, BinData]}, State) ->
    lib_scene_agent:send_to_local_scene(CopyId, BinData),
    {noreply, State};

%% 全场景广播
%% 注意：1、包含等级限制
%%       2、排除自己
handle_cast({send_to_local_scene, [LvLim, ExceptList, CopyId, BinData]}, State) ->
    lib_scene_agent:send_to_local_scene(LvLim, ExceptList, CopyId, BinData),
    {noreply, State};

%% 九宫格广播
handle_cast({send_to_area_role, [CopyId, X, Y, BinData]}, State) ->
    lib_scene_agent:send_to_area_role(CopyId, X, Y, BinData),
    {noreply, State};

%% 移动九宫格广播
handle_cast({move_broadcast, [CopyId, X1, Y1, X2, Y2, BinData, BinData1, BinData2, SendList]}, State) ->
    lib_scene_map:move_broadcast(CopyId, X1, Y1, X2, Y2, BinData, BinData1, BinData2, SendList),
    {noreply, State};

%% -----------------------------------------------------------------------------
%% 各功能相关
%% -----------------------------------------------------------------------------
%% 获取怪物位置
handle_cast({get_mon_pos, [RoleNode, RoleId, SceneId, CopyId]}, State) ->
    lib_mon_agent:get_mon_pos(RoleNode, RoleId, SceneId, CopyId),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({update_timer} = Info, State) ->
    try
        handle_cast(Info, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
            % 重置定时器
            NowTimeMs = time:unixtime_ms(),
            NewUpdateTimer = erlang:send_after(?SCENE_UPDATE_OBJ_INTERVAL, self(), {update_timer}),
            NewState = State#scene{update_timer = NewUpdateTimer, last_update_time = NowTimeMs},
            {noreply, NewState}
    end;
handle_info({handle_timer} = Info, State) ->
    try
        handle_cast(Info, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
            % 重置定时器
            NowTimeMs = time:unixtime_ms(),
            NewHandleTimer = erlang:send_after(?SCENE_HANDLE_CMD_INTERVAL, self(), {handle_timer}),
            NewState = State#scene{handle_timer = NewHandleTimer, last_handle_time = NowTimeMs},
            {noreply, NewState}
    end;
handle_info(Info, State) ->
    try
        handle_cast(Info, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
    #scene{scene_id = SceneId, line_id = LineId} = State,
    unregist_scene_pid(SceneId, LineId),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 获取场景进程PID
get_scene_pid(SceneId, LineId) ->
    dist:get_scene_process_pid(SceneId, LineId).

%% 删除场景进程PID
unregist_scene_pid(SceneId, LineId) ->
    RegName = dist:scene_process_name(SceneId, LineId),
    dist:unregister(local, RegName).