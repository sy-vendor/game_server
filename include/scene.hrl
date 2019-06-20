%%-------------------------------------------------------
%% @File     : scene.hrl
%% @Brief    : 场景头文件
%% @Author   : sy
%% @Date     : 2019-05-24
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__SCENE_H__).
-define(__SCENE_H__, 0).

-include("fsm.hrl").

%% 场景数据标识
-define(SCENE_ROLE(Key), {scene_role, Key}).
-define(SCENE_MON(Key), {scene_mon, Key}).
-define(SCENE_OBJ_KEY(ObjType, CopyId), {scene_key, ObjType, CopyId}).


%% 九宫格数据保存
-define(SCENE_TABLE_AREA(ObjType, Id, CopyId), {scene_area, ObjType, Id, CopyId}).

%% 场景分线管理
-define(SCENE_MGR, scene_mgr).

%% 场景COPY初始值
-define(COPY_0, 0).
%% 场景LINE初始值
-define(LINE_1, 1).

%% 场景类型定义
%% 场景类型定义
-define(SCENE_NEW, 0).               %% 新手场景
-define(SCENE_OUTSIDE, 1).           %% 野外场景
-define(SCENE_KF, 3).                %% 跨服场景

%% 场景NPC数据ETS
-define(ETS_SCENE_NPC, ets_scene_npc).

%% 场景定时更新数据列表定义
-define(SCENE_UPDATE_MON, scene_update_mon_list).
-define(SCENE_UPDATE_ROLE, scene_update_role_list).
-define(SCENE_HANDLE_CMD, scene_handle_cmd_queue).
-define(SCENE_HANDLE_DELAY, scene_handle_delay_queue).

%% 场景定时更新时间间隔(毫秒)
-define(SCENE_UPDATE_OBJ_INTERVAL, 200).
-define(SCENE_HANDLE_CMD_INTERVAL, 100).

%% 场景主体ID
-define(SCENE_OBJ_ID, scene_obj_id).

%% 场景管理数据
-record(scene_mgr, {
    scene_id = 0,             %% 场景ID
    role_num_lim = 0,         %% 场景人数限制
    auto_line = 0,            %% 是否自动开启分线(1是|0否)
    line_no = 1,              %% 分线增量
    role_num = []             %% 分线人数([{LineId, RoleNum},...])
}).

%% NPC配置结构
-record(scene_npc, {
    npc_id = 0,               %% NPCID
    name = <<>>,              %% NPC名称
    sname = <<>>,             %% 场景名称
    scene_id = 0,             %% 场景ID
    x = 0,                    %% X坐标
    y = 0,                    %% Y坐标
    talk = 0                  %% 对话ID
}).

%% 场景信息数据
-record(scene, {
    scene_id = 0,             %% 场景ID
    line_id = 1,              %% 分线ID
    name = <<>>,              %% 场景名称
    type = 0,                 %% 场景类型
    sub_type = 0,             %% 场景子类型
    width = 0,                %% 地图宽度
    height = 0,               %% 地图高度
    x = 0,                    %% 默认出生X坐标
    y = 0,                    %% 默认出生Y坐标
    role_num_lim = 0,         %% 场景人数限制
    auto_line = 0,            %% 是否自动开启分线(1是|0否)
    npc = [],                 %% NPC列表
    mon = [],                 %% 默认怪物列表

    last_handle_time = 0,     %% 上次协议相关处理时间戳(毫秒)
    handle_timer = [],        %% 协议相关处理定时器
    last_update_time = 0,     %% 上次更新主体信息时间戳(毫秒)
    update_timer = []         %% 场景主体信息更新定时器
}).


%% 场景玩家数据
-record(scene_role, {
    key = [],                       %% 玩家KEY|[RoleId, Platform, ServerNum]
    role_id = 0,                    %% 玩家ID
    platform = "",                  %% 平台标识
    server_num = 0,                 %% 所在的服标识
    ori_sid = 0,                    %% 原始服务器标识(创建玩家时)
    nickname = "",                  %% 玩家名称
    career = 0,                     %% 职业
    sex = 0,                        %% 性别|1男 2女
    lv = 1,                         %% 等级
    vip_type = 0,                   %% VIP类型
    vip_lv = 0,                     %% VIP等级
    fsm_info = #fsm_info{},         %% 有限状态机

    rtype_id = 0,                   %% 机器人类型ID

    sid = none,                     %% 玩家消息进程PID
    pid = 0,                        %% 玩家进程PID
    node = none,                    %% 游戏服节点
    kfnode = none,                  %% 跨服节点(保存当前连到的跨服逻辑节点)
    kfgroup_id = 0,                 %% 跨服分组ID
    kfact_pid = none,               %% 对应跨服活动进程PID
    scene_id = 0,                   %% 当前所在场景ID
    line_id = 0,                    %% 分线ID
    copy_id = 0,                    %% 房间ID
    x = 0,                          %% 当前X坐标(整数)
    y = 0,                          %% 当前Y坐标(整数)

    combat_power = 0,               %% 战力
    title_id = 0                    %% 称号
}).
-endif.