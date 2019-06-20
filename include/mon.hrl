%%-------------------------------------------------------
%% @File     : mon.hrl
%% @Brief    : 怪物头文件
%% @Author   : sy
%% @Date     : 2019-05-24
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__MON_H__).
-define(__MON_H__, 0).

-include("fsm.hrl").

%% 采集可被打断的秒数
-define(COLL_BREAK_TIME, 10).

%% 怪物种类定义(对应kind字段)
-define(MON_NORMAL, 0).                   %% 普通怪物
-define(MON_COLLECT, 1).                  %% 采集怪物
-define(MON_SKILL_MON, 2).                %% 技能表现怪物
-define(MON_STAR_MON, 3).                 %% 星阵表现怪物
-define(MON_AI_MON, 4).                   %% 隐形AI怪物

%% BOSS类型定义(对应boss字段)
-define(BOSS_NONE, 0).                    %% 非BOSS类型
-define(BOSS_WILD, 1).                    %% 普通野外BOSS
-define(BOSS_DUNGEON, 2).                 %% 副本BOSS
-define(BOSS_WORLD, 3).                   %% 世界BOSS
-define(BOSS_KF, 4).                      %% 跨服BOSS


%% 单个怪物状态数据
-record(scene_mon, {
    mon_id = 0,                           %% 怪物唯一ID
    mtype_id = 0,                         %% 怪物类型ID
    name = <<>>,                          %% 怪物名称
    lv = 1,                               %% 等级
    drop_exp = 0,                         %% 获得经验

    node = none,                          %% 所在节点
    scene_id = 0,                         %% 所在场景ID
    line_id = 1,                          %% 所在分线ID
    copy_id = 0,                          %% 所在房间ID
    d_x = 0,                              %% 默认出生点X坐标
    d_y = 0,                              %% 默认出生点Y坐标
    x = 0,                                %% 当前X坐标(整数)
    y = 0,                                %% 当前Y坐标(整数)
    invisible = 0,                        %% 是否可见(1不可见|0可见)

    kind = 0,                             %% 怪物种类|0怪物 1采集物
    boss = 0,                             %% boss类型|0普通怪 1BOSS
    fsm_info = #fsm_info{},               %% 有限状态机

    hp = 0,                               %% 初始血量
    summon_type = 0,                      %% 怪物召唤主体类型(1怪物|2玩家)
    summon_key = [],                      %% 怪物召唤主体KEY|[ObjId, Platform, ServerNum]

    c_ai_list = [],                       %% 怪物AI行为配置列表|[{触发类型, [AIId, ...]},...]
    ai_list = [],                         %% 怪物AI行为列表|[{触发类型, [AIId, ...]},...]
    ai_state = [],                        %% 怪物行为状态列表
    ai_content = []                       %% 怪物AI行为内容
}).


-endif.