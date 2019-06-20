%%-------------------------------------------------------
%% @File     : activity.hrl
%% @Brief    : 活动相关
%% @Author   : sy
%% @Date     : 2019-05-22
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__ACTIVITY_H__).
-define(__ACTIVITY_H__, 0).

%% =============================================================================
%% 活动ID
%% =============================================================================

%% -----------------------------------------------------------------------------
%% --- 本服活动 ---
%% -----------------------------------------------------------------------------
%% === 日常活动 1xxx

%% === 开服活动 2xxx

%% === 合服活动 3xxx

%% === 运营活动 4xxx

%% -----------------------------------------------------------------------------
%% --- 跨服活动 ---（可以根据ID是否大于5000判断是否是跨服活动）
%% -----------------------------------------------------------------------------
%% === 跨服日常活动 5xxx

%% === 跨服运营活动 6xxx

%% === 永久开启活动 8xxx

%% =============================================================================
%% 活动管理
%% =============================================================================
-define(ETS_ACTIVITY, ets_activity).
-define(ETS_OPERATION_ACTIVITY, ets_operation_activity).

%% 活动内容结构
-define(activity, #{
  act_id => 0,                   %% 活动类型ID
  b_act_id => 0,                 %% 绑定活动类型ID
  op_id => 0,                    %% 运营活动ID
  conf_id => 0,                  %% 活动配置ID
  type => 0,                     %% 活动类型
  lv => 1,                       %% 活动开启等级
  start_time => 0,               %% 开始时间
  end_time => 0,                 %% 结束时间
  state => 0,                    %% 活动状态
  callback => undefined,         %% 回调模块
  callback2 => undefined,        %% 回调模块(跨服用)
  schedule => [],                %% 时间表
  conf => undefined,             %% 配置 [运营活动有效]
  index => 0,                    %% 配置索引 [运营活动有效]
  group => false,                %% 是否分组 [以下4个针对跨服]
  group_id => 0,                 %% 所属跨服分组ID
  dispatch_node => none,         %% 分配的跨服节点
  server_list => [],             %% 该分组下面的游戏服|[?kf_server{}, ...]
  open_list => [],               %% 游戏服开启时间 [跨服]
  copy => false
}).

%% 运营活动结构
-define(operation_activity, #{
  op_id => 0,                    %% 活动唯一ID
  act_id => 0,                   %% 活动类型ID
  start_time => 0,               %% 开始时间
  end_time => 0,                 %% 结束时间
  valid => 0,                    %% 是否有效
  conf => undefined,             %% 活动配置
  schedule => false              %% 是否进行中
}).

%% 活动状态
-define(ACT_STATE_UNDEFINED, 0).   %% 无
-define(ACT_STATE_PREPARE, 1).     %% 准备
-define(ACT_STATE_ONGOING, 2).     %% 进行中
-define(ACT_STATE_FINISH, 3).      %% 已结束
-define(ACT_STATE_REMOVE, 4).      %% 待移除

%% 时间类型
-define(ACTIVITY_TIME_NORMAL, 1).  %% 普通
-define(ACTIVITY_TIME_START, 2).   %% 开服
-define(ACTIVITY_TIME_MERGE, 3).   %% 合服

-define(VALID_ACTIVITY_TIMING, [
  ?ACTIVITY_TIME_NORMAL,
  ?ACTIVITY_TIME_START,
  ?ACTIVITY_TIME_MERGE
]).

%% 活动类型
-define(ACT_TYPE_DAILY, 1).        %% 日常活动
-define(ACT_TYPE_OPEN, 2).         %% 开服活动
-define(ACT_TYPE_MERGE, 3).        %% 合服活动
-define(ACT_TYPE_OPERA, 4).        %% 运营活动
-define(ACT_TYPE_KF_OPERA, 6).     %% 跨服运营活动
-define(ACT_TYPE_PERMANENT, 8).    %% 永久活动
-define(ACT_ID_BOUNDARY, 5000).    %% 本服、跨服活动ID分界
-define(ACT_IS_KF(ActID), (ActID > ?ACT_ID_BOUNDARY)).

%% 启动服务器后多久检查活动（延迟防止节点还没连通）
-define(ACTIVITY_START_CHECK_INTERVAL, 120).

%% =============================================================================
%% SQL语句
%% =============================================================================

-define(SQL_ALL_OPERATION_ACTIVITY_FETCH, <<"select `id`, `act_id`, `timing`, `begin_time`, `end_time`, `index`, `conf`, `valid` from operation_activity_schedule where floor(act_id / 1000) = ~w">>).
-define(SQL_OPERATION_ACTIVITY_FETCH, <<"select `id`, `act_id`, `timing`, `begin_time`, `end_time`, `index`, `conf`, `valid` from operation_activity_schedule where id = ~w">>).

%% 数据库相关
-define(SQL_ROLE_ACTIVITY_DATA_FETCH, <<"select `award_state`, `u_time` from `role_activity_data` where `role_id`=~p and `act_id`=~p">>).
-define(SQL_ROLE_ACTIVITY_DATA_REPLACE, <<"replace into `role_activity_data` (`role_id`, `act_id`, `award_state`, `u_time`) values ('~p', '~p', '~s', '~p')">>).
-define(SQL_ROLE_ACTIVITY_DATA_REPLACE_P1, <<"replace into `role_activity_data` (`role_id`, `act_id`, `award_state`, `u_time`) values ~s">>).
-define(SQL_ROLE_ACTIVITY_DATA_REPLACE_P2, <<"('~p', '~p', '~s', '~p')">>).

-endif.