%%-------------------------------------------------------
%% @File     : fsm.hrl
%% @Brief    : 有限状态机头文件
%% @Author   : sy
%% @Date     : 2019-05-24
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__FSM_H__).
-define(__FSM_H__, 0).

%% 状态宏定义
-define(FSM_IDLE, 1).          %% 空闲状态
-define(FSM_MOVE, 2).          %% 移动
-define(FSM_ATTACK, 3).        %% 攻击状态
-define(FSM_COLLECT, 4).       %% 采集状态
-define(FSM_DIE, 5).           %% 死亡状态

%% 有限状态机信息
-record(fsm_info, {
    cur_state_type = -1,         %% 当前状态类型|-1为初始化状态
    cur_state = undefined,       %% 当前状态模块
    next_state_type = -1,        %% 下一个状态类型
    state_duration = 0,          %% 状态持续时间(ms)
    state_dict = dict:new()      %% 存在的状态列表
}).

-endif.