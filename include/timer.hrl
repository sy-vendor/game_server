%%-------------------------------------------------------
%% @File     : timer.hrl
%% @Brief    : 定时器头文件
%% @Author   : sy
%% @Date     : 2019-09-28
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__TIMER_H__).
-define(__TIMER_H__, 0).

%%% --------------------------------------------------------------------------
%%% 定时器详细内容配置格式说明：
%%% 格式如: 
%%% 	{TriggerMod, Module, Function, Args, DayList, WeekList, {StartHour, StartMin, StartSec}, {EndHour, EndMin, EndSec}, Gap}
%%%  	TriggerMod : 	该定时器进程启动时是否允许触发一次(0否1是)
%%% 	Module : 		模块
%%% 	Function : 		方法
%%% 	Args :			参数
%%% 	DayList : 		指定触发日期，每天触发为[]，指定某天触发如[7, 8, 9]
%%%		WeekList : 		指定触发星期，例如[1, 2, 7]，表示周一，周二，周日触发
%%% 	StartHour : 	开始小时数，如8，22
%%% 	StartMin : 		开始分钟数，如0，30
%%% 	StartSec : 		开始秒数，如0，30
%%% 	EndHour : 		结束小时数，如0，23
%%% 	EndMin : 		结束分钟数，如0，20
%%% 	EndSec : 		结束秒数，如0，30
%%% 	Gap : 			时间间隔秒数，如0，30，3600。0表示只刷新一次
%%% --------------------------------------------------------------------------

%% 公共节点定时器
% 格式说明：
% {启动触发, 模块, 函数, 参数列表, DayList, WeekList, StartTime, EndTime, Gap}
% StartTime = EndTime :: {Hour, Minute, Second}
-define(TIMER_LOGIC_LIST, [
]).

%% 跨服通信节点定时器
-define(TIMER_KFCLIENT_LIST, []).

%% 定时器刷新秒数
-define(TIMER_REFRESH, 10).
%% 定时器初始休眠秒数
-define(INIT_TIMEOUT, 2).

%% 定时器服务状态
-record(timer_state, {
    daytime = 0,            % 当天0点时间戳
    day = 0,                % 当天是几号
    week = 0,               % 当天是周几
    list = []               % 定时器列表
}).

%% 每条定时器具体内容
-record(timer_info, {
    next_time = 0,          % 下一次执行时间戳
    module = undefined,     % 模块
    func = undefined,       % 方法
    args = [],              % 参数
    day_list = [],          % 日期列表
    week_list = [],         % 周几列表
    start_hour = 0,         % 开始小时
    start_minute = 0,       % 开始分钟
    start_second = 0,       % 开始秒数
    end_hour = 0,           % 结束小时
    end_minute = 0,         % 结束分钟
    end_second = 0,         % 结束秒数
    gap = 0                 % 间隔秒数
}).

-endif.  %% _TIMER_H__