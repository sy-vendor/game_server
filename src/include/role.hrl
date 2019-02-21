%%-------------------------------------------------------
%% @File     : role.hrl
%% @Brief    : role头文件
%% @Author   : sy
%% @Date     : 2018-12-07
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__ROLE_H__).
-define(__ROLE_H__, 1).

%% 玩家初始化模块定义
%%       1、所有功能模块的初始化操作统一放在这里处理；
%%       2、各自模块的初始化函数统一命名为: init/2；
%%       3、如果B模块依赖于A模块的数据，需要把A放在B前面初始化。
-define(INIT_MODULES, [lib_daily_api, lib_waiter_api, lib_merge_api, lib_order_api, lib_sign_api, lib_invite_api, lib_offline_api]).

%% @doc 定时保存玩家数据
%%      各自模块的定时入库函数统一命名为：save/3。
-define(SAVE_MODULES, [lib_daily_api, lib_waiter_api, lib_merge_api, lib_order_api, lib_sign_api, lib_invite_api]).

%% 玩家定时器数据
-record(tick_time, {
    five = 1,                         %% 五秒检查
    minute = 1,                       %% 分钟检查
    three_minute = 1                  %% 三分钟检查
}).

-record(role, {
    role_id = 0,                        %% 玩家Id
    role_name = <<>>,                   %% 玩家姓名
    account = "",                       %% 玩家账号
    role_lv = 1,                        %% 玩家等级
    url = "",
    reg_time = 0,                       %% 注册时间
    last_login_time = 0,                %% 最近登陆时间
    last_logout_time = 0,               %% 最进登出时间
    socket = none,                      %% socket
    reg_ip = ["0.0.0.0"],               %% 注册IP
    ip = ["0.0.0.0"],                   %% IP地址
    sid = none,                         %% 消息进程PID
    pid = none,                         %% 玩家进程PID
    sex = 0,                            %% 性别
    state = 0,                          %% 账号状态
    coin = 0,                           %% 金币
    diamond = 0,                        %% 钻石数
    reputation = 0,                     %% 口碑值
    tick_time = #tick_time{}            %% 定时器信息
}).

-define(TYPE_COIN, 1).                   %% 金币
-define(TYPE_DIAMOND, 2).                %% 钻石
-define(TYPE_REPUTATION, 3).             %% 口碑值

%% 服务员加成类型
-define(WAITER_SKILL_TYPE_ONE, 1).      %% 提升外卖订单收益
-define(WAITER_SKILL_TYPE_TWO, 2).      %% 提升餐厅收入收益
-define(WAITER_SKILL_TYPE_THREE, 3).    %% 提升离线收益
-define(WAITER_SKILL_TYPE_FOUR, 4).     %% 减少生产费用

-endif.  %% __ROLE_H__