%%-------------------------------------------------------
%% @File     : role.hrl
%% @Brief    : role头文件
%% @Author   : sy
%% @Date     : 2019-05-22
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(_ROLE_H__).
-define(_ROLE_H__, 0).

%% 定时关闭间隔(ms)
-define(INTERVAL_STOP_SPAN, 300000).

%% 定时保存间隔(ms)
-define(INTERVAL_SAVE_SPAN, 30 * 60 * 1000).

%% 秒循环间隔(ms)
-define(INTERVAL_SEC_SPAN, 1000).

%% 玩家状态定义
-define(ROLE_STATE_NORMAL, 0).        %% 正常状态
-define(ROLE_STATE_BANACC, 1).        %% 账号封禁状态
-define(ROLE_STATE_BANIP, 2).         %% IP封禁状态
-define(ROLE_STATE_BANDEV, 3).        %% 设备封禁状态

%% 职业性别对应关系([{职业,性别},...])
-define(CAREER_LIST, [{1, 1}, {2, 2}, {3, 1}, {4, 2}]).

%% 玩家初始化模块定义
%% 注意：1、所有功能模块的初始化操作统一放在这里处理；
%%       2、各自模块的初始化函数统一命名为: init/3；
%%       3、如果B模块依赖于A模块的数据，需要把A放在B前面初始化。
-define(INIT_MODULES, [
    lib_daily_api
]).

%% @doc 定时保存玩家数据
%%      注意：1、变动频繁的模块建议放在：interval_save；
%%            2、各自模块的定时入库函数统一命名为：save/3。
-define(SAVE_MODULES, [
    lib_daily_api
]).

%% @doc 属性更新模块
%%      注意：1、所有功能模块的属性计算操作统一放在这里处理；
%%            2、各自模块的属性计算函数统一命名为: calc_attr/0 或 calc_attr/1（参数为RoleState）；
-define(ATTR_MODULES, [
]).

%% 玩家登陆信息
-record(role, {
    socket = undefined,               %% socket
    pid = undefined,                  %% 玩家进程
    login_flag = 0,                   %% 是否登录
    acc_id = 0,                       %% 账号ID
    accname = undefined,              %% 账号名称
    timeout_total_num = 0,            %% 总超时次数
    cmd_total_num = 0,                %% 总请求次数
    cmd_list = [],                    %% 请求列表
    cmd_last_time = 0,                %% 最后请求时间
    server_num = 0                    %% 服务器编号
}).

%% 只为玩家统计用的，不要加别的字段进来了
-record(ets_online, {
    role_id = 0,                      %% 玩家Id
    pid = none,                       %% 玩家进程
    sid = none                        %% 发送进程
}).


%% 玩家进程数据
-record(role_state, {
    accname = "",                     %% 平台账号
    role_id = 0,                      %% 用户Id
    plat_id = "",                     %% 平台标识
    platform = "",                    %% 平台名称
    server_num = 0,                   %% 当前所在服务器标识
    ori_sid = 0,                      %% 原始服务器标识(创建玩家时)
    ditch_id = "",                    %% 渠道
    device = "",                      %% 设备号
    app_key = "",                     %% 应用标识
    reg_time = 0,                     %% 注册时间
    last_login_time = 0,              %% 最后登陆时间(秒)
    last_logout_time = 0,             %% 最后退出时间(秒)
    last_offline_time = 0,            %% 最后离线时间(秒)
    socket = none,                    %% socket
    ip = ["0.0.0.0"],                 %% IP地址
    sid = none,                       %% 消息进程PID
    pid = none,                       %% 玩家进程PID
    nickname = <<>>,                  %% 玩家名
    career = 0,                       %% 职业
    sex = 0,                          %% 性別
    lv = 1,                           %% 等级
    first_pay_time = 0,               %% 首充时间戳(秒)
    is_pay = 0,                       %% 是否充值过(1是|0否)
    total_got_gold = 0,               %% 历史总获得元宝
    gold = 0,                         %% 元宝
    bgold = 0,                        %% 绑定元宝
    coin = 0,                         %% 铜币
    bcoin = 0,                        %% 绑铜
    sys_icon = 0,                     %% 头像
    exp = 0,                          %% 经验
    combat_power = 0,                 %% 玩家战力

    kfnode = none,                    %% 跨服节点(保存当前连到的跨服逻辑节点)
    kfgroup_id = 0,                   %% 跨服分组ID
    kfact_pid = none,                 %% 对应跨服活动进程PID

    scene_id = 0,                     %% 场景Id
    line_id = 1,                      %% 场景线Id
    copy_id = 0,                      %% 场景分组Id
    x = 0,                            %% 当前X坐标
    y = 0,                            %% 当前Y坐标
    last_scene_id = 0,                %% 之前场景Id
    last_line_id = 1,                 %% 之前場景线Id
    last_copy_id = 0,                 %% 之前场景分组Id
    last_x = 0,                       %% 之前X坐标
    last_y = 0,                       %% 之前Y坐标
    reset_flag = 0,                   %% 玩家坐标修正标识
    group = 0,                        %% 分组

    info_sign = 0                     %% 更新标识
}).


-endif.  %% __ROLE_H__