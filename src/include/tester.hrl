%%-------------------------------------------------------
%% @File     : tester.hrl
%% @Brief    : tester头文件
%% @Author   : sy
%% @Date     : 2018-12-07
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__TESTER_H__).
-define(__TESTER_H__, 1).

-record(tester, {
    id = 0,                 %% 角色ID
    pid = 0,                %% 进程ID
    name = <<>>,            %% 角色名
    acc_name = <<>>,        %% 帐号名
    socket,
    send_count = 0,
    connect_time,
    read_bin = false,      %% 标识正在读取数据包头
    test_mod,               %% 测试模块
    test_mod2,              %% 测试模块
    work_index = 0          %% 工作序号
}).

-endif.  %% __TESTER_H__