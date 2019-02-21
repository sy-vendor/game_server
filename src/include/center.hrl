%%-------------------------------------------------------
%% @File     : center.hrl
%% @Brief    : center头文件
%% @Author   : sy
%% @Date     : 2018-12-25
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__CENTER_H__).
-define(__CENTER_H__, 1).

%% 服务器节点ETS
-define(SERVER_SRV_DATA_LIST, server_srv_data_list).
-define(ETS_SERVER_INFO, ets_server_info).

-define(SERVER_TYPE_MAIN, center).     %% 中央管理器节点
-define(SERVER_TYPE_SRV, logic).      %% 游戏节点服

%% 节点数据
-record(server_srv_data, {
    srv_id,          %% 服务器ID int()
    name,            %% 名称 bitstring()
    node,            %% 节点名称 string()
    host,            %% 域名 string()
    port,            %% 端口
    cookie,          %% 连接远端服务器时的erl cookie   string()
    ext = []         %% 附加数据[SrvOpenTime]
}).

%% 节点服连接数据
-record(server_srv, {
    srv :: #server_srv_data{},
    srv_id,         %% 服务器ID int()
    name,           %% 名称 bitstring()
    node,           %% 节点名称 atom()
    host,           %% 域名 string()
    cookie,         %% 连接远端服务器时的erl cookie   atom()
    pid,            %% 进程ID
    mpid,           %% 镜像进程监视的pid
    mref,           %% 镜像进程监视器
    timer_ref       %% 重连定时器
}).



%% =============================================================================
%% 相关SQL
%% =============================================================================


-endif.  %% __CENTER_H__