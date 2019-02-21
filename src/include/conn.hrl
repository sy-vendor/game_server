%%-------------------------------------------------------
%% @File     : conn.hrl
%% @Brief    : conn头文件
%% @Author   : sy
%% @Date     : 2018-12-09
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__CONN_H__).
-define(__CONN_H__, 1).

-record(conn, {
    object,             %% 控制对象
    account = <<>>,     %% 连接器的所有者帐号名
    pid_object,         %% 控制对象pid
    object_id,          %% 角色ID
    socket,             %% socket
    ip,                 %% 客户端IP
    port ,              %% 客户端连接端口
    connect_time = 0,   %% 建立连接的时间
    length = 0,         %% 包体长度
    seq = 0,            %% 当前包序
    cmd = 0,
    read_bin = false,   %% 标识正在读取数据
    recv_count = 0,     %% 已接收的消息数量
    send_count = 0,     %% 已发送的消息数量
    error_send = 0,     %% 发送错误次数
    bad_req_count = 0,  %% 记录客户端发送的错误数据包个数
    last_hb = 0,        %% 最后一次心跳包时间
    hb_count = 1,       %% 心跳包计数
    hb_start = 0,       %% 心跳包开始时间
    hb_err = 0          %% 心跳包错误次数
}).


-define(CLIENT_GAME,        <<"game_client------------">>).
-define(CLIENT_WEB,         <<"web_client------------">>).

-endif.  %% __CONN_H__