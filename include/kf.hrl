%%-------------------------------------------------------
%% @File     : kf.hrl
%% @Brief    : 跨服信息头文件
%% @Author   : sy
%% @Date     : 2019-05-14
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__KF_H__).
-define(__KF_H__, 0).

%% 服务器节点ETS
-define(ETS_SERVER_INFO, ets_server_info).
-define(ETS_NODE_INFO, ets_node_info).
-define(ETS_KF_GROUP, ets_kf_group).

%% 重连跨服中心管理节点的间隔时间(毫秒)
-define(PING_KFCENTER_INTERVAL, 60000).

%% 刷新跨服中心节点列表的间隔时间(毫秒)
-define(REFRESH_KF_INTERVAL, 60000).

%% 刷新跨服分组时间(毫秒)
-define(REF_KF_GROUP_INTERVAL, 60000).

%% 跨服活动开启状态
-define(KF_ACT_ON, 1).                  %% 活动开启中
-define(KF_ACT_OFF, 0).                 %% 活动关闭中

%% 跨服节点进程数据
-record(kfcenter_state, {
    dict_client_node = undefined        %% 已连上来的跨服通信节点
}).

%% 跨服通信进程数据
-record(kfclient_state, {
    kfcenter_id = 0,                    %% 跨服中心管理节点Id
    kfcenter_name = undefined,          %% 跨服中心管理节点名
    kfcenter_cookie = undefined,        %% 跨服中心管理节点Cookie
    connected = []
}).

%% 跨服活动记录
-record(kf_activity, {
    act_id = 0,                         %% 跨服活动ID
    b_act_id = 0,                       %% 绑定跨服活动ID
    op_state = 0,                       %% 进行状态(1进行中|0非进行中)
    act_state = 0,                      %% 活动状态
    group_dict = dict:new(),            %% 跨服分组列表|[#kf_group{}, ...]
    svr_dict = dict:new()               %% 服务器列表|[{{Platform, ServerNum}, GroupId}]
}).

%% 跨服分组记录
-record(kf_group, {
    group_id = 0,                       %% 跨服房间ID
    act_id = 0,                         %% 跨服活动ID
    server_list = [],                   %% 服务器列表|[?kf_server{}, ...]
    kf_node = none                      %% 分配的跨服节点
}).

%% 跨服服务器数据
-define(kf_server, #{
    server_key => [],                   %% 服务器KEY|{Platform, ServerNum}
    platform => "",                     %% 平台名称
    server_num => 0,                    %% 服务器标识
    server_name => "",                  %% 服务器名称
    node => none,                       %% 服务器所属0线节点名称
    open_time => 0                      %% 服务器开服时间
}).

%% =============================================================================
%% 相关SQL
%% =============================================================================

-define(SQL_KF_INFO_FETCH, <<"select `id`, `name`, `cookie` from `kf_info` limit 1">>).
-define(SQL_KF_CENTER_FETCH, <<"select `name` from kf_info where `id`=~p">>).
-define(SQL_GET_CONN_NODE, <<"select `client_node`, `platform`, `server_num` from `node_kf_connect` where `center_node`='~s'">>).
-define(SQL_ADD_CONN_NODE, <<"replace into `node_kf_connect`(`client_node`, `center_node`, `platform`, `server_num`, `connect_time`) values('~s', '~s', '~s', ~p, ~p)">>).
-define(SQL_DEL_CONN_NODE, <<"delete from `node_kf_connect` where `client_node`=~w">>).

-define(SQL_KF_ACT_GROUP_BY_ACT_ID_FETCH, <<"select ~p, kg.`platform`, kg.`server_num`, kg.`group_id`, ifnull(gs.`server_name`, ''), ifnull(replace(gs.`node`, '\r\n ', ''), ''), ifnull(gs.`open_time`, 0)
    from `kf_group` as kg 
    left join `kf_game_server` as gs on kg.`platform` = gs.`platform` and kg.`server_num` = gs.`server_num`
    where kg.`center_name`='~s' and kg.`time`<~p and kg.`activity_id`=~p">>).

-define(SQL_KF_ACT_GROUP_ALL_FETCH, <<"select kg.`activity_id`, kg.`platform`, kg.`server_num`, kg.`group_id`, ifnull(REPLACE(gs.`node`, '\r\n ', ''), ''), ifnull(gs.`open_time`, 0), ifnull(gs.`server_name`, '') 
    from `kf_group` as kg 
    left join `kf_game_server` as gs on kg.`platform` = gs.`platform` and kg.`server_num` = gs.`server_num`
    where kg.`center_name`='~s' and kg.`time`<~p">>).

-endif.  %% __KF_H__
