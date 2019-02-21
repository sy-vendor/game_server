%%-------------------------------------------------------
%% @File     : ets.hrl
%% @Brief    : ets头文件
%% @Author   : sy
%% @Date     : 2018-12-07
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__ETS_H__).
-define(__ETS_H__, 1).

-define(ETS_SERVER_STATE, server_state).      %% 服务器信息
-define(ETS_NODE, ets_node).                  %% 节点列表
-define(ETS_ONLINE, ets_online).              %% 玩家在线列表

-endif.  %% __ETS_H__