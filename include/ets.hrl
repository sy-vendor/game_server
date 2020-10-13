%%-------------------------------------------------------
%% @File     : ets.hrl
%% @Brief    : ETS定义头文件
%% @Author   : sy
%% @Date     : 2019-10-06
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__ETS_HRL__).
-define(__ETS_HRL__, 1).

%% ETS定义
-define(ETS_SERVER_STATE, server_state).      %% 服务器信息
-define(ETS_NODE, ets_node).                  %% 节点列表

-endif.