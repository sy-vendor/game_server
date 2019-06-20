%%-------------------------------------------------------
%% @File     : acc_data.hrl
%% @Brief    :
%% @Author   : sy
%% @Date     : 2018-05-15
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__ACC_Data_H__).
-define(__ACC_Data_H__, 0).

%% 类型定义
-define(ACC_ROLE_ID,            1).     % 玩家ID自增
-define(ACC_BAN_ID,             2).     % 封禁规则ID自增

%% 自增类型列表
-define(ACC_TYPE_LIST, [
  ?ACC_ROLE_ID, ?ACC_BAN_ID
]).

%% SQL相关
-define(SQL_ACC_DATA_ROLE_GET, <<"select max(`role_id`) from `role_info` limit 1">>).
-define(SQL_ACC_DATA_BAN_GET, <<"select max(`id`) from `ban_info` limit 1">>).

-endif.  %% __ACC_Data_H__