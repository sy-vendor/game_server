%%-------------------------------------------------------
%% @File     : order.hrl
%% @Brief    : 订单系统头文件
%% @Author   : sy
%% @Date     : 2019-01-03
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__ORDER_H__).
-define(__ORDER_H__, 1).

%% 进程字典定义
-define(DICT_ROLE_ORDER, dict_role_order).    %% 订单系统信息

%% 订单状态
-define(ORDER_TYPE_NOTHING, 0).     %% 未接单
-define(ORDER_TYPE_READY, 1).       %% 准备
-define(ORDER_TYPE_COMPLETE, 2).    %% 送货中

%% 订单系统数据
-record(role_order, {
    order_id = 0,       %% 订单序号
    map_id = 0,         %% 订单映射Id
    order_type = 0,     %% 订单状态
    order_time = 0,     %% 订单时间
    order_reward = 0    %% 订单奖励初始值
}).

-define(SQL_ROLE_ORDER_FETCH, <<"select `order_id`, `map_id`, `order_type`, `order_time`, `order_reward`  from `role_order` where `role_id`=~p">>).
-define(SQL_ROLE_ORDER_REPLACE_P1, <<"replace into `role_order` (`role_id`, `order_id`, `map_id`, `order_type`, `order_time`, `order_reward`) values ~s">>).
-define(SQL_ROLE_ORDER_REPLACE_P2, <<"(~p, ~p, ~p, ~p, ~p, ~p)">>).
-define(SQL_ROLE_ORDER_DELETE, <<"delete from role_order where `role_id`=~p">>).

-endif.  %% __ORDER_H__