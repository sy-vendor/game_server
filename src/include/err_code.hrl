%%-------------------------------------------------------
%% @File     : err_code.hrl
%% @Brief    : 通用错误码头文件
%% @Author   : sy
%% @Date     : 2018-12-18
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__ERR_CODE_H__).
-define(__ERR_CODE_H__, 1).

-define(ERR_COMMON_SYS, 10000).                     %% 系统错误
-define(ERR_COMMON_COST_LIMIT, 10001).							%% 消耗不足
-define(ERR_COMMON_MAP_LIMIT, 10002).							  %% 地图已填满
-define(ERR_COMMON_BUY_LIMIT, 10003).							  %% 购买限制
-define(ERR_COMMON_ALREADY_SIGN, 10004).						%% 已经签到
-define(ERR_COMMON_REWARD_LIMIT, 10005).						%% 奖励领取条件限制
-define(ERR_COMMON_REWARD_ALREADY, 10006).				  %% 奖励已经领取
-define(ERR_COMMON_CNT_LIMIT, 10007).				        %% 次数不足
-define(ERR_COMMON_FUNC_UNOPEND, 10008).				    %% 功能未开启
-define(ERR_COMMON_ORDER_OUTTIME, 10009).				    %% 订单不存在或已超时
-define(ERR_COMMON_ORDER_NOACCEPT, 10010).  		    %% 订单未接收
-define(ERR_COMMON_INSUFFICIENT_MATERIAL, 10011).  	%% 食材不足
-define(ERR_COMMON_ORDER_NOCOMMPLET, 10012).  	    %% 订单未完成
-define(ERR_COMMON_WAITER_NOEXIST, 10013).    	    %% 服务员不存在
-define(ERR_COMMON_WAITER_UNLOCK, 10014).    	      %% 上一类服务员未解锁
-define(ERR_COMMON_ORDER_COMMPLET, 10015).    	    %% 订单已完成
-define(ERR_COMMON_BUY_ERR, 10016).    	            %% 不能购买该菜肴
-define(ERR_COMMON_ORDER_ACCEPT, 10017).    	      %% 该订单已经接受

-endif.  %% __ERR_CODE_H__