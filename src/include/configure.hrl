%%-------------------------------------------------------
%% @File     : configure.hrl
%% @Brief    : 配置文件头文件
%% @Author   : sy
%% @Date     : 2018-12-21
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__CONFIGURE_H__).
-define(__CONFIGURE_H__, 1).

-define(JsonDir, "../configure/").
-define(Json, "json").

%% 配置文件定义
-define(COMMON_JSON, "common").
-define(BUILD_JSON, "building").
-define(ORDER_JSON, "orderForm").
-define(WAITER_JSON, "waiter").
-define(SKILL_JSON, "skill").
-define(SIGN_JSON, "sign").
-define(GIFT_JSON, "gift").

%% 需要启动时加载的配置文件
-define(JSON_LIST, [?COMMON_JSON, ?BUILD_JSON, ?ORDER_JSON, ?WAITER_JSON, ?SKILL_JSON, ?SIGN_JSON, ?GIFT_JSON]).

-record(common_config, {key, value}).
-record(building_config, {id = 0, start_value = 0, growth_value = 0, yield = 0, yield_offline = 0, recovery = 0, start_diamond = 0, growth_diamond = 0}).
-record(orderFrom_config, {id = 0, demand_goods = [], probability = 0, init_time = 0, ready_time = 0, delivery_time = 0, reward_gold = 0, reward_popularity = 0}).
-record(waiter_config, {id = 0, skill = 0, unlockCondition = 0}).
-record(skill_config, {id = 0, lv = 0, type = 0, intimacy = 0, intimacy_once = 0, reward_consume = 0, skill_addition = 0}).
-record(sign_config, {id = 0, common_sign = 0, luxury_sign = 0}).
-record(gift_config, {id = 0, type = 0, diamond = 0, condition = 0}).


-endif.  %% __CONFIGURE_H__