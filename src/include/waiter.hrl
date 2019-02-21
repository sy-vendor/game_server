%%-------------------------------------------------------
%% @File     : waiter.hrl
%% @Brief    : 服务员头文件
%% @Author   : sy
%% @Date     : 2019-01-03
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__WAITER_H__).
-define(__WAITER_H__, 1).

%% 进程字典定义
-define(DICT_WAITER_LIST, dict_waiter_list).    %% 服务台列表进程字典

%% 服务员信息
-record(waiter, {
    id = 0,         %% 服务员唯一Id
    type = 0,       %% 服务员类型
    intimate = 0,   %% 服务员亲密度
    skill_lv = 0,   %% 技能等级
    sign = false
}).

%% 数据库相关
-define(SQL_ROLE_WAITER_FETCH, <<"select `id`, `type`, `intimate`, `skill_lv`  from `role_waiter` where `role_id`=~p">>).
-define(SQL_ROLE_WAITER_REPLACE_P1, <<"replace into `role_waiter` (`role_id`, `id`, `type`, `intimate`, `skill_lv`) values ~s">>).
-define(SQL_ROLE_WAITER_REPLACE_P2, <<"(~p, ~p, ~p, ~p, ~p)">>).

-endif.  %% __WAITER_H__