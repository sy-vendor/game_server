%%-------------------------------------------------------
%% @File     : sign.hrl
%% @Brief    : 签到头文件
%% @Author   : sy
%% @Date     : 2018-12-19
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__SIGN_H__).
-define(__SIGN_H__, 1).

%% 进程字典定义
-define(DICT_ROLE_SIGN, dict_role_sign).    %% 玩家签到信息

-define(SIGN_FALSE, 0).     %% 未签到
-define(SIGN_TRUE, 1).      %% 已签到


-define(SIGN_TYPE_NORMAL, 0).   %% 普通签到
-define(SIGN_TYPE_LUXURY, 1).   %% 豪华签到

%% 合并数据
-record(role_sign, {
    is_sign = 0,            %% 今日是否签到
    total_sign_cnt = 0,     %% 签到总次数
    sign_time = 0,          %% 签到时间
    sign = false            %% 标识
}).


%% 数据库相关
-define(SQL_ROLE_SIGN_FETCH, <<"select `is_sign`, `total_sign_cnt`, `sign_time`  from `role_sign` where `role_id`=~p">>).
-define(SQL_ROLE_SIGN_REPLACE_P1, <<"replace into `role_sign` (`role_id`, `is_sign`, `total_sign_cnt`, `sign_time`) values ~s">>).
-define(SQL_ROLE_SIGN_REPLACE_P2, <<"(~p, ~p, ~p, ~p)">>).

-endif.  %% __SIGN_H__