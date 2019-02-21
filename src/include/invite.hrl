%%-------------------------------------------------------
%% @File     : invite.hrl
%% @Brief    : 邀请好友头文件
%% @Author   : sy
%% @Date     : 2018-12-19
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__INVITE_H__).
-define(__INVITE_H__, 1).

%% 进程字典定义
-define(DICT_ROLE_INVITE, dict_role_invite).    %% 玩家签到信息

-define(REWARD_FAIL, 0).
-define(REWARD_SUCCESS, 1).

-record(role_invite, {
    b_invite_id = 0,        %% 邀请玩家Id
    b_invite_account = "",  %% 账号名
    b_invite_url = "",      %% 邀请玩家头像
    sort = 0,               %% 排序
    is_reward = 0,          %% 奖励是否领取
    sign = false            %% 数据库更新标识
}).

-define(INVITE_GIFT, 1).


%% 数据库相关
-define(SQL_ROLE_INVITE_FETCH, <<"select `b_invite_id`, `b_invite_account`, `b_invite_url`, `sort`, `is_reward` from `role_invite` where `role_id`=~p">>).
-define(SQL_ROLE_INVITE_REPLACE_P1, <<"replace into `role_invite` (`role_id`, `b_invite_id`, `b_invite_account`, `b_invite_url`, `sort`, `is_reward`) values ~s">>).
-define(SQL_ROLE_INVITE_REPLACE_P2, <<"(~p, ~p, '~s', '~s', ~p, ~p)">>).

-endif.  %% __INVITE_H__