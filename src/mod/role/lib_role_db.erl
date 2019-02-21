%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 玩家数据存储相关
%%% @end
%%% Created : 07. 十二月 2018 14:49
%%%-------------------------------------------------------------------
-module(lib_role_db).
-author("suyang").

%% API
-export([init_role_info/6,
    get_role_state_by_id/1,
    get_role_login_data/1,
    get_role_max/0,
    get_role_id_by_account/1,
    save_role_info/13,
    get_role_url_by_id/1]).


%% @doc 玩家注册初始化role_info
-define(SQL_ROLE_INFO_INSERT, <<"insert into `role_info` (`role_id`, `role_name`, `account`, `res_ip`, `url`, `sex`, `reg_time`, `coin`, `diamond`, `reputation`) values (~p, '~s', '~s', '~s', '~s', ~p, ~p, ~p, ~p, ~p)">>).
init_role_info(RoleId, RoleName, Account, Ip, Url, Sex) ->
    db:execute(io_lib:format(?SQL_ROLE_INFO_INSERT, [RoleId, RoleName, Account, Ip, Url, Sex, util_time:unixtime(), 100000, 100000, 100000])).

%% @doc 取得指定帐号的角色状态
-define(SQL_GET_ROLE_STATE_BY_ID, <<"select `account`, `state` from `role_info` where `role_id`=~p limit 1">>).
get_role_state_by_id(RoleId) ->
    db:get_row(io_lib:format(?SQL_GET_ROLE_STATE_BY_ID, [RoleId])).

%% @doc 获取role_login表登陆数据
-define(SQL_GET_ROLE_INFO_DATA, <<"select `role_name`, `url`, `sex`, `reg_time`, `last_logout_time`, `coin`, `diamond`, `reputation` from `role_info` where `role_id`=~p limit 1">>).
get_role_login_data(RoleId) ->
    db:get_row(io_lib:format(?SQL_GET_ROLE_INFO_DATA, [RoleId])).

%% @doc 获取总玩家数
-define(SQL_GET_ROLE_INFO_MAX, <<"select count(1) from role_info">>).
get_role_max() ->
    db:execute(io_lib:format(?SQL_ROLE_INFO_INSERT, [])).

%% @doc 取得指定帐号的ID
-define(SQL_GET_ROLE_ID_BY_ACCOUNT, <<"select `role_id` from `role_info` where `account`='~s' limit 1">>).
get_role_id_by_account(Account) ->
    db:get_row(io_lib:format(?SQL_GET_ROLE_ID_BY_ACCOUNT, [Account])).

%% @doc 存储玩家信息
-define(SQL_ROLE_INFO_UPDATE, <<"update `role_info` set `role_name`='~s', `account`='~s', `ip`='~s', `role_lv`=~p, `reg_time`=~p
        , `last_login_time`=~p, `last_logout_time`=~p, `sex`=~p, `state`=~p, `coin`=~p, `diamond`=~p, `reputation`=~p where `role_id`=~p">>).
save_role_info(RoleId, RoleName, Account, Ip, RoleLv, ResTime, LastLoginTime, LastLogoutTime, Sex, State, Coin, Diamond, Reputation) ->
    db:execute(io_lib:format(?SQL_ROLE_INFO_UPDATE, [RoleName, Account, Ip, RoleLv, ResTime, LastLoginTime, LastLogoutTime, Sex, State, Coin, Diamond, Reputation, RoleId])).

%% @doc 获取玩家头像
-define(SQL_GET_ROLE_URL_BY_ID, <<"select `url` from `role_info` where `role_id` = ~p limit 1">>).
get_role_url_by_id(RoleId) ->
    db:get_row(io_lib:format(?SQL_GET_ROLE_URL_BY_ID, [RoleId])).