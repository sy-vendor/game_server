%%-------------------------------------------------------
%% @File     : config.hrl
%% @Brief    : 通用配置
%% @Author   : sy
%% @Date     : 2019-05-14
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__CONFIG_H__).
-define(__CONFIG_H__, 0).

%% 系统配置
-record(conf_sys_config, {
    key = 0,       % 键
    value = []     % 值（不限类型）
}).

-record(conf_personal_setting, {
    id = 0,        % 键
    val = 0        % 值（整数）
}).

%% 个人设置ID宏
-define(PS_ACCEPT_STRANGER_CHAT, 5). %% 接受陌生人会话

%% -----------------------------------------------------------------------------

%% 获取玩家设置
-define(SQL_GET_ROLE_SETTING, <<"select setting from role_setting where role_id = ~w">>).
%% 保存玩家设置
-define(SQL_SAVE_ROLE_SETTING, <<"replace into role_setting(role_id, setting) values(~w, '~s')">>).

-endif.
