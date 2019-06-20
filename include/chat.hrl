%%-------------------------------------------------------
%% @File     : chat.hrl
%% @Brief    : 聊天信息
%% @Author   : sy
%% @Date     : 2019-05-24
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__CHAT_H__).
-define(__CHAT_H__, 0).

%% 传闻进程字典标识
-define(RUMOR_QUEUE, rumor_queue).

%% 传闻类型
-define(RUMOR_TYPE_NORMAL, 1).      %% 普通，立即发送
-define(RUMOR_TYPE_TIMES, 2).       %% 发送指定次数
-define(RUMOR_TYPE_LOOP, 3).        %% 指定时间段内循环

-define(VALID_WEB_RUMOR_TYPE, [
    ?RUMOR_TYPE_TIMES,
    ?RUMOR_TYPE_LOOP
]).

%% 时间类型
-define(RUMOR_TIME_NORMAL, 1).      %% 普通
-define(RUMOR_TIME_START, 2).       %% 开服
-define(RUMOR_TIME_MERGE, 3).       %% 合服

-define(VALID_RUMOR_TIMING, [
    ?RUMOR_TIME_NORMAL,
    ?RUMOR_TIME_START,
    ?RUMOR_TIME_MERGE
]).

%% 传闻种类
-define(RUMOR_CATEGORY_GM, 3).      %% 后台
-define(VFR(Title), (is_list(Title) orelse is_binary(Title))).

%% 传闻数据
-record(rumor, {
    key = 0,                        %% key
    id = 0,                         %% 数据库ID（仅从数据库中读取的有效）
    type = 0,                       %% 类型（传闻类型）
    category = 0,                   %% 种类
    temp_id = 0,                    %% 模板ID
    scope = world,                  %% 范围(world | {guild, GuildId})
    begin_time = 0,                 %% 开始时间（类型2、3）
    end_time = 0,                   %% 结束时间（类型3）
    next_time = 0,                  %% 辅助计算字段
    interval = 0,                   %% 间隔时间（类型2、3）
    times = 0,                      %% 循环次数（类型2）
    content = ""                    %% 内容
}).

%% 相关SQL
-define(SQL_ROLE_CHAT_DATA_GET, <<"select `channels`, `sensitive`, `ban_type`, `ban_times`, `ban_end_time`, `time` from `role_chat` where `role_id`=~p">>).
-define(SQL_ROLE_CHAT_DATA_INSERT, <<"insert into `role_chat`(`role_id`, `time`) values (~p, ~p)">>).
-define(SQL_ROLE_CHAT_DATA_UPDATE, <<"update `role_chat` set `channels`='~s', `sensitive`=~p, `ban_times`=~p, `time`=~p where `role_id`=~p">>).

-define(SQL_BAN_ROLE_CHAT_UPDATE, <<"update `role_chat` set `ban_type`=~p, `ban_end_time`=~p where `role_id`=~p">>).
-define(SQL_UNBAN_ROLE_CHAT_UPDATE, <<"update `role_chat` set `ban_type`=0, `sensitive`=0, `ban_end_time`=0 where `role_id`=~p">>).

-define(SQL_ALL_VALID_RUMOR_FETCH, <<"select `id`, `type`, `time_type`, `begin_time`, `end_time`, `interval`, `times`, `content`, `valid`, `scope` from `sys_rumor`">>).
-define(SQL_SINGLE_RUMOR_FETCH, <<"select `type`, `begin_time`, `end_time`, `interval`, `times`, `content`, `scope` from `sys_rumor` where `id`=~p">>).

-endif.  %% _CHAT_H__