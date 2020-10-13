%%-------------------------------------------------------
%% @File     : common.hrl
%% @Brief    : 公共定义
%% @Author   : sy
%% @Date     : 2019-09-28
%%---------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__COMMON_H__).
-define(__COMMON_H__, 0).

-include_lib("stdlib/include/ms_transform.hrl").
-include("ets.hrl").

%% 数据库相关
-define(DB, db).                 %% 数据库函数
-define(POOL_GAME, db_game).     %% 游戏数据库
-define(POOL_LOG, db_log).       %% 日志数据库
-define(INFO_DB_NUM, 50).        %% 保存频率

%% 时间相关定义
-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS, 86400).    %% 一天
-define(ONE_HOUR, 3600).            %% 一小时
-define(HALF_HOUR, 1800).           %% 半小时
-define(ONE_DAY_18HOUR,   64800).   %% 18点

%% 节点ID编号
-define(NODE_ID_GAME, 10).       %% 游戏逻辑节点点

%% 服务器类型
-define(SERVER_TYPE_LOCAL, 1).   %% 本服节点

%% 节点隐藏标识
-define(NODE_HIDDEN, 1).         %% 隐藏状态
-define(NODE_SHOW, 0).           %% 显示状态

%% 游戏节点记录
-record(node, {
    id = 0,                      %% 节点ID
    ip = undefined,              %% 节点IP
    port = 0,                    %% 端口号
    name = undefined,            %% 节点名称
    cookie = undefined,          %% 验证密码
    num = 0,                     %% 数量
    state = 0,                   %% 是否隐藏节点(1隐藏|0显示)
    time = 0                     %% 启动时间
}).

%% 服务器信息
-record(server_state, {
    name = "",                   %% 名称
    value = 0                    %% 值
}).

%% 错误处理
-define(DEBUG(F, A), log:log("debug", F, A, ?MODULE, ?LINE)).
-define(INFO(F, A), log:log("info", F, A, ?MODULE, ?LINE)).
-define(ERR(F, A), log:log("error", F, A, ?MODULE, ?LINE)).
-define(err(Msg), error_logger:error_report(Msg)).
-define(err(F, A), error_logger:error_report(lists:flatten(io_lib:format("~p ~p~n" ++ F, lists:append([?MODULE, ?LINE], A))))).

-define(ERROR_MSG(Format, Args), log:errlog(Format, Args)).
-define(ERROR_MSG(Format), log:errlog(Format, [])).

%% 三元表达式
-define(iif(A, B, C), (case A of true -> B; false -> C end)).

%% 断言
-define(ASSERT(Expression, Error), case (catch Expression) of true -> ok; _ -> erlang:throw(Error) end).
%% 字符串 or binary
-define(VALID_STR(Str), (is_binary(Str) orelse is_list(Str))).

%% 临时打印
-define(PRINT(Text), io:format(Text)).
-define(PRINT(Format, Args), io:format(Format, Args)).

-endif.  %% __COMMON_H__
