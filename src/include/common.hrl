%%-------------------------------------------------------
%% @File     : common.hrl
%% @Brief    : 公共定义
%% @Author   : sy
%% @Date     : 2018-12-07
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__COMMON_H__).
-define(__COMMON_H__, 1).

%% 时间
-define(DATE_SECONDS, 86400).
-define(DATE_MS, 86400000).
-define(HOUR_SECONDS, 3600).
-define(HOUR_MS, 3600000).

%% 秒循环间隔(ms)
-define(INTERVAL_SEC_SPAN, 1000).

%% 定时关闭间隔(ms)
-define(INTERVAL_STOP_SPAN, 5000).

%% 微信相关
-define(APP_ID, "wx5ea26fbf09e8cf7c").
-define(APP_SECRET, "3f7e12e2ed6a12ef2e857b75565926e5").
-define(GRANT_TYPE, "authorization_code").

%% 公共定义0|1
-define(SUCCESS, 0).
-define(FAIL, 1).

%% 服务器信息
-record(server_state, {
    name = "",                   %% 名称
    value = 0                    %% 值
}).

%% 节点信息
-record(node, {
    id = 0,                      %% 节点ID
    ip = undefined,              %% 节点IP
    port = 0,                    %% 端口号
    name = undefined,            %% 节点名称
    cookie = undefined,          %% Cookie
    state = 0,                   %% 是否隐藏节点(1隐藏|0显示)
    time = 0                     %% 启动时间
}).

%% 输出sql
-ifdef(debug).
-define(DEBUG(Msg), util:debug(Msg, [], ?MODULE, ?LINE)).     %% 输出调试信息
-define(DEBUG(F, A), util:debug(F, A, ?MODULE, ?LINE)).
-else.
-define(DEBUG(Msg), ok).     %% 停止输出调试信息
-define(DEBUG(F, A), ok).
-endif.

%% 日志记录
-define(INFO(Msg), catch util:info(Msg, [], ?MODULE, ?LINE)).       %% 输出普通信息
-define(INFO(F, A), catch util:info(F, A, ?MODULE, ?LINE)).
-define(ERR(Msg), catch util:error(Msg, [], ?MODULE, ?LINE)).       %% 输出错误信息
-define(ERR(F, A), catch util:error(F, A, ?MODULE, ?LINE)).

-define(ERROR_MSG(Format, Args), util:errlog(Format, Args)).
-define(ERROR_MSG(Format), util:errlog(Format, [])).

%% 只为玩家统计用的，不要加别的字段进来了
-record(ets_online, {
    role_id = 0,                      %% 玩家Id
    pid = none,                       %% 玩家进程
    sid = none                        %% 发送进程
}).

-define(female, 0).
-define(male, 1).

%% 带catch的gen_server:call/2，返回{error, timeout} | {error, noproc} | {error, term()} | term() | {exit, normal}
-define(CALL(Pid, Request),
    case catch gen_server:call(Pid, Request) of
        {'EXIT', {timeout, _}} -> {error, timeout};
        {'EXIT', {noproc, _}} -> {error, noproc};
        {'EXIT', normal} -> {exit, normal};
        {'EXIT', ReasonErr} -> {error, ReasonErr};
        Rtn -> Rtn
    end).

-define(GCALL(P_i_d, R_e_q),
    case catch gen_server:call(P_i_d, R_e_q) of
        {'EXIT', E_r_r} ->
            ?ERROR_MSG("gen_server call error(~w)", [E_r_r]),
            {false, err_server_busy};
        R_e_p ->
            R_e_p
    end).

%% 提取record转成k-v形式
-ifdef(debug).
-define(record_kv(Record, Name), lists:zip(record_info(fields, Name), tl(tuple_to_list(Record)))).
-else.
-define(record_kv(Record, Name), Record).
-endif.

%% 流程控制
-define(IF(C, T, F), case (C) of true -> T;false -> F end).

%% 单个参数版本用法
-define(TRY_CATCH(Expression),
    fun() ->
        try
            Expression
        catch
            _:ErrReason ->
                ?ERROR_MSG("Catch exception: Reason:~w, Stacktrace:~w", [ErrReason, erlang:get_stacktrace()])
        end
    end()).

-define(ROLE_ID_INC, 1).

%% 字符串 or binary
-define(VALID_STR(Str), (is_binary(Str) orelse is_list(Str))).

%% 自增类型列表
-define(ACC_TYPE_LIST, [?ROLE_ID_INC]).

%% 节点隐藏标识
-define(NODE_HIDDEN, 1).         %% 隐藏状态
-define(NODE_SHOW, 0).           %% 显示状态

%% 服务器类型
-define(SERVER_TYPE_LOCAL, 1).   %% 逻辑节点
-define(SERVER_TYPE_CENTER, 2).  %% 中央节点

%% 节点ID编号
-define(NODE_ID_GAME, 10).       %% 逻辑节点
-define(NODE_ID_CENTER, 100).    %% 中央节点

-define(u8,     8/unsigned-integer).
-define(u16,    16/unsigned-integer).
-define(u32,    32/unsigned-integer).
-define(i8,     8/signed-integer).
-define(i16,    16/signed-integer).
-define(i32,    32/signed-integer).
-define(f,      32/float).

-endif.  %% __COMMON_H__