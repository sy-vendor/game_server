%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 游戏逻辑线服务
%%% @end
%%% Created : 06. 10月 2019 10:05
%%%-------------------------------------------------------------------

-module(svr_logic).
-behaviour(gen_server).
-include("common.hrl").

-export([
    i/0,
    p/0,
    call/1,
    cast/1,
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    do_handle_call/3,
    do_handle_cast/2,
	save/0
]).

%% SQL宏定义
-define(SQL_OPEN_TIME_GET, <<"select `cf_value` from `game_info` where `cf_name`='version'">>).
-define(SQL_OPEN_TIME_GET2, <<"select `reg_time` from `role_login` order by `role_id` limit 30,1">>).
-define(SQL_ACT_SERVER_ID_GET, <<"select `cf_value` from `game_info` where `cf_name`='sid'">>).
-define(SQL_MERGE_TIME_GET, <<"select `time`, `merge_count` from `merge_count` order by `time` desc limit 1">>).
-define(SQL_SHUTDOWN_TICK_GET, <<"select `cf_value` from `game_info` where `cf_name`='shutdowntick'">>).
-define(REPLACE_SHUTDOWN_TICK, <<"replace into `game_info`(cf_name, cf_value) values('~s', '~s')">>).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 进程信息
i() ->
    call({info}).
    
%% @doc 进程ID
p() ->
    dist:whereis_name(local, ?MODULE).
    
%% @doc call函数
call(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, err_common_service_not_start}
    end.

%% @doc cast函数
cast(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        _ ->
            ignore
    end.

%% =============================================================================
%% Gen_server Callbacks
%% =============================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    % 初始mysql
    ok = init_mysql(),
    % 初始ets表
    ok = init_ets(),
    % 初始化服务器数据
    ok = init_server_state(),
    {ok, ?MODULE}.

handle_call(Request, From, State) ->
    try
        do_handle_call(Request, From, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, from ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Request, From, State, Reason, erlang:get_stacktrace()]),
            {reply, {error, 255}, State}
    end.

handle_cast(Request, State) ->
    try
        do_handle_cast(Request, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Request, State, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_handle_cast(Info, State)
    catch
        _:Reason ->
            ?ERROR_MSG("module ~w, line ~w, info ~w, state ~w, reason ~w, stacktrace ~w",
                [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% 默认匹配
do_handle_call(_R, _FROM, State) ->
    {reply, ok, State}.

%% 默认匹配
do_handle_cast(_Reason, State) ->
    {noreply, State}.

%% MYSQL数据库连接初始化
init_mysql() ->
    % 游戏数据库
    [DbHost, DbPort, DbUser, DbPass, DbName, DbEncode] = config:get_mysql(),
    ?DB:start(?POOL_GAME, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, 40),
    ok.

%% ETS表初始化
init_ets() ->
    % 节点
    ets:new(?ETS_NODE, [{keypos, #node.id}, named_table, public, set, {read_concurrency, true}]),
    % 服务器信息
    ets:new(?ETS_SERVER_STATE, [{keypos, #server_state.name}, named_table, public, set]),
    ok.

%% 初始化服务器状态
init_server_state() ->
    NowTime = time:unixtime(),
    % 初始化开服时间
    OpenTime = case ?DB:get_row(?POOL_GAME, io_lib:format(?SQL_OPEN_TIME_GET, [])) of
        [] -> 
            case ?DB:get_row(?POOL_GAME, io_lib:format(?SQL_OPEN_TIME_GET2, [])) of
                [RegTime] when is_number(RegTime) -> time:unixdate(RegTime);
                _ -> time:unixdate(NowTime)
            end;
        [Str] ->
            [Y, M, D | _] = string:tokens(binary_to_list(Str), "-"),
            Year = list_to_integer(Y),
            Month = list_to_integer(M),
            Day = list_to_integer(D),
            time:unixtime({{Year, Month, Day}, {0, 0, 0}})
    end,
    % 初始化实际服务器ID
    SId = case ?DB:get_row(?POOL_GAME, io_lib:format(?SQL_ACT_SERVER_ID_GET, [])) of
        [] -> 0;
        [ServerIdStr] -> list_to_integer(binary_to_list(ServerIdStr))
    end,
	% 初始化上一次关机时间
    ShutDownTick = case ?DB:get_row(?POOL_GAME, io_lib:format(?SQL_SHUTDOWN_TICK_GET, [])) of
        [] -> 0;
        [ShutDownTickT] -> list_to_integer(binary_to_list(ShutDownTickT))
    end,
    ets:insert(?ETS_SERVER_STATE, #server_state{name = sid, value = SId}),
    ets:insert(?ETS_SERVER_STATE, #server_state{name = open_time, value = OpenTime}),
    ets:insert(?ETS_SERVER_STATE, #server_state{name = server_type, value = ?SERVER_TYPE_LOCAL}),
	  ets:insert(?ETS_SERVER_STATE, #server_state{name = shut_down_tick, value = ShutDownTick}),
    % 初始化合服时间
    [MergeTime, MergeCount] = case ?DB:get_row(?POOL_GAME, io_lib:format(?SQL_MERGE_TIME_GET, [])) of
        [MergeTimeS, MergeCountS] when is_integer(MergeTimeS), is_integer(MergeCountS) -> [MergeTimeS, MergeCountS];
        _ -> [0, 0]
    end,
    ets:insert(?ETS_SERVER_STATE, #server_state{name = merge_time, value = MergeTime}),
    ets:insert(?ETS_SERVER_STATE, #server_state{name = merge_count, value = MergeCount}),
    ets:insert(?ETS_SERVER_STATE, #server_state{name = start_time, value = NowTime}),
    ok.

%% 保存停服时间
save() ->
	Name = type:term_to_bitstring(shutdowntick),
	Time = type:term_to_string(time:unixtime()),
	SQL = io_lib:format(?REPLACE_SHUTDOWN_TICK, [Name, Time]),
	?DB:execute(?POOL_GAME, SQL),
	ok.
	
	