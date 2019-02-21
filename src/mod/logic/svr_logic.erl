%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 游戏逻辑服务
%%% @end
%%% Created : 10. 十二月 2018 18:42
%%%-------------------------------------------------------------------
-module(svr_logic).
-author("suyang").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([i/0,
    p/0,
    call/1,
    cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).
-include("ets.hrl").
-include("common.hrl").


%%%===================================================================
%%% API
%%%===================================================================
%% @doc 进程信息
i() ->
    call({info}).

%% @doc 进程ID
p() ->
    util_dist:whereis_name(local, ?MODULE).

%% @doc call函数
call(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, Msg);
        _ ->
            {error, server_not_start}
    end.

%% @doc cast函数
cast(Msg) ->
    case p() of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg);
        _ ->
            ignore
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Id]) ->
    erlang:process_flag(trap_exit, true),
    %% ETS表初始化
    ok = init_ets(),
    %% 初始化服务器信息
    ok = init_server_info(Id),
    {ok, ?MODULE}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc ETS表初始化
init_ets() ->
    % 节点
    ets:new(?ETS_NODE, [{keypos, #node.id}, named_table, public, set, {read_concurrency, true}]),
    % 服务器信息
    ets:new(?ETS_SERVER_STATE, [{keypos, #server_state.name}, named_table, public, set]),
    % 玩家在线数据
    ets:new(?ETS_ONLINE, [{keypos, #ets_online.role_id}, named_table, public, set]),
    ok.

%% 初始化服务器状态
init_server_info(Id) ->
    NowTime = util_time:unixtime(),
    % 初始化开服时间
    OpenTime = case string:tokens(sys_env:get(open_time), "-") of
                   [Y, M, D | _] ->
                       Year = list_to_integer(Y),
                       Month = list_to_integer(M),
                       Day = list_to_integer(D),
                       util_time:datetime_to_seconds({{Year, Month, Day}, {0, 0, 0}});
                   _ ->
                       util_time:unixdate(NowTime)
               end,
    ets:insert(?ETS_SERVER_STATE, #server_state{name = sid, value = Id}),
    ets:insert(?ETS_SERVER_STATE, #server_state{name = open_time, value = OpenTime}),
    ets:insert(?ETS_SERVER_STATE, #server_state{name = server_type, value = ?SERVER_TYPE_LOCAL}),
    ok.





