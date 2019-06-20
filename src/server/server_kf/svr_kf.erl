%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 跨服节点服务
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_kf).
-author("sy").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([i/0, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("kf.hrl").
-include("activity.hrl").

%%%===================================================================
%%% API
%%%===================================================================
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
      {error, ?ERR_COMMON_SERVICE_NOT_START}
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
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  erlang:process_flag(trap_exit, true),
  % 初始mysql
  ok = init_mysql(),
  % 初始ets表
  ok = init_ets(),
  % 初始化服务器数据
  ok = init_server_state(),
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
%% MYSQL数据库连接初始化
init_mysql() ->
  % 游戏数据库
  [DbHost, DbPort, DbUser, DbPass, DbName, DbEncode] = config:get_mysql(),
  ?DB:start(?POOL_GAME, DbHost, DbPort, DbUser, DbPass, DbName, DbEncode, 30),
  % 日志数据库
  [LDbHost, LDbPort, LDbUser, LDbPass, LDbName, LDbEncode] = config:get_log_mysql(),
  ?DB:start(?POOL_LOG, LDbHost, LDbPort, LDbUser, LDbPass, LDbName, LDbEncode, 30),
  ok.

%% ETS表初始化
init_ets() ->
  % 节点
  ets:new(?ETS_NODE, [{keypos, #node.id}, named_table, public, set]),
  % 服务器信息
  ets:new(?ETS_SERVER_STATE, [{keypos, #server_state.name}, named_table, public, set]),
  % 初始化场景NPC信息
  lib_scene_npc:init_ets(),
  % 跨服分组
  ets:new(?ETS_SERVER_INFO, [{keypos, 1}, named_table, public, set, {read_concurrency, true}]),
  ets:new(?ETS_NODE_INFO, [{keypos, 1}, named_table, public, set, {read_concurrency, true}]),
  ets:new(?ETS_KF_GROUP, [{keypos, 1}, named_table, public, set, {read_concurrency, true}]),
  % 活动
  ets:new(?ETS_ACTIVITY, [{keypos, 1}, named_table, public, set, {read_concurrency, true}]),
  ets:new(?ETS_OPERATION_ACTIVITY, [{keypos, 1}, named_table, public, set]),
  ok.

%% 初始化服务器状态
init_server_state() ->
  LocalName = node(),
  IsMainNode = case ?DB:get_row(?POOL_GAME, <<"select `id`, `name`, `cookie` from `kf_info` limit 1">>) of
                 [] ->
                   ?ERROR_MSG("svr_kfcenter_init: main kfcenter was not found in database table kf_info"),
                   false;
                 [_Id, Name, _Cookie] ->
                   case LocalName =:= type:object_to_atom(Name) of
                     true -> true;
                     false -> false
                   end
               end,
  ets:insert(?ETS_SERVER_STATE, #server_state{name = main_kfcenter, value = IsMainNode}),
  ets:insert(?ETS_SERVER_STATE, #server_state{name = server_type, value = ?SERVER_TYPE_KF}),
  ets:insert(?ETS_SERVER_STATE, #server_state{name = start_time, value = time:unixtime()}),
  ok.