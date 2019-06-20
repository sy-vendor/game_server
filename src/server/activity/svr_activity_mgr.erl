%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 活动管理进程
%%% @end
%%% Created : 22. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_activity_mgr).
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

-export([start_link/2]).

-export([prepare_activity/1, start_activity/1,schedule_activity/1, update_activity/1, sync_activity/1, stop_activity/1, remove_activity/1,
  stop_act_by_type/2, get_activity_info/1, is_activity_ongoing/1, update_opera_activity/1, remove_opera_activity/1, get_all_going/0, i/0,
  p/0, cast/1, call/1]).

-define(SERVER, ?MODULE).

-record(state, {node_id = 0}).

-include("err_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 准备活动 [internal]
prepare_activity(Activity) ->
  cast({prepare_activity, [Activity]}).

%% @doc 启动活动 [internal]
start_activity(Activity) ->
  start_activity(Activity, false).
start_activity(Activity, Immediately) ->
  cast({start_activity, [Activity]}),
  Immediately andalso loop(time:unixtime()).

%% @doc 调度活动
schedule_activity(Activity) ->
  schedule_activity(Activity, false).
schedule_activity(Activity, Immediately) ->
  cast({schedule_activity, [Activity]}),
  Immediately andalso loop(time:unixtime()).

%% @doc 更新活动
update_activity(Activity) ->
  cast({update_activity, [Activity]}).

%% @doc 同步活动
sync_activity(Activity) ->
  cast({sync_activity, [Activity]}).

%% @doc 停止活动
stop_activity(ActId) ->
  stop_activity(ActId, false).
stop_activity(ActId, Force) ->
  cast({stop_activity, [ActId, Force]}).

%% @doc 移除活动
remove_activity(ActId) ->
  remove_activity(ActId, true).
remove_activity(ActId, Force) ->
  cast({remove_activity, [ActId, Force]}).

%% @doc 停止指定类型的活动
stop_act_by_type(Type, StopFlag) ->
  cast({stop_act_by_type, [Type, StopFlag]}).

%% @doc 获取活动信息
get_activity_info(ActId) ->
  call({get_activity_info, [ActId]}).

%% @doc 活动是否进行中
is_activity_ongoing(ActId) ->
  call({is_activity_ongoing, [ActId]}).

%% @doc 更新运营活动
update_opera_activity(ActId) ->
  cast({update_opera_activity, [ActId]}).

%% @doc 移除运营活动
remove_opera_activity(ActId) ->
  cast({remove_opera_activity, [ActId]}).

%% @doc 循环检查
loop(NowTime) ->
  cast({loop, [NowTime]}).

%% @doc 获取进行中的活动
get_all_going() ->
  call({get_all_going}).

%% @doc 进程信息
i() ->
  call({info}).

%% @doc 进程Id
p() ->
  dist:whereis_name(global, ?MODULE).

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

%% 启动进程
start_link(NodeId, SvrType) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [NodeId, SvrType], []).

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
init([NodeId, SvrType]) ->
  erlang:process_flag(trap_exit, true),
  lib_activity:init_server(NodeId, SvrType),
  {ok, #state{node_id = NodeId}}.

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
%% 获取活动信息
handle_call({info}, _From, State) ->
  Reply = lib_activity:info(),
  {reply, Reply, State};
%% 获取单个活动信息
handle_call({get_activity_info, [ActId]}, _From, State) ->
  Reply = lib_activity:get_activity_info(ActId),
  {reply, Reply, State};
%% 判断活动是否进行中
handle_call({is_activity_ongoing, [ActId]}, _From, State) ->
  Reply = lib_activity:is_activity_ongoing(ActId),
  {reply, Reply, State};
%% 获取当前所有进行中活动
handle_call({get_all_going}, _From, State) ->
  Reply = lib_activity:get_ongoing_list(),
  {reply, Reply, State};
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
%% 调度活动
handle_cast({schedule_activity, [Activity]}, State) ->
  lib_activity:schedule_activity(Activity),
  {noreply, State};
%% 准备活动
handle_cast({prepare_activity, [Activity]}, State) ->
  lib_activity:prepare_activity(Activity),
  {noreply, State};
%% 开始活动
handle_cast({start_activity, [Activity]}, State) ->
  lib_activity:start_activity(Activity),
  {noreply, State};
%% 更新活动
handle_cast({update_activity, [Activity]}, State) ->
  lib_activity:update_activity(Activity),
  {noreply, State};
%% 同步活动
handle_cast({sync_activity, [Activity]}, State) ->
  lib_activity:sync_activity(Activity),
  {noreply, State};
%% 停止活动
handle_cast({stop_activity, [ActId, Force]}, State) ->
  lib_activity:stop_activity(ActId, Force),
  {noreply, State};
%% 移除活动
handle_cast({remove_activity, [ActId, Force]}, State) ->
  lib_activity:remove_activity(ActId, Force),
  {noreply, State};
%% 停止指定类型活动
handle_cast({stop_act_by_type, [ActId, StopFlag]}, State) ->
  lib_activity:stop_act_by_type(ActId, StopFlag),
  {noreply, State};
%% 更新运营活动
handle_cast({update_opera_activity, [ActId]}, State) ->
  lib_activity:update_opera_activity(ActId),
  {noreply, State};
%% 移除运营活动
handle_cast({remove_opera_activity, [ActId]}, State) ->
  lib_activity:remove_opera_activity(ActId),
  {noreply, State};
%% 循环检查
handle_cast({loop, [NowTime]}, State) ->
  lib_activity:loop(State#state.node_id, NowTime),
  {noreply, State};
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
