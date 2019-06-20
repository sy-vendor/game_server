%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 跨服分组进程逻辑处理
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_kfgroup).
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

-export([update_group/0, apply_cast/6, apply_other/7, get_act_info/1, i/0, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("kf.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 更新跨服分组信息
update_group() ->
  cast({update_group}).

%% @doc 远程调用函数
apply_cast(ActId, Platform, ServerNum, Mod, Fun, Args) ->
  cast({apply_cast, [ActId, Platform, ServerNum, Mod, Fun, Args]}).

%% @doc 远程调用函数(本服除外)
apply_other(ActId, Platform, ServerNum, RoleNode, Mod, Fun, Args) ->
  cast({apply_other, [ActId, Platform, ServerNum, RoleNode, Mod, Fun, Args]}).

%% @doc 获取分组信息
get_act_info(ActId) ->
  call({get_act_info, [ActId]}).

%% @doc 获取进程信息
i() ->
  call({info}).

%% @doc 获取进程ID
p() ->
  dist:whereis_name(local, ?MODULE).

%% @doc call
call(Request) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:call(?MODULE, Request);
    _ ->
      {error, ?ERR_COMMON_SERVICE_NOT_START}
  end.

%% @doc cast
cast(Request) ->
  case p() of
    Pid when is_pid(Pid) ->
      gen_server:cast(?MODULE, Request);
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
  process_flag(trap_exit, true),
  lib_kfgroup:init(),
  {ok, []}.

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
%% 获取进程信息
handle_call({info}, _From, State) ->
  Reply = lib_kfgroup:info(),
  {reply, Reply, State};
handle_call({get_act_info, [ActId]}, _From, State) ->
  Reply = lib_kfgroup:get_act_info(ActId),
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
%% 更新跨服分组
handle_cast({update_group}, State) ->
  lib_kfgroup:update_group(),
  {noreply, State};
%% 定时更新跨服分组
handle_cast({refresh_group}, State) ->
  lib_kfgroup:refresh_group(),
  {noreply, State};
%% 远程调用函数
handle_cast({apply_cast, [ActId, Platform, ServerNum, Mod, Fun, Args]}, State) ->
  lib_kfgroup:apply_cast(ActId, Platform, ServerNum, Mod, Fun, Args),
  {noreply, State};
%% 远程调用函数(本服除外)
handle_cast({apply_other, [ActId, Platform, ServerNum, RoleNode, Mod, Fun, Args]}, State) ->
  lib_kfgroup:apply_other(ActId, Platform, ServerNum, RoleNode, Mod, Fun, Args),
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
handle_info(Info, State) ->
  try
    handle_cast(Info, State)
  catch
    _:Reason ->
      ?ERROR_MSG("module ~w, line ~w, req ~w, state ~w, reason ~w, stacktrace ~w",
        [?MODULE, ?LINE, Info, State, Reason, erlang:get_stacktrace()]),
      {noreply, State}
  end.

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
