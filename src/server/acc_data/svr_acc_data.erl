%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 数据自增处理进程
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_acc_data).
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

-export([get_id/1, get_ids/2, i/0, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("acc_data.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 获取自增ID
get_id(IdType) ->
  call({get_id, [IdType]}).

%% @doc 获取自增ID列表
get_ids(IdType, Count) ->
  call({get_ids, [IdType, Count]}).

%% @doc 进程信息
i() ->
  call({info}).

%% @doc 进程ID
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
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

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
  State = load_data(),
  {ok, State}.

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
%% 返回自增值
handle_call({get_id, [IdType]}, _From, State) ->
  Id = case dict:find(IdType, State) of
         {ok, IdVal} -> IdVal + 1;
         error ->
           ServerNum = config:get_server_num(),
           IdVal = ServerNum * 1000000000 + 1,
           IdVal
       end,
  NewState = dict:store(IdType, Id, State),
  {reply, Id, NewState};
%% 返回自增值列表
handle_call({get_ids, [IdType, Count]}, _From, State) ->
  {Id, IdList} = case dict:find(IdType, State) of
                   {ok, IdVal} ->
                     IdValN = IdVal + Count,
                     {IdValN, lists:seq(IdVal + 1, IdValN)};
                   error ->
                     ServerNum = config:get_server_num(),
                     BaseIdVal = ServerNum * 1000000000,
                     IdVal = BaseIdVal + Count,
                     {IdVal, lists:seq(BaseIdVal + 1, IdVal)}
                 end,
  NewState = dict:store(IdType, Id, State),
  {reply, IdList, NewState};
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
%% 初始化数据
load_data() ->
  F = fun(IDType, ListAcc) -> get_acc_id_info(IDType, ListAcc) end,
  InfoList = lists:foldl(F, [], ?ACC_TYPE_LIST),
  case InfoList of
    [] -> dict:new();
    InfoList -> dict:from_list(InfoList)
  end.

%% 获取玩家ID自增数据
get_acc_id_info(?ACC_ROLE_ID, InfoList) ->
  Sql = io_lib:format(?SQL_ACC_DATA_ROLE_GET, []),
  case ?DB:get_one(?POOL_GAME, Sql) of
    undefined -> InfoList;
    MaxRoleId -> [{?ACC_ROLE_ID, MaxRoleId}|InfoList]
  end;
%% 获取封禁规则ID自增数据
get_acc_id_info(?ACC_BAN_ID, InfoList) ->
  Sql = io_lib:format(?SQL_ACC_DATA_BAN_GET, []),
  case ?DB:get_one(?POOL_GAME, Sql) of
    undefined -> InfoList;
    MaxBanId -> [{?ACC_BAN_ID, MaxBanId}|InfoList]
  end;
get_acc_id_info(_, InfoList) -> InfoList.