%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 传闻管理进程
%%% @end
%%% Created : 24. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_rumor).
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

-export([publish/2, publish/3, publish/4, public_loop_rumor/4, reload/0, rumor_change/1, del_rumor/1, p/0, call/1, cast/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("chat.hrl").
-include("err_code.hrl").
-include("common.hrl").

%% 定时循环间隔(毫秒)
-define(RUMOR_LOOP_INTERVAL, 60000).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 发布传闻 [跨服] [world, scene]
publish(_SvrList, _Scope, 0, _Fun) -> ignore;
publish(SvrList, Scope, TempId, Fun) ->
    lib_kfcenter:cast_nodes(SvrList, ?MODULE, publish, [Scope, TempId, Fun]).

%% @doc 发布传闻
publish(Scope, TempId) ->
    publish(Scope, TempId, []).

publish(_Scope, 0, _Fun) -> ignore;
publish({scene, _, _, _} = Scope, TempId, Fun) ->
    case lib_rumor:format(Scope, TempId, Fun) of
        Rumor when is_record(Rumor, rumor) ->
            lib_rumor:publish(Rumor);
        _ ->
            ignore
    end;
publish(Scope, TempId, Fun) ->
    case lib_rumor:format(Scope, TempId, Fun) of
        Rumor when is_record(Rumor, rumor) ->
            cast({publish, Rumor});
        _ ->
            ignore
    end.

%% @doc 发送循环传闻
public_loop_rumor(Content, BeginTime, Interval, EndTime) ->
    Rumor = #rumor{
        type = ?RUMOR_TYPE_LOOP, temp_id = 0, scope = world, content = Content,
        begin_time = BeginTime, end_time = EndTime, interval = Interval
    },
    cast({publish, Rumor}).

%% @doc 重新载入传闻数据 [gm]
reload() ->
    cast({reload}).

%% @doc 传闻变化 [gm, 数据库中读取]
rumor_change(Id) ->
    cast({rumor_change, Id}).

%% @doc 删除传闻 [gm]
del_rumor(Id) ->
    Key = {?RUMOR_CATEGORY_GM, Id},
    cast({del_rumor, Key}).

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
    erlang:send_after(1000, self(), {init_for_start}),
    erlang:send_after(?RUMOR_LOOP_INTERVAL, self(), {loop}),
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
%% 启动初始化
handle_cast({init_for_start}, State) ->
    lib_rumor:init_rumor_list(),
    {noreply, State};
%% 重载传闻数据
handle_cast({reload}, State) ->
    lib_rumor:init_rumor_list(),
    {noreply, State};
%% 循环处理
handle_cast({loop}, State) ->
    lib_rumor:loop(),
    erlang:send_after(?RUMOR_LOOP_INTERVAL, self(), {loop}),
    {noreply, State};
%% 发送传闻
handle_cast({publish, Rumor}, State) ->
    lib_rumor:publish(Rumor),
    {noreply, State};
%% 删除传闻
handle_cast({del_rumor, Key}, State) ->
    lib_rumor:del_rumor(Key),
    {noreply, State};
%% 改变传闻内容
handle_cast({rumor_change, Id}, State) ->
    lib_rumor:rumor_change(Id),
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
