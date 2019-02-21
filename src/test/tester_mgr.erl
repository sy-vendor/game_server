%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十二月 2018 11:15
%%%-------------------------------------------------------------------
-module(tester_mgr).
-author("suyang").

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([get/1,
    set/2,
    lookup/1,
    all/0,
    p/0]).

-define(SERVER, ?MODULE).

-include("tester.hrl").
-include("common.hrl").

-record(state, {
    i = 1,              % 索引
    c,                  % 已创建个数
    h,                  % 目标IP
    p,                  % 目标端口
    pre,                % 前缀
    n,                  % 要创建的总数量
    intv,               % 创建间隔
    mod,                % 处理模块
    tcase,              % 测试用例
    loop = false        % 是否循环处理
}).

%%%===================================================================
%%% API
%%%===================================================================
%% @spec get(Key) -> term()
%% Key = atom()
%% @doc 取出相应设置
get(Key) when is_atom(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{_Key, Val}] -> Val;
        _ ->
            undefined
    end.

%% @spec set(Key, Val) -> ok
%% Key = atom()
%% Val = term()
%% @doc 动态修改设置
%% 注意:进行并发操作get/set同一key时的事务性需要使用者来保证
set(Key, Val) when is_atom(Key) ->
    ets:insert(?MODULE, {Key, Val}).

%% @spec lookup(Tpid) -> #tester{}
%% Tpid = pid()
%% 获取指定的tester
lookup(Tpid) ->
    case ets:lookup(tester_online, Tpid) of
        [T] -> T;
        _ -> undefined
    end.

%% 所有在线的客户端
all() ->
    ets:tab2list(tester_online).

%%
p() ->
    util_dist:whereis_name(local, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%% @doc 启动环境变量服务器
start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

start_link(Host, Port, Tcase) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Tcase], []).

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
init([Host, Port]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, public, set]),
    ets:insert(?MODULE, [{host, Host}, {port, Port}]),
    ets:new(tester_online, [set, named_table, public, {keypos, #tester.pid}]),
    State = #state{},
    {ok, State};
init([Host, Port, Tcase]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, public, set]),
    ets:insert(?MODULE, [{host, Host}, {port, Port}]),
    ets:new(tester_online, [set, named_table, public, {keypos, #tester.pid}]),
    sys_env:set(test_case, Tcase),
    State = #state{tcase = Tcase},
    self() ! init_tcase,
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
handle_info({login_test, N, Per}, State) ->
    login_test:test(N, Per),
    {noreply, State};
%% 初始化测试用例
handle_info(init_tcase, State) ->
    State1 = init_tcase(State),
    {noreply, State1};
handle_info(stop, State) ->
    {stop, normal, State};
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
    tester:stop_all(),
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
init_tcase(State = #state{tcase = Case}) ->
    case get_case(Case, get_node_pre()) of
        {login_test, N, Per} -> self() ! {login_test, N, Per};
        {creat, N, Per} -> self ! {creat, N, Per}
    end,
    State.

get_node_pre() ->
    Node = erlang:node(),
    NodeL = erlang:atom_to_list(Node),
    hd(string:tokens(NodeL, "@")).

get_case(test, _) -> {login_test, 1000, test};
get_case(creat, _) -> {creat, 1, test};
get_case(_, _) ->
    false.