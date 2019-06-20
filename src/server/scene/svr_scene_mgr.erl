%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 场景管理进程
%%% @end
%%% Created : 24. 五月 2019
%%%-------------------------------------------------------------------
-module(svr_scene_mgr).
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

-export([create_scene/3, close_scene/2, get_line_id/2, check_role_num/2, add_line_role/2, del_line_role/2, creat_new_dungeon/5, i/0,
    p/0, cast/1, call/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("common.hrl").
-include("scene.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 新建场景
create_scene(SceneId, LineId, ExtraArgs) ->
    call({create_scene, [SceneId, LineId, ExtraArgs]}).

%% @doc 关闭场景进程
close_scene(SceneId, LineId) ->
    cast({close_scene, [SceneId, LineId]}).

%% @doc 获取场景分线ID
get_line_id(SceneId, LineList) ->
    call({get_line_id, [SceneId, LineList]}).

%% @doc 判断场景人数
check_role_num(SceneId, LineId) ->
    call({check_role_num, [SceneId, LineId]}).

%% @doc 增加对应场景人数
add_line_role(SceneId, LineId) ->
    cast({add_line_role, [SceneId, LineId]}).

%% @doc 删除对应场景人数
del_line_role(SceneId, LineId) ->
    cast({del_line_role, [SceneId, LineId]}).

%% 创建新副本进程
creat_new_dungeon(DunRoleL, ConfD, ConfC, Module, ExtraArgs) ->
    call({creat_new_dungeon, [DunRoleL, ConfD, ConfC, Module, ExtraArgs]}).

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
    process_flag(trap_exit, true),
    lib_scene_mgr:init(),
    {ok, 0}.

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
%% 创建场景
handle_call({create_scene, [SceneId, LineId, ExtraArgs]}, _FROM, State) ->
    ScenePid = lib_scene_mgr:create_scene(SceneId, LineId, ExtraArgs),
    {reply, ScenePid, State};
%% 获取场景分线ID
handle_call({get_line_id, [SceneId, LineList]}, _FROM, State) ->
    LineId = lib_scene_mgr:get_line_id(SceneId, LineList),
    {reply, LineId, State};
%% 判断场景人数
handle_call({check_role_num, [SceneId, LineId]}, _FROM, State) ->
    IsFull = lib_scene_mgr:check_role_num(SceneId, LineId),
    {reply, IsFull, State};
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
%% 开启所有场景
handle_cast({open_all_scene}, State) ->
    lib_scene_mgr:open_all_scene(),
    {noreply, State};
%% 开启单个场景
handle_cast({open_single_scene, [SceneList]}, State) ->
    lib_scene_mgr:open_single_scene(SceneList),
    {noreply, State};
%% 关闭场景进程
handle_cast({close_scene, [SceneId, LineId]}, State) ->
    lib_scene_mgr:close_scene(SceneId, LineId),
    {noreply, State};
%% 删除场景分线玩家
handle_cast({add_line_role, [SceneId, LineId]}, State) ->
    lib_scene_mgr:add_line_role(SceneId, LineId),
    {noreply, State};
%% 删除场景分线玩家
handle_cast({del_line_role, [SceneId, LineId]}, State) ->
    lib_scene_mgr:del_line_role(SceneId, LineId),
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
