%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 线上热更模块
%%% @end
%%% Created : 30. 9月 2019 16:12
%%%-------------------------------------------------------------------

%%----------------------------------------------------
%% Erlang模块热更新到所有节点（包括server的回调函数，如果对state有影响时慎用）
%%
%% 检查：u:c()                %% 列出前5分钟内编译过的文件
%%       u:c(N)               %% 列出前N分钟内编译过的文件
%%
%% 更新：u:u()                %% 更新前5分钟内编译过的文件               
%%       u:u(N)               %% 更新前N分钟内编译过的文件   
%%       u:u([svr_xx, ...])   %% 指定模块（不带后缀名）
%%       u:u(m)               %% 编译并加载文件
%%
%% Tips: u - update, c - check
%% 
%%----------------------------------------------------

-module(u).
-include_lib("kernel/include/file.hrl").
-include("common.hrl").

-export([
    c/0,
    c/1,
    admin/0,
    u/0,
    u/1,
    m/1,
    info/1,
    load/1
]).

c() ->
    c(5).

c(S) when is_integer(S) ->
    case file:list_dir(".") of
        {ok, FileList} ->
            Files = get_new_file(FileList, S * 60),
            info("---------check modules---------~n~w~n=========check modules=========", [Files]);
        Any ->
            info("Error Dir: ~w", [Any])
    end;
c(_) ->
    info("ERROR======> Badarg", []).

admin() ->
    spawn(fun() -> u(m) end),
    ok.

u() ->
    u(5).

u(m) ->
    StartTime = time:unixtime(),
    info("----------makes----------", []),
    c:cd("../"),
    make:all(),
    c:cd("ebin"),
    EndTime = time:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    u(Time / 60);
u(S) when is_number(S) ->
    case file:list_dir(".") of
        {ok, FileList} ->
            Files = get_new_file(FileList, util:ceil(S * 60) + 3),
            AllZone = svr_node:get_all_node(),
            info("---------modules---------~n~w~n----------nodes----------", [Files]),
            load(Files),
            loads(AllZone, Files);
        Any ->
            info("Error Dir: ~w", [Any])
    end;
u(Files) when is_list(Files) ->
    AllZone = svr_node:get_all_node(),
    info("---------modules---------~n~w~n----------nodes----------", [Files]),
    load(Files),
    loads(AllZone, Files);
u(_) ->
    info("ERROR======> Badarg", []).

%% m(['src/data/*','src/lib/lib_goods.erl'])
m(Files) when is_list(Files) ->
    StartTime = time:unixtime(),
    info("----------makes----------~n~w~n", [Files]),
    c:cd("../"),
    Res = make:files(Files, [debug_info, {i, "include"}, {outdir, "ebin"}]),
    c:cd("ebin"),
    EndTime = time:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    Res.

info(V) ->
    info(V, []).

info(V, P) ->
    io:format(V ++ "~n", P).

%% 更新到所有节点
loads([], _Files) -> ok;
loads([H | T], Files) ->
    info("[~w]", [H#node.name]),
    rpc:cast(H#node.name, u, load, [Files]),
    loads(T, Files).

get_new_file(Files, S) ->
    get_new_file(Files, S, []).

get_new_file([], _S, Result) -> Result;
get_new_file([H | T], S, Result) ->
    NewResult = case string:tokens(H, ".") of
        [Left, Right] when Right =:= "beam" ->
            case file:read_file_info(H) of
                {ok, FileInfo} ->
                    Now = calendar:local_time(),
                    case calendar:time_difference(FileInfo#file_info.mtime, Now) of
                        {Days, Times} ->
                            Seconds = calendar:time_to_seconds(Times),
                            case Days =:= 0 andalso Seconds < S of
                                true ->
                                    FileName = type:list_to_atom(Left),
                                    [FileName | Result];
                                false -> Result
                            end;
                        _ -> Result
                    end;
                _ -> Result
            end;
        _ -> Result
    end,
    get_new_file(T, S, NewResult).

load([]) -> ok;
load([FileName | T]) ->
    c:l(FileName),
    info("loaded: ~w", [FileName]),
    load(T).
