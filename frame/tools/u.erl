%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 线上热更模块
%%% @end
%%% Created : 25. 十二月 2018 9:55
%%%-------------------------------------------------------------------
-module(u).
-author("suyang").

%% API
-export([c/0,
    c/1,
    u/0,
    u/1,
    m/1,
    info/1,
    load/1,
    u_conf/1,
    u_conf/0]).

-include("common.hrl").
-include_lib("kernel/include/file.hrl").

c() ->
    c(5).

c(S) when is_integer(S) ->
    case file:list_dir(".") of
        {ok, FileList} ->
            Files = get_new_file(FileList, S * 60),
            info("~n---------check modules---------~n~w~n=========check modules=========", [Files]);
        Any ->
            info("Error Dir: ~w", [Any])
    end;
c(_) ->
    info("ERROR======> Badarg", []).

u() ->
    u(5).

u(m) ->
    StartTime = util_time:unixtime(),
    info("~n----------makes----------", []),
    c:cd("../"),
    make:all(),
    c:cd("ebin"),
    EndTime = util_time:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    u(Time / 60);
u(S) when is_number(S) ->
    case file:list_dir(".") of
        {ok, FileList} ->
            Files = get_new_file(FileList, util_math:ceil(S * 60) + 3),
            AllZone = svr_node:get_all_node(),
            info("---------modules---------~n~w~n----------nodes----------", [Files]),
            load(Files),
            loads(AllZone, Files);
        Any ->
            info("Error Dir: ~w", [Any])
    end;
u(Files) when is_list(Files) ->
    AllZone = svr_node:get_all_node(),
    info("~n---------modules---------~n~w~n----------nodes----------", [Files]),
    load(Files),
    loads(AllZone, Files);
u(_) ->
    info("ERROR======> Badarg", []).

%% m(['src/role/*','src/mod/role/role.erl'])
m(Files) when is_list(Files) ->
    StartTime = util_time:unixtime(),
    info("~n----------makes----------~n~w~n", [Files]),
    c:cd("../"),
    Res = make:files(Files, [debug_info, {i, "include"}, {outdir, "ebin"}]),
    c:cd("ebin"),
    EndTime = util_time:unixtime(),
    Time = EndTime - StartTime,
    info("Make Time : ~w s", [Time]),
    Res.

info(V) ->
    info(V, []).

info(V, P) ->
    io:format(V ++ "~n", P).

%% @doc 更新到所有节点
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
                                                FileName = util_type:list_to_atom(Left),
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

%% @doc 配置文件更新
u_conf() ->
    Pid = svr_configure:p(),
    svr_configure:cast(Pid, {refresh_all_config}).
u_conf(JsonName) ->
    Pid = svr_configure:p(),
    svr_configure:cast(Pid, {refresh_sign_config, JsonName}).
