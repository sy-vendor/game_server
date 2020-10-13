%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 本地热更模块
%%% @end
%%% Created : 30. 9月 2019 12:11
%%%-------------------------------------------------------------------

-module(h).
-include_lib("kernel/include/file.hrl").

-export([
    h/0,         % 热更修改过的erl文件
    hh/0,        % 热更修改过的hrl头文件
    nodes_load/1,
    load/1
]).

% 热更 
% ==== u:h()  % 编译加载有改动过的erl文件（注意：修改了头文件的话请用下面一条）
%      u:hh()
h() ->
    ErlFileList = filelib:wildcard("../src/**/*.erl"),
    F = fun([$., $., $/ | ErlFile__] = ErlFile) ->
            case file:read_file_info(ErlFile) of            
                {ok, #file_info{mtime=ErlTime}} ->
                    BeamName = lists:concat(["../ebin/", filename:basename(ErlFile, ".erl"), code:objfile_extension()]),
                    case file:read_file_info(BeamName) of
                        {ok, #file_info{mtime=BeamTime}} when ErlTime > BeamTime ->
                            ErlFile__;
                        {ok, _} -> 
                            skip;
                        _E ->  
                            ErlFile__
                    end;
                _ -> 
                    skip
            end
    end,
    CompileFiles = [Y || Y <- [F(X) || X <- ErlFileList], Y=/=skip ],

    StartTime = unixtime(),
    io:format("----------makes----------~n~p~n", [CompileFiles]),
    c:cd("../"),
    Res = make:files(CompileFiles, [netload]),
    c:cd("ebin"),
    EndTime = unixtime(),
    io:format("Make result = ~p~nMake Time : ~p s", [Res, EndTime-StartTime]),
    % 其他服加载代码（cast到某条线调用，通过跨服中心让所有服务器加载这些文件）
    catch kf_load_beam(CompileFiles),
    ok.

hh() ->
    StartTime = unixtime(),
    c:l(mmake),
    io:format("----------makes----------~n", []),
    c:cd("../"),
    Res = ( catch mmake:all(8, [netload]) ),
    c:cd("ebin"),
    EndTime = unixtime(),
    io:format("MMake result = ~p~nMMake Time : ~p s", [Res, EndTime-StartTime]),
    ok.

%% 取得当前的unix时间戳
%% 单位：秒
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 跨服加载代码（跨服加载，通知所有服务器加载）
kf_load_beam([]) -> skip;
kf_load_beam(Files) ->
    Modules = [list_to_atom(filename:basename(File, ".erl")) || File<-Files ],
    case nodes(connected) of
        [Node|_] ->
            rpc:cast(Node, ?MODULE, kf_load_beam_all_server_in_local_node, [Modules]);
        _ -> 
            skip
    end,
    ok.

% 加载代码
nodes_load(Modules) ->
    % 本节点加载代码
    catch load(Modules),
    % 连接的其他节点加载代码
    ContenctedNodes = nodes(connected),
    [rpc:cast(Node, ?MODULE, load, [Modules]) || Node<-ContenctedNodes].

load(Modules) -> [ c:nl(M) || M <- Modules].

