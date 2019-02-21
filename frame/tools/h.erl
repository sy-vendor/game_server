%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 本地热更模块
%%% @end
%%% Created : 6. 十二月 2018 18:15
%%%-------------------------------------------------------------------

-module(h).
-include_lib("kernel/include/file.hrl").

-export([
    h/0,
    hh/0,
    nodes_load/1,
    load/1
]).

% 热更
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

% 加载代码
nodes_load(Modules) ->
    % 本节点加载代码
    catch load(Modules),
    % 连接的其他节点加载代码
    ContenctedNodes = nodes(connected),
    [rpc:cast(Node, ?MODULE, load, [Modules]) || Node<-ContenctedNodes].

load(Modules) -> [ c:nl(M) || M <- Modules].

