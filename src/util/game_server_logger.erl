-module(game_server_logger).

-export([
    debug/1, debug/2,
    info/1, info/2,
    warning/1, warning/2,
    error/1, error/2,
    critical/1, critical/2
]).

%% Debug level logging
debug(Msg) ->
    lager:debug(Msg).

debug(Format, Args) ->
    lager:debug(Format, Args).

%% Info level logging
info(Msg) ->
    lager:info(Msg).

info(Format, Args) ->
    lager:info(Format, Args).

%% Warning level logging
warning(Msg) ->
    lager:warning(Msg).

warning(Format, Args) ->
    lager:warning(Format, Args).

%% Error level logging
error(Msg) ->
    lager:error(Msg).

error(Format, Args) ->
    lager:error(Format, Args).

%% Critical level logging
critical(Msg) ->
    lager:critical(Msg).

critical(Format, Args) ->
    lager:critical(Format, Args). 