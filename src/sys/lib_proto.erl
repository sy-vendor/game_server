%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 协议解析相关函数
%%% @end
%%% Created : 15. 五月 2019
%%%-------------------------------------------------------------------
-module(lib_proto).
-author("sy").

%% API
-export([encode_msg/2, get_resolve_handler/1, get_proto_mod/1, get_msg_name/1]).

-include("common.hrl").

-define(u8,     8/unsigned-integer).
-define(u16,    16/unsigned-integer).
-define(u32,    32/unsigned-integer).
-define(i8,     8/signed-integer).
-define(i16,    16/signed-integer).
-define(i32,    32/signed-integer).
-define(f,      32/float).

%% @spec pack(Cmd, Data) -> {ok, Bin} | {error, Reason}
%% Cmd = integer()
%% Data = tuple()
%% Bin = binary()
%% Reason = bitstring()
%% @doc 打包协议数据
encode_msg(Cmd, Data) ->
  case get_resolve_handler(Cmd) of
    undefined ->
      ?ERR("模块影射失败[~w]:~w", [Cmd, Data]),
      {error, handler_not_find};
    ProtoMod ->
      Bin = ProtoMod:encode_msg(Data),
      Len = byte_size(Bin),
      BinData = <<Len:?u32, Cmd:?u16, Bin/binary>>,
      {ok, BinData}
  end.

%% @doc 协议格式需定义
get_proto_mod(Cmd) ->
  CmdStr = integer_to_list(trunc(Cmd / 100)),
  list_to_atom("proto_" ++ CmdStr).

get_resolve_handler(Cmd) ->
  CmdStr = integer_to_list(Cmd),
  list_to_atom("proto_hander_" ++ CmdStr).

get_msg_name(_Cmd) -> undefined.
