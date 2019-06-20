%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 协议解析相关函数
%%% @end
%%% Created : 10. 十二月 2018 16:59
%%%-------------------------------------------------------------------
-module(lib_proto).
-author("suyang").

%% API
-export([pack/2,
    unmask/2,
    read_bin/1,
    handle_data/2,
    decode_data/1]).

-include("common.hrl").
-include("conn.hrl").

%% @spec pack(Cmd, Data) -> {ok, Bin} | {error, Reason}
%% Cmd = integer()
%% Data = tuple()
%% Bin = binary()
%% Reason = bitstring()
%% @doc 协议打包
pack(Cmd, Data) ->
    case mapping:module(game_server, Cmd) of
        {ok, _Auth, _Caller, Proto, _ModName} ->
            NewData = Proto:encode_msg(Data),
            Len = byte_size(NewData),
            Bin = <<Len:?u32, Cmd:?u16, NewData/binary>>,
            BinData = build_frame(Bin),
            {ok, BinData};
        {error, _Code} ->
            ?ERROR_MSG("mapping err[~w]:~w", [Cmd, Data]),
            {error, pack_data_abnormal}
    end.

%% @doc 数据处理
handle_data(BinData, State = #conn{seq = Seq}) ->
    case decode_data(BinData) of
        {incomplete} ->
            StateN = State#conn{read_bin = false},
            sys_conn:read_next(StateN);
        {_, UnmaskedData, <<>>} ->
            {L, Cmd, Data} = lib_proto:read_bin(UnmaskedData),
            case L =< 0 of
                false -> sys_conn:routing(Cmd, Data, State#conn{read_bin = false, cmd = Cmd, seq = (Seq + 1) rem 128});
                true -> sys_conn:routing(Cmd, <<>>, State#conn{read_bin = false, cmd = Cmd, seq = (Seq + 1) rem 128})
            end;
        {_, UnmaskedData, Extra} ->
            {L, Cmd, Data} = lib_proto:read_bin(UnmaskedData),
            {noreply, StateN} =
                case L =< 0 of
                    false -> sys_conn:routing(Cmd, Data, State#conn{read_bin = false, cmd = Cmd, seq = (Seq + 1) rem 128});
                    true -> sys_conn:routing(Cmd, <<>>, State#conn{read_bin = false, cmd = Cmd, seq = (Seq + 1) rem 128})
                end,
            handle_data(Extra, StateN);
        {unknown, OpCode, _, Extra} ->
            case OpCode of
                8 -> ignore;
                _ -> ?ERROR_MSG("Unknown packet OpCode:~p Extra:~w", [OpCode, Extra])
            end,
            StateN = State#conn{read_bin = false},
            sys_conn:read_next(StateN)
    end.

decode_data(<<>>) -> {incomplete};
decode_data(Data) -> decode_new_data(Data).

decode_new_data(Data) when is_list(Data) -> decode_new_data(list_to_binary(Data));
decode_new_data(Data) when is_binary(Data) ->
    <<_Fin:1, _Rsv:3, Opcode:4, Mask:1, Len:7, Rest/binary>> = Data,
    {Length, RestPacket} = case Len of
                               126 ->
                                   <<ALen:16, ARest/binary>> = Rest,
                                   {ALen, ARest};
                               127 ->
                                   <<ALen:64, ARest/binary>> = Rest,
                                   {ALen, ARest};
                               _ ->
                                   {Len, Rest}
                           end,
    {MaskN, PayLoad} = case Mask of
                           1 ->
                               <<MaskData:32, PLoad/binary>> = RestPacket,
                               {<<MaskData:32>>, PLoad};
                           _ ->
                               {<<0:32>>, RestPacket}
                       end,
    {UnmaskedData, Extra} =
        case Length > size(PayLoad) of
            true -> {<<>>, Data};
            _ ->
                <<APayLoad:Length/binary, EExtra/binary>> = PayLoad,
                {unmask(APayLoad, MaskN), EExtra}
        end,
    case Opcode of
        _ when Length > size(PayLoad) -> {incomplete};
        0 -> {continuation, binary_to_list(UnmaskedData), Extra};
        2 -> {binary, UnmaskedData, Extra};
        _ -> {unknown, Opcode, UnmaskedData, Extra}
    end.

unmask(PayLoad, 0) -> PayLoad;
unmask(PayLoad, Mask) -> unmask(PayLoad, Mask, <<>>).
unmask(<<Frame:32, Rest/binary>>, <<Mask:32>> = MaskB, Acc) ->
    unmask(Rest, MaskB, <<Acc/binary, (Frame bxor Mask):32>>);
unmask(<<Frame:24>>, <<Mask:24, _/binary>>, Acc) ->
    <<Acc/binary, (Frame bxor Mask):24>>;
unmask(<<Frame:16>>, <<Mask:16, _/binary>>, Acc) ->
    <<Acc/binary, (Frame bxor Mask):16>>;
unmask(<<Frame:8>>, <<Mask:8, _/binary>>, Acc) ->
    <<Acc/binary, (Frame bxor Mask):8>>;
unmask(<<>>, _, Acc) -> Acc.

%% @doc 协议数据解包
read_bin(Bin) ->
    case Bin of
        <<Len:?u32, Cmd:?u16, Rest/binary>> -> {Len, Cmd, Rest};
        _R -> {-1, -1, <<>>}
    end.

%%封装二进制版本的websocket数据
build_frame(Content) when erlang:is_binary(Content) ->
    DataLength = byte_size(Content),
    build_frame_binary(DataLength, Content);

%%封装文本的websocket数据
build_frame(Content) when erlang:is_list(Content) ->
    Bin = unicode:characters_to_binary(Content), %转换为二进制
    DataLength = byte_size(Bin),
    build_frame_text(DataLength, Bin).

build_frame_binary(DataLength, Bin) when DataLength =< 125 ->
    <<1:1, 0:3, 2:4, 0:1, DataLength:7, Bin/binary>>;
build_frame_binary(DataLength, Bin) when DataLength >= 125, DataLength =< 65535 ->
    <<1:1, 0:3, 2:4, 0:1, 126:7, DataLength:16, Bin/binary>>;
build_frame_binary(DataLength, Bin) when DataLength > 65535 ->
    <<1:1, 0:3, 2:4, 0:1, 127:7, DataLength:64, Bin/binary>>.
build_frame_text(DataLength, Bin) when DataLength =< 125 ->
    <<1:1, 0:3, 1:4, 0:1, DataLength:7, Bin/binary>>;
build_frame_text(DataLength, Bin) when DataLength >= 125, DataLength =< 65535 ->
    <<1:1, 0:3, 1:4, 0:1, 126:7, DataLength:16, Bin/binary>>;
build_frame_text(DataLength, Bin) when DataLength > 65535 ->
    <<1:1, 0:3, 1:4, 0:1, 127:7, DataLength:64, Bin/binary>>.