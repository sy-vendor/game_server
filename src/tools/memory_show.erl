%%%-------------------------------------------------------------------
%%% @author sy
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 热更模块
%%% @end
%%% Created : 30. 9月 2019 14:44
%%%-------------------------------------------------------------------

-module(memory_show).
-compile(export_all).

show(N) ->
    F = fun(P) ->
        case catch process_info(P, [memory, dictionary]) of
            [{_, Memory}, {_, Dict}] ->
                InitStart = util:prop_get_value('$initial_call', Dict, null),
                {InitStart, Memory};
            _ -> {null, 0}
        end
    end,
    Infos1 = lists:map(F, processes()),
    Infos2 = [{Name,M} || {Name,M}<-Infos1, Name=/=null],
    SortFun = fun({_, M1}, {_, M2}) -> M1>M2 end,
    Infos3 = lists:sort(SortFun, Infos2),
    Infos4 = lists:sublist(Infos3, N),
    [io:format("~p : ~p ~n", [Name,M]) || {Name,M}<-Infos4],
    ok.

show(N, SkipNames) ->
    F = fun(P) ->
        case catch process_info(P, [memory, dictionary]) of
            [{_, Memory}, {_, Dict}] ->
                InitStart = util:prop_get_value('$initial_call', Dict, null),
                case catch tuple_to_list(InitStart) of
                    [Name|_] -> 
                        case lists:member(Name, SkipNames) of
                            true -> {null, 0};
                            false ->{InitStart, Memory}
                        end;
                    _ -> {null, 0}
                end;
            _ -> {null, 0}
        end
    end,
    Infos1 = lists:map(F, processes()),
    Infos2 = [{Name,M} || {Name,M}<-Infos1, Name=/=null],
    SortFun = fun({_, M1}, {_, M2}) -> M1>M2 end,
    Infos3 = lists:sort(SortFun, Infos2),
    Infos4 = lists:sublist(Infos3, N),
    [io:format("~p : ~p ~n", [Name,M]) || {Name,M}<-Infos4],
    ok.

show1(N) ->
    F = fun(P, Acc) ->
        case catch process_info(P, [memory, dictionary]) of
            [{_, Memory}, {_, Dict}] ->
                InitStart = util:prop_get_value('$initial_call', Dict, null),
                case lists:keyfind(InitStart, 1, Acc) of
                    false -> [{InitStart, Memory, 1}|Acc];
                    {InitStart,Memory1,Num} -> lists:keystore(InitStart, 1, Acc, {InitStart, Memory+Memory1,Num+1})
                end;
            _ -> Acc
        end
    end,
    Infos1 = lists:foldl(F, [], processes()),
    Infos2 = [{Name,M,Num} || {Name,M,Num}<-Infos1, Name=/=null],
    SortFun = fun({_, M1,_}, {_, M2,_}) -> M1>M2 end,
    Infos3 = lists:sort(SortFun, Infos2),
    Infos4 = lists:sublist(Infos3, N),
    [io:format("~p : per_memory=~p process_num=~p ~n", [Name,(M div Num),Num]) || {Name,M,Num}<-Infos4],
    ok.

            


