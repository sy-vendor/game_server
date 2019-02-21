%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 通用资产处理
%%% @end
%%% Created : 03. 一月 2019 17:12
%%%-------------------------------------------------------------------
-module(lib_role_asset).
-author("suyang").

%% API
-export([send_change_info/1,
    cost_assets/2,
    add_assets/2,
    is_asset_enough/2]).

-include("common.hrl").
-include("logic.hrl").
-include("role.hrl").
-include("waiter.hrl").
-include("err_code.hrl").


%% @doc 推送玩家资产变化信息
send_change_info(Role) ->
    #role{coin = Coin, diamond = Diamond, reputation = Reputation} = Role,
    {ok, Bin} = lib_proto:pack(11003, #'RoleAssetChangeNotify'{coin = Coin, diamond = Diamond, reputation = Reputation}),
    lib_send:send_to_role(Role, Bin).

%% @doc 扣除消耗
cost_assets(CostList, Role) when is_record(Role, role) ->
    cost_asset2(CostList, Role);
cost_assets(_CostList, Role) -> ?ERROR_MSG("parameter error Role:~w", [Role]).

cost_asset2([], Role) -> Role;
cost_asset2([Cost | CostList], Role) ->
    case cost_asset3(Cost, Role) of
        RoleN when is_record(RoleN, role) ->
            cost_asset2(CostList, RoleN);
        {ok, RoleN} when is_record(RoleN, role) ->
            cost_asset2(CostList, RoleN);
        _R ->
            ?ERROR_MSG("the return result parameter is incorrect Reason:~w", [_R]),
            cost_asset2(CostList, Role)
    end.

cost_asset3({?TYPE_COIN, Val}, Role) ->
    cost_asset(?TYPE_COIN, Val, Role);
cost_asset3({?TYPE_DIAMOND, Val}, Role) ->
    cost_asset(?TYPE_DIAMOND, Val, Role);
cost_asset3({?TYPE_REPUTATION, Val}, Role) ->
    cost_asset(?TYPE_REPUTATION, Val, Role);
cost_asset3({Type, _}, _Role) ->
    ?ERROR_MSG("error in type:~w", [Type]);
cost_asset3(Cost, _Role) ->
    ?ERROR_MSG("unknown cause of error:~w", [Cost]).

-define(SQL_ROLE_UPDATE_DIAMOND, <<"update `role_info` set `diamond`=~p where `role_id`=~p">>).
cost_asset(_Type, 0, Role) -> Role;
cost_asset(Type, Cost, Role) ->
    #role{role_id = RoleId, coin = Coin, diamond = Diamond} = Role,
    % 扣除货币
    {RoleN, DBSign} =
        case Type of
            ?TYPE_COIN when Coin >= Cost ->
                RemainNum = Coin - Cost,
                {Role#role{coin = RemainNum}, false};
            ?TYPE_DIAMOND when Diamond >= Cost ->
                RemainNum = Diamond - Cost,
                {Role#role{diamond = RemainNum}, true}
        end,
    #role{coin = CoinN, diamond = DiamondN} = RoleN,
    ?IF(CoinN < 0 orelse DiamondN < 0, erlang:throw({error, ?ERR_COMMON_COST_LIMIT}), ignore),
    % 写数据库
    if
        DBSign ->
            Sql = io_lib:format(?SQL_ROLE_UPDATE_DIAMOND, [DiamondN, RoleId]),
            db:execute(Sql);
        true -> ignore
    end,
    send_change_info(RoleN),
    RoleN.

%% @doc 增加物品
add_assets(AddList, Role) when is_record(Role, role) ->
    add_asset2(AddList, Role);
add_assets(_AddList, Role) -> ?ERROR_MSG("parameter error Role:~w", [Role]).

add_asset2([], Role) -> Role;
add_asset2([Add | AddList], Role) ->
    case add_asset3(Add, Role) of
        RoleN when is_record(RoleN, role) ->
            add_asset2(AddList, RoleN);
        {ok, RoleN} when is_record(RoleN, role) ->
            add_asset2(AddList, RoleN);
        _R ->
            ?ERROR_MSG("the return result parameter is incorrect Reason:~w", [_R]),
            add_asset2(AddList, Role)
    end.

add_asset3({?TYPE_COIN, Val}, Role) ->
    add_asset(?TYPE_COIN, Val, Role);
add_asset3({?TYPE_DIAMOND, Val}, Role) ->
    add_asset(?TYPE_DIAMOND, Val, Role);
add_asset3({?TYPE_REPUTATION, Val}, Role) ->
    add_asset(?TYPE_REPUTATION, Val, Role);
add_asset3({Type, _}, _Role) ->
    ?ERROR_MSG("error in type:~w", [Type]);
add_asset3(Cost, _Role) ->
    ?ERROR_MSG("unknown cause of error:~w", [Cost]).

add_asset(_Type, 0, Role) -> Role;
add_asset(Type, Add, Role) ->
    #role{role_id = RoleId, coin = Coin, diamond = Diamond, reputation = Reputation} = Role,
    % 增加货币
    {RoleN, DBSign} =
        case Type of
            ?TYPE_COIN ->
                RemainNum = Coin + Add,
                {Role#role{coin = RemainNum}, false};
            ?TYPE_DIAMOND ->
                RemainNum = Diamond + Add,
                {Role#role{diamond = RemainNum}, true};
            ?TYPE_REPUTATION ->
                RemainNum = Reputation + Add,
                {Role#role{reputation = RemainNum}, false}
        end,
    #role{diamond = DiamondN} = RoleN,
    % 写数据库
    if
        DBSign ->
            Sql = io_lib:format(?SQL_ROLE_UPDATE_DIAMOND, [DiamondN, RoleId]),
            db:execute(Sql);
        true -> ignore
    end,
    send_change_info(RoleN),
    RoleN.

%% @doc 判断是否足够
is_asset_enough(List, Role) when is_record(Role, role) ->
    is_asset_enough2(List, Role);
is_asset_enough(_List, Role) ->
    ?ERROR_MSG("parameter error Role:~w", [Role]).

is_asset_enough2([], _Role) -> true;
is_asset_enough2([T | L], Role) ->
    case is_enough(T, Role) of
        {true, _} -> is_asset_enough2(L, Role);
        _ -> false
    end.

is_enough({?TYPE_COIN, Val}, Role) ->
    {is_enough_asset(Role, ?TYPE_COIN, Val), ?ERR_COMMON_COST_LIMIT};
is_enough({?TYPE_DIAMOND, Val}, Role) ->
    {is_enough_asset(Role, ?TYPE_DIAMOND, Val), ?ERR_COMMON_COST_LIMIT};
is_enough(_, _Role) ->
    {false, ?ERR_COMMON_COST_LIMIT}.

is_enough_asset(Role, Type, Val) ->
    #role{coin = Coin, diamond = Diamond} = Role,
    case Type of
        ?TYPE_COIN -> Coin >= Val;
        ?TYPE_DIAMOND -> Diamond >= Val;
        _ -> false
    end.


