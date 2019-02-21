%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 配置文件
%%% @end
%%% Created : 21. 十二月 2018 18:04
%%%-------------------------------------------------------------------
-module(lib_configure).
-author("suyang").

%% API
-export([]).

-export([init_data/0, get_json_data/1]).

-include("configure.hrl").
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 初始化配置表(后续配置文件建表需在下面添加)
init_data() ->
    [put(Name, get_json_data(Name)) || Name <- ?JSON_LIST].

get_json_data(?COMMON_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?COMMON_JSON) of
            {true, CommonInfo} ->
                ListAcc ++ build_common_config(CommonInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(?BUILD_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?BUILD_JSON) of
            {true, BuildInfo} ->
                ListAcc ++ build_build_config(BuildInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(?ORDER_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?ORDER_JSON) of
            {true, OrderInfo} ->
                ListAcc ++ build_order_config(OrderInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(?WAITER_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?WAITER_JSON) of
            {true, WaitInfo} ->
                ListAcc ++ build_wait_config(WaitInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(?SKILL_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?SKILL_JSON) of
            {true, SkillInfo} ->
                ListAcc ++ build_skill_config(SkillInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(?SIGN_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?SIGN_JSON) of
            {true, SignInfo} ->
                ListAcc ++ build_sign_config(SignInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(?GIFT_JSON) ->
    {ok, JsonList} = file:list_dir(?JsonDir),
    Fun = fun(File, ListAcc) ->
        case check_json(?JsonDir, File, ?GIFT_JSON) of
            {true, GiftInfo} ->
                ListAcc ++ build_gift_config(GiftInfo, []);
            _ERR -> ListAcc
        end end,
    lists:foldl(Fun, [], JsonList);
get_json_data(_JsonName) ->
    ?ERROR_MSG("Josn Name is not find============:~w", [_JsonName]),
    [].

check_json(Dir, File, Name) ->
    case string:tokens(File, ".") of
        [Name, ?Json] ->
            NewFile = Dir ++ File,
            case jsx:consult(NewFile) of
                [JsonList] when erlang:length(JsonList) > 0 ->
                    {true, JsonList};
                _ -> false
            end;
        _ERR -> false
    end.

build_common_config([], ListAcc) -> ListAcc;
build_common_config([List | Next], ListAcc) ->
    ConfKey = proplists:get_value(<<"key">>, List),       %% key
    Key = util_type:to_atom(ConfKey),
    Val = proplists:get_value(<<"val">>, List),       %% val
    CommonConfig = #common_config{key = Key, value = Val},
    build_common_config(Next, [CommonConfig | ListAcc]).

build_build_config([], ListAcc) -> ListAcc;
build_build_config([List | Next], ListAcc) ->
    Id = proplists:get_value(<<"q_id">>, List),         %
    StartValue = proplists:get_value(<<"q_start_value">>, List),
    GrowValue = proplists:get_value(<<"q_growth_value">>, List),
    Yield = proplists:get_value(<<"q_yield">>, List),
    YieldOffline = proplists:get_value(<<"q_yield_offline">>, List),
    StartDiamond = proplists:get_value(<<"q_start_diamond">>, List),
    GrowthDiamond = proplists:get_value(<<"q_growth_diamond">>, List),
    BuildingConfig = #building_config{id = Id, start_value = StartValue, growth_value = GrowValue, yield = Yield, yield_offline = YieldOffline, start_diamond = StartDiamond, growth_diamond = GrowthDiamond},
    build_build_config(Next, [BuildingConfig | ListAcc]).

build_order_config([], ListAcc) -> ListAcc;
build_order_config([List | Next], ListAcc) ->
    Id = proplists:get_value(<<"q_id">>, List),
    ConfGoods = proplists:get_value(<<"q_demandGoods">>, List),
    DemandGoods = build_goods(ConfGoods, []),
    Probability = proplists:get_value(<<"q_probability">>, List),
    InitTime = proplists:get_value(<<"q_init_time">>, List),
    ReadyTime = proplists:get_value(<<"q_ready_time">>, List),
    DeliveryTime = proplists:get_value(<<"q_delivery_time">>, List),
    RewardGold = proplists:get_value(<<"q_rewardGlod">>, List),
    RewardPopularity = proplists:get_value(<<"q_rewardPopularity">>, List),
    OrderConfig = #orderFrom_config{id = Id, demand_goods = DemandGoods, probability = Probability, init_time = InitTime, ready_time = ReadyTime, delivery_time = DeliveryTime, reward_gold = RewardGold, reward_popularity = RewardPopularity},
    build_order_config(Next, [OrderConfig | ListAcc]).

build_goods([], List) -> List;
build_goods([T | ConfGoods], List) ->
    GoodsLevel = proplists:get_value(<<"q_goodsLevel">>, T),
    GoodsNum = proplists:get_value(<<"q_goodsNum">>, T),
    ListN = [{GoodsLevel, GoodsNum} | List],
    build_goods(ConfGoods, ListN).

build_wait_config([], ListAcc) -> ListAcc;
build_wait_config([List | Next], ListAcc) ->
    Id = proplists:get_value(<<"q_id">>, List),
    Skill = proplists:get_value(<<"q_skill">>, List),
    UnlockCondition = proplists:get_value(<<"q_unlockCondition">>, List),
    WaiterConfig = #waiter_config{id = Id, skill = Skill, unlockCondition = UnlockCondition},
    build_wait_config(Next, [WaiterConfig | ListAcc]).

build_skill_config([], ListAcc) -> ListAcc;
build_skill_config([List | Next], ListAcc) ->
    Id = proplists:get_value(<<"q_id">>, List),
    Lv = proplists:get_value(<<"q_lvl">>, List),
    Type = proplists:get_value(<<"q_type">>, List),
    Intimacy = proplists:get_value(<<"q_Intimacy">>, List),
    IntimacyOnce = proplists:get_value(<<"q_IntimacyOnce">>, List),
    RewardConsume = proplists:get_value(<<"q_rewardConsume">>, List),
    SkillAddition = proplists:get_value(<<"q_skillAddition">>, List),
    SkillConfig = #skill_config{id = Id, lv = Lv, type = Type, intimacy = Intimacy, intimacy_once = IntimacyOnce, reward_consume = RewardConsume, skill_addition = SkillAddition},
    build_skill_config(Next, [SkillConfig | ListAcc]).

build_sign_config([], ListAcc) -> ListAcc;
build_sign_config([List | Next], ListAcc) ->
    Id = proplists:get_value(<<"q_id">>, List),
    CommonSign = proplists:get_value(<<"q_commonSign">>, List),
    LuxurySign = proplists:get_value(<<"q_luxurySign">>, List),
    SignConfig = #sign_config{id = Id, common_sign = CommonSign, luxury_sign = LuxurySign},
    build_sign_config(Next, [SignConfig | ListAcc]).

build_gift_config([], ListAcc) -> ListAcc;
build_gift_config([List | Next], ListAcc) ->
    Id = proplists:get_value(<<"q_id">>, List),
    Type = proplists:get_value(<<"q_type">>, List),
    Diamond = proplists:get_value(<<"q_diamond">>, List),
    Condition = proplists:get_value(<<"q_condition">>, List),
    GiftConfig = #gift_config{id = Id, type = Type, diamond = Diamond, condition = Condition},
    build_gift_config(Next, [GiftConfig | ListAcc]).