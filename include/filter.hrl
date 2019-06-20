%%-------------------------------------------------------
%% @File     : filter.hrl
%% @Brief    : 过滤词头文件
%% @Author   : sy
%% @Date     : 2019-05-14
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__FILTER_HRL__).
-define(__FILTER_HRL__, 0).

-define(ETS_SENSITIVE_CONTENT, ets_game_sensitive_content).
-define(ETS_SENSITIVE_TALK_PASS_1, ets_game_sensitive_talk_pass_1).
-define(ETS_SENSITIVE_TALK_PASS_2, ets_game_sensitive_talk_pass_2).
-define(ETS_SENSITIVE_TALK_PASS_3, ets_game_sensitive_talk_pass_3).
-define(ETS_SENSITIVE_WORD_GROUP, ets_game_sensitive_word_group).

-define(FMT_FILTER(X), io_lib:format("~ts", [X])).

%% -----------------------------------------------------------------------------
%% SQL
%% -----------------------------------------------------------------------------

-define(SQL_LOAD_ALL_WORD_GROUP, <<"select `id`, `word` from `filter_word`">>).
-define(SQL_LOAD_ONE_WORD_GROUP, <<"select `word` from `filter_word` where `id` = ~w">>).

-endif.