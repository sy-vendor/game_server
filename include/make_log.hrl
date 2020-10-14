%%-------------------------------------------------------
%% @File     : make_log.hrl
%% @Brief    : 日志
%% @Author   : sy
%% @Date     : 2019-09-28
%%---------------------------------------------------------
%% 避免头文件多重包含
-ifndef(__MAKE_LOG_HRL__).
-define(__MAKE_LOG_HRL__, 1).

%% 日志信息结构
-record(svr_log_info, {
    record_mod = 0,			%% 各日志的record命名
    file_name = "",			%% 各日志文件的名字
    file_content_list = []	%% 创建的时间戳，日志内容；...
}).

-define(MAKE_LOG_LIST, []).



-endif.