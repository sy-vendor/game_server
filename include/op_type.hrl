%%-------------------------------------------------------
%% @File     : .hrl
%% @Brief    : 操作方式定义
%% @Author   : sy
%% @Date     : 2019-05-24
%%-------------------------------------------------------

%% 避免头文件多重包含
-ifndef(__OP_TYPE_HRL__).
-define(__OP_TYPE_HRL__, 0).

%%----------协议用操作类型-----------
-define(OP_SUCCESS, 1).   %% 操作成功
-define(OP_FAIL,    2).   %% 操作失败

-define(OP_ADD,    1).    %% 增加
-define(OP_DELETE, 2).    %% 删除
-define(OP_UPDATE, 3).    %% 更新
%%----------协议用操作类型-----------

-define(OPT_GM_OPERATE, 1).									%% gm操作

-endif.