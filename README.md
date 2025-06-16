# Game Server Framework

一个基于 Erlang/OTP 构建的灵活、可扩展的游戏服务器框架。

## 特性

- 完整的项目结构和依赖管理
- 日志系统 (Lager)
- 配置管理
- 通用游戏房间/框架
- HTTP/REST 和 WebSocket 接口
- 安全认证系统
- 热代码升级
- 单元测试

## 系统架构

```
+------------------+     +------------------+     +------------------+
|                  |     |                  |     |                  |
|  HTTP/WebSocket  | --> |  Auth Middleware | --> |  Game Framework  |
|    Interface     |     |                  |     |                  |
+------------------+     +------------------+     +------------------+
                                                         |
                                                         v
+------------------+     +------------------+     +------------------+
|                  |     |                  |     |                  |
|  Room Manager    | <-- |  Game Instance   | <-- |  Game Behaviour  |
|                  |     |                  |     |                  |
+------------------+     +------------------+     +------------------+
```

## 快速开始

### 环境要求

- Erlang/OTP 24+
- rebar3

### 安装

```bash
git clone https://github.com/your-username/game_server.git
cd game_server
rebar3 compile
```

### 运行

```bash
rebar3 shell
```

## 框架组件

### 1. 认证系统

基于 Token 的认证系统，支持：
- Token 生成和验证
- 权限控制
- 请求频率限制

```erlang
% 生成 Token
{ok, Token} = game_server_auth:generate_token(UserId)

% 验证 Token
case game_server_auth:validate_token(Token) of
    {ok, #token{user_id = UserId}} ->
        % Token 有效
    {error, Reason} ->
        % Token 无效
end

% 检查权限
case game_server_auth:check_permission(Token, <<"game:play">>) of
    {ok, UserId} ->
        % 有权限
    {error, Reason} ->
        % 无权限
end
```

### 2. 房间管理

房间管理系统，支持：
- 创建游戏房间
- 加入/离开房间
- 房间状态管理
- 自动清理空房间

```erlang
% 创建房间
{ok, RoomId} = game_server_room_manager:create_room(GameType, MaxPlayers)

% 加入房间
{ok, RoomId} = game_server_room_manager:join_room(RoomId, PlayerId)

% 离开房间
ok = game_server_room_manager:leave_room(RoomId, PlayerId)

% 获取房间信息
{ok, Room} = game_server_room_manager:get_room_info(RoomId)
```

### 3. 游戏框架

通用游戏框架，支持：
- 游戏实例管理
- 玩家管理
- 动作处理
- 状态管理

```erlang
% 启动游戏
{ok, GameId} = game_server_game_framework:start_game(GameType, Module, Args)

% 加入游戏
{ok, GameId} = game_server_game_framework:join_game(GameId, PlayerId)

% 处理游戏动作
ok = game_server_game_framework:handle_action(GameId, PlayerId, Action)

% 获取游戏状态
{ok, State} = game_server_game_framework:get_game_state(GameId)
```

### 4. 游戏行为

定义游戏实现必须遵循的接口：

```erlang
-module(my_game).
-behaviour(game_server_game_behaviour).

-export([
    init/1,
    handle_join/2,
    handle_leave/2,
    handle_action/3,
    get_state/1,
    get_players/1,
    is_game_over/1,
    get_winner/1
]).

% 实现必要的回调函数
...
```

## API 文档

### HTTP API

#### 认证

```http
POST /api/auth/token
Content-Type: application/json

{
    "user_id": "user123"
}
```

响应：
```json
{
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

#### 游戏管理

```http
POST /api/games
Authorization: Bearer <token>
Content-Type: application/json

{
    "game_type": "poker",
    "max_players": 6
}
```

### WebSocket API

#### 连接

```javascript
const ws = new WebSocket('ws://server:8081/ws?token=<token>');
```

#### 消息

1. 加入房间
```json
{
    "type": "join_room",
    "room_id": "room123"
}
```

2. 游戏动作
```json
{
    "type": "game_action",
    "action": {
        "type": "play_card",
        "card": "A♠"
    }
}
```

## 配置

服务器可以通过环境变量或 `sys.config` 文件进行配置：

```erlang
[
    {game_server, [
        {http_port, 8080},
        {ws_port, 8081},
        {log_level, info}
    ]}
].
```

## 开发指南

### 添加新游戏类型

1. 创建新的游戏模块，实现 `game_server_game_behaviour`
2. 实现必要的回调函数
3. 注册游戏类型
4. 使用游戏框架 API 管理游戏实例

### 热代码升级

1. 准备新版本代码
2. 生成 appup 文件
3. 创建新版本发布
4. 执行热升级

```erlang
% 加载新模块
c:l(Module).

% 执行热升级
release_handler:create_RELEASE(Release, ReleaseDir).
release_handler:install_release(Release).
```

## 测试

```bash
# 运行单元测试
rebar3 eunit

# 运行集成测试
rebar3 ct
```

## 贡献

1. Fork 项目
2. 创建特性分支
3. 提交更改
4. 推送到分支
5. 创建 Pull Request
