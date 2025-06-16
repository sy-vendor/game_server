# Erlang Game Server Framework

A lightweight and high-performance game server framework built with Erlang.

## Features

- High concurrency support leveraging Erlang's actor model
- Modular architecture for easy extension
- Built-in tools and utilities for game server development
- Configurable server components
- Real-time communication capabilities

## Project Structure

```
.
├── src/           # Source code
│   ├── server/    # Server core logic (HTTP/WebSocket)
│   ├── game/      # Game framework and behaviours
│   ├── base/      # Base functionality
│   ├── util/      # Utility functions
│   ├── tools/     # Development tools
│   ├── config/    # Configuration handling
│   └── lib/       # Library files
├── include/       # Header files
├── script/        # Scripts
├── config/        # Configuration files
└── rebar.config   # Build and dependency config
```

## Requirements

- Erlang/OTP 24.0 or later
- rebar3 (for build management)

## Quick Start

1. Clone the repository
2. Build the project:
   ```bash
   rebar3 compile
   ```
3. Start the server:
   ```bash
   rebar3 shell
   ```

## HTTP API

### Health Check
- **GET** `/health`
- **Response:** `{ "status": "ok" }`

### Game API (Demo)
- **POST** `/api/game`
- **Body:** JSON `{ "action": "start" }`
- **Response:** `{ "status": "success" }` or error JSON

## WebSocket API

- **Endpoint:** `/ws`
- **Protocol:** JSON text frames

### 支持的消息类型

#### 1. 加入游戏
```json
{ "type": "join", "game_id": "game1" }
```
- **响应：** `{ "status": "joined" }`

#### 2. 游戏动作
```json
{ "type": "action", "action": "move" }
```
- **响应：** `{ "status": "action_received" }`

#### 3. 错误消息
- **格式：** `{ "error": "invalid_message" }` 或 `{ "error": "unknown_message_type" }`

## How to Extend

- 实现 `game_server_behaviour` 行为模块，定义你的游戏逻辑。
- 通过 `game_server_framework` API 管理游戏房间和玩家。
- 可自定义 HTTP/WebSocket 路由和协议。

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Hot Code Upgrade (热更新与升级)

Erlang/OTP 原生支持代码热加载和应用级热升级。

### 1. 热加载模块（开发/调试阶段）
- 在 Erlang shell 或远程节点中：
  ```erlang
  l(game_server_framework).
  l(my_custom_game).
  %% 或
  code:load_file(game_server_framework).
  ```
- 进程会自动切换到新代码，状态通过 `code_change/3` 回调平滑迁移。

### 2. 平滑升级状态
- 在 `code_change/3` 回调中处理 record/数据结构变更：
  ```erlang
  code_change(_OldVsn, State, _Extra) ->
      %% 这里可以做状态结构的转换
      {ok, State}.
  ```

### 3. Release 级热升级
- 使用 `rebar3 release` 生成 release 包。
- 编写 `src/game_server.appup` 文件，描述升级/降级步骤。
- 使用 `bin/game_server upgrade <version>` 实现平滑升级。

#### appup 文件示例：
```erlang
{
  "0.2.0", [
    {update, game_server_framework, supervisor},
    {update, game_server_worker, supervisor}
  ],
  [
    {update, game_server_framework, supervisor},
    {update, game_server_worker, supervisor}
  ]
}.
```

更多 Erlang 热升级资料请参考官方文档或社区教程。
