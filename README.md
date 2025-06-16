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
