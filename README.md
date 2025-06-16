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
│   ├── server/    # Server core logic
│   ├── base/      # Base functionality
│   ├── util/      # Utility functions
│   ├── tools/     # Development tools
│   ├── config/    # Configuration handling
│   └── lib/       # Library files
├── include/       # Header files
├── script/        # Scripts
└── config/        # Configuration files
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

## License

MIT License
