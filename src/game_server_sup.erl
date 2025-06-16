-module(game_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    
    ChildSpecs = [
        #{
            id => game_server_worker,
            start => {game_server_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [game_server_worker]
        },
        #{
            id => game_server_framework,
            start => {game_server_framework, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [game_server_framework]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}. 