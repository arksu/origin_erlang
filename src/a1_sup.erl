%%% -------------------------------------------------------------------
%%% Author  : arksu
%%% Description :
%%%
%%% Created : 31.01.2011
%%% -------------------------------------------------------------------
-module(a1_sup).

-behaviour(supervisor).

-export([
	 init/1,
     start_link/0
        ]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DB =
    {db_server, {db_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        []},

    RandomsServer =
    {randoms, {randoms, start_link, []},
        permanent,
        brutal_kill,
        worker,
        []},
    LoginServer =
    {login_server, {login_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        []},
    GameServer =
    {game_server, {game_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        []},
    Players =
    {player_server, {player_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        []},

    WorldServer =
    {world_server, {world_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [world_server]}, % << its gen_server

    TimeServer =
    {time_server, {time_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [time_server]}, % << its gen_server

	{ok, {{one_for_one, 10, 1}, [
       DB,
	   RandomsServer,
	   WorldServer,
       Players,
       TimeServer,
       GameServer,
       LoginServer
    ]}}.
