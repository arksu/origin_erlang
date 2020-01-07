-module(game_server).


-export([start_link/0, status/0]). 

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).

-export([start0/1, sock_handler/2]).

-include("defines.hrl").
-include("base_io.hrl").
-include("net.hrl").
 
start_link() ->
	Cfg = config:get_config(),
	GameServerPort = config:get_value(Cfg, game_server_port), 
    P = spawn_link(?MODULE, start0, [GameServerPort]),
    register(game_server, P),
    {ok, P}. 
 
start0(Port) ->
    process_flag(trap_exit, true),
    ?DEBUG("~p starting..." ,[?MODULE]),
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSock} ->
        ?DEBUG("~p started" ,[?MODULE]),
        P = spawn_link(?MODULE, sock_handler, [self(), LSock]),
        sock_loop(P, 1, nil, LSock);
    Other ->
        ?ERROR_MSG("case gen_tcp:listen other=~p",[Other]),
        exit(Other)
    end.

status() ->
    game_server ! {self(), status},
    receive 
    {_, Status} ->
        Status
    after 1000 ->
        timeout
    end.

sock_loop(P, Procs, LastError, LSock) ->
    receive
    {P, connected} ->
        P2 = spawn_link(?MODULE, sock_handler, [self(), LSock]),
        sock_loop(P2,  Procs+1, LastError, LSock);
    {'EXIT', P, {lasterror, Reason}} ->
        P2 = spawn_link(?MODULE, sock_handler, [self(), LSock]),
        sock_loop(P2, Procs, Reason, LSock);
    {'EXIT', _, {lasterror, Reason}} ->
        ?DEBUG("game_server process exit"),
        sock_loop(P, Procs-1, Reason, LSock);
    {'EXIT', _, _} ->
        ?DEBUG("game_server process exit"),
        sock_loop(P, Procs-1, LastError, LSock);
    {From, status} ->
        From ! {self(), {ok, [{procs, Procs}, {lasterr, LastError}]}},
        sock_loop(P, Procs, nil, LSock);
    {_From, stop} ->
        ?DEBUG("~p stopped" ,[?MODULE]),
        exit(stop) 
    end.

sock_handler(Top, S) ->
    case gen_tcp:accept(S) of
    {ok, Socket} -> 
        Top ! {self(), connected},
        ?DEBUG("connected: ~w",[Socket]),
        Pid = case player_server:spawn_player(Socket) of 
            err ->
                lasterror({spawn_player, failed});
            P ->
                P
        end,
        link(Pid),
        send_raw_packet(Socket, ?GAMESERVER_HELO, write_int(333)),
        work_loop(Socket, Pid);  
    {error, Reason} ->
        lasterror({noaccept, S, Reason}) 
    end.

work_loop(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, B} ->
        Bin = list_to_binary(B),
        %?DEBUG("get packet: ~w ~s",[Socket,bin_to_hex(Bin)]),
        case player_server:handle_packet(Pid, Socket, Bin) of
            ok ->
                work_loop(Socket, Pid);
            _ ->
                lasterror({badresult, player_packet})
        end;
    {error, Reason} ->
        lasterror({badread, Reason})
    end.

lasterror(R) ->
    exit({lasterror, R}).