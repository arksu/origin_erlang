%% Author: arksu
%% Created: 14.01.2011
-module(login_server).
-include("mysql.hrl").

-export([start_link/0, status/0, net_send_loop/1]).


-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(PROTO_VERSION, 2).

-define(LOGINSERVER_HELO,           1).
-define(LOGINSERVER_PROTO,          2).
-define(LOGINSERVER_PROTO_ACK,      3).
-define(LOGINSERVER_AUTH,           4). 
-define(LOGINSERVER_AUTH_ACK,       5).
-define(LOGINSERVER_GETCOOKIE,      6).
-define(LOGINSERVER_GETCOOKIE_ACK,  7).
-define(LOGINSERVER_BYE,            8).
-define(LOGINSERVER_GETCHARS,       9).
-define(LOGINSERVER_GETCHARS_ACK,   10).
-define(LOGINSERVER_PING,           11).


-record(client_state,
    {
      ust = invalid,              %% internal state
      user = "",
      password = "",
      accountid = 0,
	  last_char = 0,
	  send_pid = none
    }).

-export([start0/1, sock_handler/2, parse_chars/2]).

-include("defines.hrl").
-include("base_io.hrl").
-include("net.hrl").

start_link() ->
	Cfg = config:get_config(),
	LoginServerPort = config:get_value(Cfg, login_server_port), 
    P = spawn_link(?MODULE, start0, [LoginServerPort]),
    register(login_server, P),
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
    login_server ! {self(), status},
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
        sock_loop(P, Procs-1, Reason, LSock);
    {'EXIT', _, _} ->
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
		P = spawn_link(?MODULE, net_send_loop, [Socket]),
        send_packet(P, ?LOGINSERVER_HELO, write_int(-7756)),

        hloop(Socket, #client_state{send_pid = P}) ;
    {error, Reason} ->
        lasterror({noaccept, S, Reason})
    end.

net_send_loop(Socket) ->
	receive
		{send, Data} ->
			?DEBUG("login send ~p",[Data]),
			case gen_tcp:send(Socket, Data) of
				ok -> net_send_loop(Socket);
				{error, Reason} ->
					?DEBUG("login send error: ~p",[Reason]),
					exit({net_send_error, Reason})
			end;
		{kill} -> ok
	end.

hloop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
    {ok, B} ->
        Bin = list_to_binary(B),
        ?DEBUG("~s",[bin_to_hex(Bin)]),
        StateNew = process_data(State, Socket, Bin),
        hloop(Socket, StateNew);

    {error, Reason} ->
        lasterror({badread, Reason})
    end.

lasterror(R) ->
    exit({lasterror, R}).

process_data(#client_state{send_pid=SendPid} = St, _Socket, Bin) ->
	{_L, B0} = read_word(Bin),
    {Id, B1} = read_byte(B0),
    ?DEBUG("parse packet id=~p", [Id]),
    case Id of
        ?LOGINSERVER_PROTO ->
            ?DEBUG("recv LOGINSERVER_PROTO"),
            {I2, _B2} = read_int(B1),
            if I2 =:= ?PROTO_VERSION ->
                send_packet(SendPid, ?LOGINSERVER_PROTO_ACK, write_int(1)),
                St#client_state{ust = proto};
            true ->
                send_packet(SendPid, ?LOGINSERVER_PROTO_ACK, write_int(0)),
                St#client_state{ust = invalid_proto}
            end;

        ?LOGINSERVER_PING ->
            ?DEBUG("recv login ping"),
            #client_state{accountid = AccID, ust = Ust} = St,
            case Ust of
                auth_ok -> db_server:login_update_time(AccID);
                chars -> db_server:login_update_time(AccID);
                _ -> ok
            end,
            St;

        ?LOGINSERVER_AUTH ->
            ?DEBUG("recv LOGINSERVER_AUTH"),
            #client_state{ust = S} = St,
            if S =/= proto -> lasterror({bad_auth_state}); true -> ok end,
            {Login, B2} = read_string(B1),
            {Pwd, _} = read_string(B2),
            ?DEBUG("login: ~p pwd: ~p",[Login, Pwd]),
            case auth(Login, Pwd) of
            {ok, AccID, LastChar} ->
                    ?DEBUG("ok"),
					db_server:player_logged_result(Login, AccID, true),
                    send_packet(SendPid, ?LOGINSERVER_AUTH_ACK, write_int(1)),
                    ?DEBUG("auth ok! accid = "++integer_to_list(AccID)),
                    St#client_state{ust = auth_ok, password = Pwd, user = Login, accountid = AccID, last_char=LastChar };
            {already_logged, AccID} ->
                    ?DEBUG("already_logged"),
					db_server:player_logged_result(Login, AccID, already_logged),
                    send_packet(SendPid, ?LOGINSERVER_AUTH_ACK, write_int(3)),
                    St#client_state{ust = already_logged, password = Pwd, user = Login};
            {not_found} ->
                    ?DEBUG("not_found"),
					db_server:player_logged_result(Login, 0, not_found),
                    send_packet(SendPid, ?LOGINSERVER_AUTH_ACK, write_int(2)),
                    St#client_state{ust = user_not_found};
			{fail_account_online, AccID} ->
                    ?DEBUG("fail_account_online"),
					db_server:player_logged_result(Login, AccID, fail_account_online),
                    send_packet(SendPid, ?LOGINSERVER_AUTH_ACK, write_int(4)),
                    St#client_state{ust = user_not_found};
            Other ->
                    ?DEBUG("auth error ~p",[Other]),
					db_server:player_logged_result(Login, 0, bad_pass),
                    send_packet(SendPid, ?LOGINSERVER_AUTH_ACK, write_int(0)),
                    St#client_state{ust = wrong_pass}
            end;
        ?LOGINSERVER_GETCHARS ->
            ?DEBUG("recv LOGINSERVER_GETCHARS"),
            #client_state{ust = S, accountid = AccID, last_char=LastChar } = St,
            if S =/= auth_ok -> lasterror({bad_getchars_state}); true -> ok end,
            case db_server:login_get_chars(AccID) of
                [] ->
                    ?DEBUG("no chars"),
                    send_packet(SendPid, ?LOGINSERVER_GETCHARS_ACK, write_int(0)),
                    St#client_state{ust = no_chars};
                Chars ->
                    ?DEBUG("send chars..."++integer_to_list(length(Chars))),
                    db_server:login_update_time(AccID),
                    Pkt = list_to_binary(parse_chars(lists:reverse(Chars), [])),
                    send_packet(SendPid, ?LOGINSERVER_GETCHARS_ACK,
                                [write_int(length(Chars)),write_int(LastChar),
                                Pkt]),
                    St#client_state{ust = chars}
            end;
        ?LOGINSERVER_GETCOOKIE ->
            ?DEBUG("recv LOGINSERVER_GETCOOKIE"),
            {CharID, _B2} = read_int(B1),
            #client_state{ust = S, password = P, user = U, accountid = AccID} = St,
            if S =/= chars -> lasterror({bad_getcookie_state}); true -> ok end,
            case db_server:login_get_cookie(U, P, CharID) of
                [] ->
                    lasterror({bad_getcookie_state});
                Cookie ->
                    db_server:login_update_time(AccID),
                    Cfg = config:get_config(),
                    GameServer = config:get_value(Cfg, game_server),
					GameServerPort = config:get_value(Cfg, game_server_port), 
					?DEBUG("game server port = ~p", [GameServerPort]),
                    send_packet(SendPid, ?LOGINSERVER_GETCOOKIE_ACK,
                                [write_int(1),
                                Cookie,
                                write_string(GameServer),
								write_word(GameServerPort)
								]),
                    St#client_state{ust = cookie}
            end
    end.

auth([], _) -> {not_found};
auth(Login, Pwd) ->
	?DEBUG("try auth"),
    case db_server:login_get_pass(string:to_lower(Login)) of
        [] ->
            {not_found};
        {Pwd, AccID, LastChar} ->
			case player_server:is_account_online(AccID) of
				error -> {fail_account_online, AccID};
				true -> {already_logged, AccID};
				false ->
					db_server:login_update_time(AccID),
					{ok, AccID, LastChar}
			end;
        _ ->
            {err}
    end.

parse_chars([],Acc) ->
    Acc;
parse_chars(Chars,Acc) ->
    [H|T] = Chars,
    [ID, Name] = H,
    parse_chars(T, [[write_int(ID), write_string(binary_to_list(Name))]|Acc]).

