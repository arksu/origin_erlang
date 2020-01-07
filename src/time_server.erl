%%% -------------------------------------------------------------------
%%% Author  : arksu
%%% Description :
%%%
%%% Created : 14.10.2011
%%% -------------------------------------------------------------------
-module(time_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, time_generator_loop/0, time_generator_start/0, get_time/0
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("defines.hrl").

-record(state, {
					 ping_time = 0, % аккумулятор времени пинга
					 online_time = 0, % аккумулятор времени апдейта онлайна в базе данных
					 last_tick_count = 0,
					 generator_pid = error,
					 timer = 0, % аккумулятор для подсчета глобального времени
					 global_time = 0, % глобальное время сервера (в секундах от старта мира)
					 db_update_time = 0 % аккумулятор таймер апдейта времени в бд
				}).


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% получить текущее время сервера
% блокирующий вызов
get_time() ->
	?MODULE ! {get_time, self()},
	receive
		{get_time_ack, Time} -> Time
	after 5000 -> {error, timeout_get_time}
	end.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ?DEBUG("~p started~n" ,[?MODULE]),
	a1_utils:wait_servers([db_request, player_server, world_server]),
	%db_server:wait(),
	%sleep(2000),
	GeneratorPid = spawn_link(?MODULE, time_generator_start, []),
    {ok, #state{
				last_tick_count		= get_tick_count(),
				global_time 		= db_server:get_server_time(),
				generator_pid 		= GeneratorPid
				}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
	?WARNING_MSG("unhandled call ~p from ~p",[Request, From]),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	?WARNING_MSG("unhandled cast ~p",[Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
% обработать тик

%{state,
%% ping_time = 1713,
%% online_time = 1713,
%% last_tick_count = 562798157,
%% generator_pid = <0.70.0>,
%% timer = 713,
%% global_time = 10055948  + (713 + 334) div 1000
%% db_update_time = 4676}


handle_info({current_time_tick, NewTime}, #state{
				online_time = OnlineTime,
				ping_time = PingTime,
				last_tick_count=Time,
				global_time = GlobalTime,
				timer = OldTimer,
				db_update_time = DBTime
												} = State) ->
	DT = NewTime - Time,
	Timer = OldTimer + DT,

	%?DEBUG("gl=~p timer=~p",[GlobalTime, Timer]),
	NewGlobalTime = GlobalTime + Timer div 1000,

	NewTimer = Timer rem 1000,
    NewOnlineTime = update_online(OnlineTime+DT), 

    %?DEBUG("tick ~p", [DT]),
    player_server ! {tick, DT},
	world_server ! {tick, DT, NewGlobalTime},

    NewPingTime = update_ping(PingTime+DT, DT),
	% обновляем время в бд только каждые N секунд. не каждый тик
	NewDBTime = if DBTime + DT > 5000 ->
		db_server:update_server_time(NewGlobalTime),
		0;
	true ->
		DBTime + DT
	end,

    {noreply, State#state{ping_time = NewPingTime,
						  online_time = NewOnlineTime,
						  last_tick_count = NewTime,
						  global_time = NewGlobalTime,
						  db_update_time = NewDBTime,
						  timer = NewTimer
						 }};

% получить текущее время сервера
handle_info({get_time, From}, #state{global_time=GTime} = State) ->
	%?DEBUG("handle get_time from ~p",[From]),
	From ! {get_time_ack, GTime},
	{noreply, State};

handle_info(Info, State) ->
	?WARNING_MSG("unhandled info ~p",[Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_tick_count() ->
    {_,S,M} = now(),
    (S*1000)+(M div 1000).

time_generator_start() ->
	?DEBUG("time worker started"),
	time_generator_loop().

% генератор тиков
time_generator_loop() ->
	sleep(?TIMER_RESOLUTION),
	?MODULE ! {current_time_tick, get_tick_count()},
	time_generator_loop().

%% --------------------------------------------------------------------
update_online(OnlineTime) when OnlineTime < ?UPDATE_ONLINE_TIME -> OnlineTime;
update_online(OnlineTime) ->
	%?DEBUG("update_online"),
    %player_server:update_online(),
    OnlineTime-?UPDATE_ONLINE_TIME.

%% --------------------------------------------------------------------
update_ping(Time,_) when Time < ?PING_TICK -> Time;
update_ping(Time, DT) ->
	%?DEBUG("ping_tick"),
    player_server ! {ping_tick, DT},
    Time - ?PING_TICK.