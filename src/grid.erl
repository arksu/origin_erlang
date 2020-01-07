%% Author: arksu
%% Created: 13.02.2012

-module(grid).

-export([start_link/2, start_grid/1,
		 worker_map_load/2, worker_obj_load/2, worker_claims_load/2]).

-export([
		check_collision/5,
		find_object_neighbors/2,
		object_spawn/2,
		object_changed/4,
		object_set_param_notify/3,
		object_set_param/3,
		object_remove_param_notify/3,
		get_claim_grids/2,
		get_claim_gridsx/5,
		get_claim_gridsy/6,
		
		obj_inventory_click/6,
		obj_inventory_context/7,

		send_all_notify/2
		]).

-include("defines.hrl").
-include("map.hrl").
-include("types.hrl").
-include("objects.hrl").

-define(GRID_MIN_UPDATE_TIME, 9). % для тестов сделаем апдейт каждые 2 минуты  
-define(GRID_MAX_UPDATE_COUNT, 15). % всего апдейтим максимум 6 раз при загрузке
-define(GRID_TIMEOUT_WAIT_PROCESS, 15). % сколько ждем апдейта грида, перед тем как упасть с ошибкой



%----------------------------------------------------------------------------------------------------
% динамический процесс. живой объект или игрок
-record(dynamic_pid, {
				pid=error,			% ид процесса
				linked=[],			% список связанных динамических процессов [#dynamic_pid], те что двигаются вместе с этим объектом (лодка, игрок тащит на себе чтото)
				objid=error,		% ид объекта
				account_id=0		% ид аккаунта. используется только для игроков. у обычных объектов = 0
		}).

%%--------------------------------------------------------------------------------------------------
% заспавнить грид
start_link(Sg, Grid) ->
	spawn_link(?MODULE, start_grid, [#grid_state{coord={Sg, Grid}, level=map_server:get_level(Grid)}]).

%%--------------------------------------------------------------------------------------------------
% инициализация грида
start_grid(#grid_state{coord=G} = State) ->
	?DEBUG("spawn grid process ~p",[G]),
	% ставим ловушку на exit, будем ловить краши игроков удаляя их из подписчиков
	process_flag(trap_exit, true),
	% при старте грид должен себя загрузить: тайлы и объекты
	PMap = spawn(?MODULE, worker_map_load, [G, self()]),
	PObj = spawn(?MODULE, worker_obj_load, [G, self()]),
	PClaims = spawn(?MODULE, worker_claims_load, [G, self()]),
	main_loop(State#grid_state{worker_map=PMap, worker_obj=PObj, worker_claims=PClaims}).

%%--------------------------------------------------------------------------------------------------
% основной цикл
main_loop(State) ->
	receive
		M -> main_loop(handle_msg(State, M))
	end.

%%--------------------------------------------------------------------------------------------------
% обработать сообщение
handle_msg(State, []) -> State;
handle_msg(State, List) when is_list(List) ->
	[M, T] = List,
	handle_msg(handle_msg(State, M), T);
handle_msg(
  #grid_state{is_loaded=IsLoaded} =
  State, M) ->
	%?DEBUG("grid handle msg ~p",[M]),
	case M of
		{claim_remove, FromObjID} ->
			claim_remove(State, FromObjID);
		{claim_change, NewClaim} ->
			claim_change(State, NewClaim);
		{get_claims, From} ->
			claim_send_all(State, From), State;
		
		{move_request_unlock, LockArea, Taked, ObjID, X, Y, SetParam, AllNotify} ->
			S1 = unlock_area(State, LockArea),
			% добавим ему объект взятый из моего стейта
			ThatState0 = put_object(S1, Taked),
			% ставим ему в стейт позицию, с уведомлением всех подписчиков
			case SetParam of
				none -> ThatState0;
				pos ->
					object_pos(ThatState0, ObjID, X, Y, AllNotify);
				Param ->
					object_set_param(ThatState0, ObjID, Param, AllNotify)
			end;		
					  
		{take_object, ObjID, From} ->
			{Taked, S1} = take_object(State, ObjID),
			From ! {take_object_ack, Taked},
			S1;
		
		{send_all_dyns, Msg} ->
			#grid_state{dynamic_pids=Dyn} = State,
			send_all_dyns(Dyn, Msg),
			State;
		
		{obj_visual_state_ack, From, FromObjID, FromState, ObjID, ObjMsg} ->
			obj_action(State, ObjMsg, From, FromObjID, FromState, ObjID);
		
		{player_close_obj, From, ObjID} ->
			player_close_obj(State, From, ObjID);

		{object_say, Say} ->
			object_say(State, Say);

		{send_notify, Msg} ->
			#grid_state{notify = Notify} = State,
			send_all_notify(Notify, Msg),
			State;

		% найти объект используя фильтр
		{find_filter_objects, Fun, Rect, From} ->
			Ack = find_filter_objects(State, Fun, Rect),
			From ! case Ack of none -> {none, State#grid_state.coord}; _ -> {ack, {find_filter_objects_ack, Ack}} end,
			State;

		{find_object, From, ObjID} ->
			?DEBUG("find object from=~p objid=~p", [From, ObjID]),
            case find_object(State, ObjID) of
                none -> From ! {none, State#grid_state.coord};
                Object -> From ! {ack, {object_found, Object}}
            end,
            State;

		% бросить вещь на землю
		{object_drop, Item, X, Y} ->
            object_drop(State, Item, X, Y);

		{get_tile, From, X, Y} ->
			#grid_state{level=Lv, coord=GC} = State,
			TGC = map_server:get_sg_grid(X, Y, Lv),
			if
				TGC == GC ->
					From ! {ack, {get_tile_ack, get_tile(State, X, Y)}};
				true ->
					From ! {none, GC}
			end,
			State;

		{update_tile, TGC, Index, Tile} ->
			#grid_state{coord= {Sg, Grid} = GC, tiles=Tiles, notify = Notify} = State,
			if
				GC == TGC ->
					NewTiles = cutils:set_tile(Tiles, Index, Tile),
					db_server:map_set_grid(Sg, Grid, NewTiles),
					S1 = State#grid_state{tiles=NewTiles},
					send_all_notify(Notify, {grid_state, S1}),
					S1;
				true -> State
			end;

		{plow_tile, X, Y} ->
			% вспахать тайл
			plow_tile(State, X, Y);

        {obj_set_param, Object, Param} ->
            ?DEBUG("GRID: obj_set_param ~p ~p", [Object, Param]),
            object_set_param_notify(State, Object, Param);

		{obj_remove, ObjID} ->
			object_remove(State, ObjID, true);

		{object_spawn, Object} ->
			object_spawn(State, Object);
		{object_spawn, Where, Object} ->
			object_spawn(State, Where, Object);

        {obj_remove_confirm, From, ObjID} ->
            ?DEBUG("GRID: obj_remove confirm ~p", [ObjID]),
            object_remove_confirm(State,From,ObjID);

		{player_spawn_object, From, Type, X, Y, Q, HP, ObjState} ->
			player_spawn_object(State, From, Type, X, Y, Q, HP, ObjState);

		% обработать запрос на действие
        {obj_action, Action, From, FromObjID, FromState, ObjID} ->
            obj_action(State, Action, From, FromObjID, FromState, ObjID);

		% действие с объектом закончено
        {action_completed, From, FromObjID, FromState, Name, ObjID} ->
            obj_action_completed(State, From, FromObjID, FromState, Name, ObjID);

		% выбрано контекст действие
        {context_action, From, FromState, ObjID, Action} ->
            obj_context_action(State, From, FromState, ObjID, Action);


		% запрос на движение
		{move_request, From, ObjID, Type, VirtualType, OldX, OldY, X, Y, SetParam, MoveType} ->
			move_request(State, From, ObjID, Type, VirtualType, OldX, OldY, X, Y, SetParam, MoveType);
		
		% запрос на обсчет коллизии
		{check_collision, From, Type, X, Y, MoveType} ->
			From ! {check_collision_ack, check_collision(State, Type, X, Y, MoveType)},
			State;

		% запросить инфо об объекте
        {request_obj_info, From, ObjID} ->
            %?DEBUG("request_obj_info objid ~p", [ObjID]),
            request_obj_info(State, From, ObjID),
            State;

        % player request objects in rect, send obj_pos for all finded objects
        {get_objects_in_rect, From, VisibleZone} ->
            %?DEBUG("get_objects_in_rect VisibleZone ~p", [VisibleZone]),
            get_objects_in_rect(State, From, VisibleZone),
            State;

		% вернуть игроку список объектов по списку ид, вышлем позицию объектов
        {get_objects_in_list, From, List} ->
            %?DEBUG("get_objects_in_rect VisibleZone ~p", [VisibleZone]),
            get_objects_in_list(State, From, List),
            State;

		% добавить объект игрока в грид с учетом коллизий
		{player_add, Object, AccountID, From, Collision} ->
			player_add(State, Object, AccountID, From, Collision);

		% инфо - чисто для дебага
		{get_info} ->
			?DEBUG("state ~p",[State#grid_state{tiles=[]}]), State;

		% залочить с получением стейта, ждем разблокировки
		{lock_data, From, LockArea} ->
			try_lock_data(State, From, LockArea);				
		{lock_fail} -> State;
		{unlock, _Locked, LockArea} -> 
			unlock_area(State, LockArea);

		% уведомить о загрузке, привязаться - считаем что я нужен кому то
		{on_load, From} ->
			#grid_state{linked=Linked} = State,
			%?DEBUG("on_load from ~p loaded: ~p",[From, IsLoaded]),
			case IsLoaded of
				true ->
					% если уже загружены - шлем свой стейт
					From ! {grid_state, State},
					State#grid_state{linked=[From|Linked]};
				false ->
					% если нет - добавляем в список тех кого уведомим о загрузке
					OnLoad = State#grid_state.on_load,
					case lists:member(From, OnLoad) of
						true -> State#grid_state{linked=[From|Linked]};
						false -> State#grid_state{on_load=[From|OnLoad], linked=[From|Linked]}
					end
			end;

		% я больше не нужен кому то. разлинкуемся
		{unload, From} ->
			#grid_state{linked=Linked} = State,
			State#grid_state{linked=lists:delete(From, Linked)};

		% получить стейт
		{get_state, From} ->
			if IsLoaded == false -> ?WARNING_MSG("grid not loaded! ~p", [State]); true -> ok end,
			From ! {grid_state, State}, State;

		% установить стейт
		{set_state, NewState} -> NewState;

		% получен стейт
		{grid_state, #grid_state{coord=NGC}=NeighborState} ->
			#grid_state{neighbors=Neighbors} = State,
			case lists:keytake(NGC, #neighbor_rec.coord, Neighbors) of
				false ->
					% пришел стейт не от моего соседа - крит
					?WARNING_MSG("grid state not from my neighbor! ~p",[NGC]),
					% это не критичная ситуация. падать тут незачем
					%error(grid_state_not_my_neighbor);
					State;

				{value, #neighbor_rec{state=none}=N, Tail} ->
					% сосед еще не загружен
					% обновим соседа, он загружен
					NewNeighbors = [ N#neighbor_rec{is_loaded=true, state_recv_time=time_server:get_time(), state=NeighborState#grid_state{neighbors=[]}} | Tail ],
					% только если все загружено - проверим загрузку всех соседей
					check_neighbors_loaded(State#grid_state{neighbors=NewNeighbors});

				{value, N, Tail} ->
					% обновим состояние соседа
					State#grid_state{neighbors=[
										N#neighbor_rec{is_loaded=true, state_recv_time=time_server:get_time(), state=NeighborState#grid_state{neighbors=[]}}
																 |Tail]}
			end;

		% активировать грид
		{activate, From} ->
			activate(State, From);

		% деактивировать грид
		{deactivate, From} ->
			deactivate(State, From);

		% процесс нормально завершился
		{'EXIT', Pid, normal} ->
			?DEBUG("exit normal msg ~p", [Pid]),
			State;

		% один из связанных процессов крашнулся
		{'EXIT', Pid, Reason} ->
			?DEBUG("exit ~p ~p",[Pid, Reason]),
			OldNotify = State#grid_state.notify,
			% ищем в списке подписчиков
			case lists:member(Pid, OldNotify) of
				true ->
					?DEBUG("exit notify ~p",[Pid]),
					% удалим подписчика и проверим пуст ли список
					deactivate(State, Pid);
				false ->
					% это не наш подписчик - завершаем процесс
					?WARNING_MSG("trap exit ~p reason: ~p",[Pid,Reason]),
					error({trap_exit, Pid, Reason})
			end;

		% тайлы загружены
		{map_loaded, fail} -> error(error_map_load); % ошибка загрузки карты
		{map_loaded, Data, Time} ->
			%?DEBUG("map loaded ~p", [Time]),
			check_loaded(State#grid_state{tiles=Data, worker_map=done, time_updated=Time});

		% объекты загружены
		{obj_loaded, List} ->
			check_loaded(add_objects(State#grid_state{worker_obj=done}, List));
		
		{claims_loaded, List} ->
			check_loaded(State#grid_state{worker_claims=done, claims=List});
		
		% апдейт
		{tick, DT, Time} ->
			tick(State, DT, Time);

		{is_active, From} -> From ! {is_active_ack, State#grid_state.is_active}, State;
		{is_live, From} -> From ! {live}, State;

		{test_error} -> error(test_error);
		_ ->
			?WARNING_MSG("unhandled msg ~p", [M]), State
	end.

%%--------------------------------------------------------------------------------------------------
% запросить активацию грида на подписчика
activate(
  #grid_state{ notify=Notify, coord=GC, is_active=IsActive }=
  State, From) ->
	% линкуемся с подписчиком. на случай краша
	link(From),

	S1 = if % проверим активен ли грид
		IsActive==false ->
			% получаем список соседних гридов
			Grids = lists:delete(GC, a1_utils:get_grids(GC)),

			% добавляем в список подписчиков, добавляем гриды в соседей
			State#grid_state{is_activate_requested=true, notify=[From|Notify], neighbors=add_neighbors(Grids, [])};
		true ->
			% грид уже активен
			From ! {activated, GC},
			State#grid_state{is_activate_requested=true, notify=[From|Notify]}
	end,
	check_neighbors_loaded(S1).

%%--------------------------------------------------------------------------------------------------
% деактивировать грид подписчиком
deactivate(#grid_state{notify=OldNotify, neighbors=Neighbors, dynamic_pids=OldDyns, locked=LA} = State, From) ->
	% разрываем связь с подписчиком
	unlink(From),
	% удаляем из подписчиков
	Notify = lists:delete(From, OldNotify),
	
	NewLA = a1_utils:keydelete(From, #grid_lock_area.owner, LA),

	% удаляем из дин списка
	{OID, Dyns} = case lists:keytake(From, #dynamic_pid.pid, OldDyns) of
		false -> {[], OldDyns};
		{value, #dynamic_pid{objid=ObjID}, Tail} -> {ObjID, Tail}
	end,

	% уведомим все динамические объекты
	lists:foreach(fun
					 (#dynamic_pid{pid=DP}) -> DP ! {deactivate_grid, self()}
				  end, Dyns),
	
	if Notify == [] ->
		   	if NewLA =/= [] -> error(locked_not_empty);
			true ->
				% разорвать связи с соседями
				lists:foreach(fun (#neighbor_rec{pid=P}) -> P ! {unload, self()} end, Neighbors),
				% подписчиков больше нет. деактивируем грид
				object_remove(State#grid_state{neighbors=[], is_active=false, notify=[], dynamic_pids=Dyns}, OID)
			end;
	true ->
		object_remove(State#grid_state{notify=Notify, dynamic_pids=Dyns, locked=NewLA}, OID)
	end.

%%--------------------------------------------------------------------------------------------------
% проверить загружен ли грид. уведомить всех кого надо
check_loaded(#grid_state{worker_map=WMap, worker_obj=WObj, worker_claims=WClaims, on_load=OnLoad}=State) ->
	% если все загружено - уведомим
	if
		(WMap==done) andalso (WObj==done) andalso (WClaims==done) ->
			%?DEBUG("grid loaded!"),
			SNew = State#grid_state{on_load=[], is_loaded=true},
			% уведомляем о загрузке
			lists:foreach(fun (P) -> P ! {grid_state, SNew} end, OnLoad),
			
			check_neighbors_loaded(SNew);
		true ->
			State
	end.

%%--------------------------------------------------------------------------------------------------
% рабочий по загрузке карты
worker_map_load({Sg, Grid}, Owner) ->
	case db_server:map_get_grid(Sg, Grid) of
		[] ->
			?CRITICAL_MSG("grid not found in db! sg=~p grid~p", [Sg, Grid]),
			Owner ! {map_loaded, fail};
 		{Data, Time} ->
			% шлем данные тому кто запросил
			Owner ! {map_loaded, Data, Time}
	end.

%%--------------------------------------------------------------------------------------------------
% рабочий для загрузки объектов из базы
worker_obj_load({Sg, Grid}, Owner) ->
	% загрузим из бд
	case db_server:objects_all_load_grid(Sg, Grid) of
    	err -> Owner ! {obj_loaded, []}; % по гриду в бд может и не быть данных. продолжаем работу
    	Rows ->
			%?DEBUG("send objects from db count=~p",[length(Rows)]),
			Owner ! {obj_loaded, parse_obj_rows(Sg, Rows, [])}
    end.

%%--------------------------------------------------------------------------------------------------
worker_claims_load({Sg, Grid}, Owner) ->
	L = load_claims(Sg, Grid),
	?DEBUG("loaded claims: ~p ~p ~p",[Sg, Grid, L]),
	Owner ! {claims_loaded, L}.

%%--------------------------------------------------------------------------------------------------
% распарсим результат из базы
parse_obj_rows(_, [], Acc) -> Acc;
parse_obj_rows(Sg, [Row|Tail], Acc) ->
	parse_obj_rows(Sg, Tail, [objects_server:parse_row_obj(Sg, Row) | Acc]).

%%--------------------------------------------------------------------------------------------------
% безусловное добавление объектов без проверки на дубликаты, использовать только при загрузке в грид
add_objects(S,[]) -> S;
add_objects(#grid_state{dyn=Dyn, tick=Tick, static=Static}=S, [#object{type=Type}=O|T]) ->
	case objects_server:get_time_type(Type) of
		static -> add_objects(S#grid_state{static=[O|Static]}, T);
		dyn -> add_objects(S#grid_state{dyn=[O|Dyn]}, T);
		tick -> add_objects(S#grid_state{tick=[O|Tick]}, T)
	end.

%%--------------------------------------------------------------------------------------------------
% добавить соседа
add_neighbor(GC) ->
	% получаем их пиды
	P = world_server:get_grid(GC),
	% шлем им "уведоми о загрузке"
	P ! {on_load, self()},
	% слинкуемся с этим гридом
	link(P),
	#neighbor_rec{coord=GC, pid=P}.

% добавить соседей по списку
add_neighbors([], Acc) -> Acc;
add_neighbors([GC|T], Acc) ->
	add_neighbors(T, [add_neighbor(GC)|Acc]).

%%--------------------------------------------------------------------------------------------------
% проверить загружены ли все соседи
check_neighbors_loaded(#grid_state{neighbors=NS, is_loaded=IsLoaded, is_activate_requested=Activated}=State) ->
	NeighborsLoaded = check_neighbor_loaded(NS),
	if  
		(NeighborsLoaded == true) andalso (IsLoaded == true) andalso (Activated == true) ->
			% грид уже активирован?
			case State#grid_state.is_active of
			false ->
				statistics(runtime),
			    statistics(wall_clock),
				T1 = get_tick_count(),

				SS = do_activate(State),

				% обработка завершилась успешно
				T2 = get_tick_count(),
				{_, Time1} = statistics(runtime),
		        {_, Time2} = statistics(wall_clock),
				?DEBUG("process done successfull! runtime=~p wall_clock=~p dt=~p",[Time1, Time2, T2-T1]),
				SS;
			true ->
				% апдейт грида по таймеру
				?DEBUG("check_neighbors_loaded PROCESS"),
				process_grid(State)
			end;
		true ->
			State
	end.

%%--------------------------------------------------------------------------------------------------
% непосредственная активация грида
do_activate(#grid_state{notify=Notify, coord=GC, time_updated=LastTime, dynamic_pids=Dyns}=State) ->
	Time = time_server:get_time(),
	% уведомим динамические объекты об активации
	lists:foreach(fun (#dynamic_pid{pid=DP}) -> DP ! {activate_grid, self()} end, Dyns),
	% при активации обновим тик объекты
	S0 = update_tick_objects(State, Time-LastTime, Time),
	S = if 
			(Time-LastTime > ?GRID_MIN_UPDATE_TIME) ->
				?DEBUG("do_activate : grid need process"),
			   	process_grid(S0, true); 
		   	true -> 
				?DEBUG("do_activate : grid process diff time = ~p", [Time-LastTime]),
				S0 
		end,
	?DEBUG("grid activated!"),
	% это инициализация грида
	a1_utils:send_msg(Notify, {activated, GC}),
	S#grid_state{is_active=true, time_tick_updated=Time}.

%%--------------------------------------------------------------------------------------------------
% загружен ли сосед
check_neighbor_loaded([]) -> true;
check_neighbor_loaded([#neighbor_rec{state=none}|_]) -> false;
check_neighbor_loaded([_|T]) -> check_neighbor_loaded(T).

%%--------------------------------------------------------------------------------------------------
tick(State, DT, Time) ->
	%?DEBUG("grid_tick ~p",[State#grid_state.coord]),
	#grid_state{is_active=IsActive, notify=Notify,
				dynamic_pids=Dyns,
				linked=Linked} = State,
	% если мы активны
	if (IsActive) ->
		if
			(length(Notify) > 0) ->

				% если есть подписчики и мы активны -
				% каждый тик обновляем динамические объекты 
				lists:foreach(fun
								 (#dynamic_pid{pid=P, account_id=0}) ->
									  % только если это не игрок. шлем ему тик, игроков обслуживает отдельный сервер и сам получает тики
									  P ! {tick, DT, Time};
								 (_) ->
									  ok
							  end, Dyns),

				% апдейтим таймер на обработку процессором, там ставим requested time в текущее время и ждем загрузки получения данных
		   		update_process_timer(
					% таймер обновления тик объектов
			   		update_tick_objects_timer(State, DT, Time),
									Time);

			% если нет подписчиков. мы активны. и linked пусто (мы никому не нужны) - апдейтим таймер выгрузки
			(Notify==[]) andalso (Linked==[]) ->
				update_unload_timer(State);
			true -> State
		end;
	% not active
	true -> State
	end.

%%--------------------------------------------------------------------------------------------------
process_grid(State) -> process_grid(State, false).
process_grid(#grid_state{time_updated=LastTime} = State, NeedSpawn) ->
	GTime = time_server:get_time(),
	TimeLen = GTime - LastTime,
	{DT, C} = if
			 (TimeLen / ?GRID_MAX_UPDATE_COUNT < ?GRID_MIN_UPDATE_TIME) ->
				 {?GRID_MIN_UPDATE_TIME, round(TimeLen / ?GRID_MIN_UPDATE_TIME)};
			 true ->
				 {TimeLen / ?GRID_MAX_UPDATE_COUNT, ?GRID_MAX_UPDATE_COUNT}
		 end,
	?DEBUG("process_grid: len=~p dt=~p count=~p",[TimeLen, DT, C]),
	if 
		C > 0 ->
			#grid_state{tiles=Tiles, coord={Sg, Grid}} = S1 = process_grid(State, DT, C, LastTime, GTime, NeedSpawn),
			% обновить время в базе
			db_server:map_set_grid(Sg, Grid, Tiles, GTime),
			S1;
		true ->
			State
	end.


process_grid(State, _, 0, _, _, _) -> State;
process_grid(State, _DT, _Count, Time, GTime, _) when (Time > GTime) -> State;
process_grid(State, DT, Count, Time, GTime, NeedSpawn) ->
	T = if (Time+DT > GTime) -> GTime; true -> Time+DT end,
	%?DEBUG("process grid dt=~p time=~p count=~p",[T-Time, T, Count]),
	Spawn = if 
				(NeedSpawn == true) andalso (Count == 1) -> 1;
				true -> 0
			end,
	process_grid(
	  process_grid(State, T, Spawn),
	  DT, Count-1, Time+DT, GTime, NeedSpawn).

% обновить грид процессором, с явным указанием времени
process_grid(#grid_state{coord={Sg,Grid}, neighbors=NS, notify=Notify, is_active=IsActive} = State, GridTime, NeedSpawn) ->
	?DEBUG("call grid_processor, spawn: ~p",[NeedSpawn]),
	% если активен - то это обновление уже прогруженного грида, т.е. не первый раз
	{ResultTilesChanged, ResultMap, ResultChanged, ResultDeleted, ResultNew} =
	grid_processor:process([#neighbor_rec{state=State}|NS], Sg, Grid, round(GridTime), NeedSpawn),
	%?DEBUG("##### process grid ~p ~p ~p " ,[ResultChanged, ResultDeleted, ResultNew]),

    % тут потом надо будет обновить измененные объекты. и добавить новые если такие были
	S1 = process_deleted_objects(State#grid_state{tiles=ResultMap}, ResultDeleted),
	% спавним новые объекты по списку
    S2 = object_spawn(S1, ResultNew),

	if 
		(ResultTilesChanged ==  1) andalso (IsActive == true) -> 
			db_server:map_set_grid(Sg, Grid, ResultMap),
			send_all_notify(Notify, {grid_state, S2});
		true ->
			ok
	end,
	
    process_changed_objects(S2#grid_state{time_updated=GridTime}, ResultChanged).

% удалить объекты по списку
process_deleted_objects(State, []) -> State;
process_deleted_objects(State, [{ObjID,Sg}|T]) ->
	db_server:object_remove(Sg, ObjID),
    process_deleted_objects(object_remove(State, ObjID), T).

% изменить объекты по списку
process_changed_objects(State, []) -> State;
process_changed_objects(State, [O|T]) ->
	#object{coord=Coord, id=ObjID} = O,
	% старые координаты нужны для определения грида в бд. если меняется грид - удаляем из старого и добавляем в новый
    process_changed_objects(object_changed(State, ObjID, Coord, O), T).

%%--------------------------------------------------------------------------------------------------
object_spawn(State, []) -> State;
object_spawn(State, List) when is_list(List) ->
    [H|T] = List,
    object_spawn(object_spawn(State, H), T);
% а можно и по одному
object_spawn(State, Obj) -> object_spawn(State, any, Obj).
object_spawn(#grid_state{level=Lv, coord=GC, neighbors=NS} = State, Where, #object{coord={X,Y,_}} = Object) ->
	% мой ли грид?
	OGC = map_server:get_sg_grid(X, Y, Lv),
	case OGC of
		coord_error -> State;
		GC ->
		    ?DEBUG("object_spawn in my grid ~p", [Object]),
		    #object{id=OldObjID, type=ObjType} = Object,

		    % generate obj id only if need
    		ObjID = if OldObjID == none -> world_server:get_next_id(); true -> OldObjID end,
    		O1 = objects_server:object_fill(Object),
    		O = O1#object{id=ObjID},
			% сохраняем в бд если нужно
			case objects_server:is_object_save_db(ObjType) of
				true -> db_server:object_add(O);
				_ -> ok
			end,
    		object_add(State, O);
		_ ->
			% по координатам объект вне нашего грида
			% где спавнить?
			case Where of
				% надо заспавнить именно в этом гриде. не ищем по соседям
				grid -> State;
				% иначе попробуем заспавнить в соседних
				any ->
					case lists:keyfind(OGC, #neighbor_rec.coord, NS) of
						false ->
							?WARNING_MSG("my gc=~p havnt neighbor coord=~p",[GC, OGC]), State;
						#neighbor_rec{pid=NP} ->
							?DEBUG("spawn obj in other grid ~p",[NP]),
							NP ! {object_spawn, grid, Object},
							State
					end
			end
	end.

%%--------------------------------------------------------------------------------------------------
object_add(State, O) -> object_add(State, O, true).
object_add(State, #object{id=ObjID, coord= {X,Y,_}, type=Type} = Object, NeedNotify) ->
    case objects_server:get_time_type(Type) of
        static->
             ?DEBUG("session: obj_add, static ~p", [Object]),
            #grid_state{ static = List, notify = Notify } = State,
            ListNew = add_object_to_list(List, Object),
            if
				(NeedNotify == true) -> send_all_notify(Notify, {obj_pos, Object});
				(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_pos, Object});
				true -> ok
			end,
            State#grid_state{ static = ListNew };
        tick ->
             ?DEBUG("session: obj_add, tick ~p", [Object]),
            #grid_state{ tick = List, notify = Notify } = State,
            #object{id=ObjID, coord={X,Y,_}} = Object,
            ListNew = add_object_to_list(List, Object),
            if
				(NeedNotify == true) -> send_all_notify(Notify, {obj_pos, Object});
				(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_pos, Object});
				true -> ok
			end,
            State#grid_state{ tick = ListNew };
        dyn ->
             ?DEBUG("session: obj_add, dyn ~p", [Object]),
            #grid_state{ dyn = List, notify = Notify, dynamic_pids = DynPids } = State,
            NewDynPids = case spawn_dynamic_object(DynPids, Object) of
                {exist, _} -> DynPids;
                Pid -> [#dynamic_pid{pid=Pid, objid=ObjID}|DynPids]
            end,
            ListNew = add_object_to_list(List, Object),
            if
				(NeedNotify == true) -> send_all_notify(Notify, {obj_pos, Object});
				(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_pos, Object});
				true -> ok
			end,
            State#grid_state{ dyn = ListNew, dynamic_pids = NewDynPids }
    end.

%% ---------------------------------------------------------------------------------------------------------------
% object drop down
object_drop(State, [], _X, _Y) -> State;
object_drop(State, List, X, Y) when is_list(List) ->
	[H|T] = List,
	object_drop(
		object_drop(State, H, X, Y),
		T, X, Y);
object_drop(State, Item, X, Y) ->
	% проверим координаты
	case check_coords(State, X, Y) of
		true ->
		    ?DEBUG("item drop ~p ~p ~p", [Item,X,Y]),
		    #grid_state{ level = Lv } = State,
    		#item{id=ObjID, type=Type, items=Items} = Item,
			SpawnType = inventory:get_type_for_spawn(Type),
    		% if obj have inventory - add it to params
			#object{params=Params} = O = objects_server:object_fill(#object{id=ObjID, type=SpawnType, coord={X,Y,Lv}, state=Item}),
			O1 = O#object{params = lists:keystore(inventory, 1, Params, {inventory, Items})},

			case objects_server:is_object_save_db(SpawnType) of
				true -> db_server:object_add(O1);
				_ -> ok
			end,
    		object_add(State, O1);
		false -> State
	end.

%%--------------------------------------------------------------------------------------------------
object_remove(State, []) -> State;
object_remove(State, List) when is_list(List) ->
    [H|T] = List,
    object_remove(object_remove(State, H, true), T);
object_remove(State,ObjID) -> object_remove(State, ObjID, true).

object_remove(State,ObjID, NeedNotify) ->
    #grid_state{ tick = Ticks,
            dyn = Dyns,
            static = Static,
			notify = Notify,
            dynamic_pids = DynPids } = State,
    ?DEBUG("dyn pids ~p", [DynPids]),

	FunObject = fun (#object{params=Params, coord={OX,OY,_}}, Pid) ->
						 case Pid of
							 none -> ok;
							 _ -> Pid ! {finish}
						 end,
						 case lists:keyfind(follow, 1, Params) of
							false ->
								case lists:keyfind(links, 1, Params) of
									false -> State;
									{links, LinksList} ->
										% все что было ко мне привязано, всем им ставим позицию
										SS = unlink_object(State, follow_list, LinksList),
										object_pos(SS, LinksList, OX, OY, true)
								end;
							{follow, _MainObjID} ->
								unlink_object(State, follow, ObjID)
						 end
				end,

	% такая же реакция должна быть если объект умер (поиск по die)
    case NeedNotify of true -> send_all_notify(Notify, {obj_remove, ObjID}); _ -> ok end,
    % find object
    case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
        case lists:keytake(ObjID, #object.id, Ticks) of
        false ->
            case lists:keytake(ObjID, #object.id, Static) of
            false ->
                ?DEBUG("WARNING! object_remove not found ~p", [ObjID]),
                State;
            {value, O, NewList} ->
                ?DEBUG("object removed from static_objects ~p", [O]),
				S1 = FunObject(O, none),
                S1#grid_state{ static = NewList }
            end;
        {value, O, NewList} ->
            ?DEBUG("object removed from tick_objects ~p", [O]),
			S1 = FunObject(O, none),
            S1#grid_state{ tick = NewList }
        end;
    {value, O, NewList} ->
        ?DEBUG("object removed from dynamic_objects ~p", [O]),
        % need kill object process
        P = case lists:keyfind(ObjID, #dynamic_pid.objid, DynPids) of
            false ->
				#object{type=ObjType} = O,
				if 
					% пид игрока удаляем при деактивации грида
					ObjType =/= player ->
						?WARNING_MSG("dynamic object havnt pid ~p",[O]);
					true -> 
						ok
				end,
				none;
            #dynamic_pid{pid=Pid} -> Pid
        end,
		S1 = FunObject(O, P),
        S1#grid_state{ dyn = NewList }
    end.

%%--------------------------------------------------------------------------------------------------
object_changed(State, ObjID, {OldX,OldY,OldLv}, #object{type=NewType} = NewObject) ->
    #grid_state{ tick = Ticks,
           	dyn = Dyns,
            static = Static,
            notify = Notify} = State,
    % update in db
    case objects_server:is_object_save_db(NewType) of 
		true -> db_server:object_update_pos({OldX,OldY,OldLv}, NewObject); 
		_ -> ok 
	end,
	
	case objects_server:visual_state_have(NewType) of
		true -> send_all_notify(Notify, {obj_visual_state, NewObject});
		false -> ok
	end,
	
    % find object
    case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
        case lists:keytake(ObjID, #object.id, Ticks) of
        false ->
            case lists:keytake(ObjID, #object.id, Static) of
            false ->
                ?DEBUG("WARNING! session object not found ~p", [ObjID]),
                State;
            {value, O, NewList} ->
                ?DEBUG("changed in static_objects old=~p new=~p", [O,NewObject]),
                #object{type=OldType} = O,
                if (OldType =/= NewType) ->
                    send_all_notify(Notify, {obj_changed_type, NewObject}),
					% возможно сменился тайм тип
					case objects_server:get_time_type(NewType) of
						static -> State#grid_state{ static = [NewObject|NewList] };
						% переносим объект в тик
						tick -> State#grid_state{ tick = [NewObject|Ticks], static=NewList };
						dyn -> error(wrong_time_type)
					end;
                true ->
                    send_all_notify(Notify, {obj_changed, NewObject}),
	                State#grid_state{ static = [NewObject|NewList] }
                end
            end;
        {value, O, NewList} ->
            ?DEBUG("changed in tick_objects ~p", [O]),
            #object{type=OldType} = O,
            if (OldType =/= NewType) ->
                    send_all_notify(Notify, {obj_changed_type, NewObject}),
					% возможно сменился тайм тип
					case objects_server:get_time_type(NewType) of
						tick -> State#grid_state{ tick = [NewObject|NewList] };
						% переносим объект в статик
						static -> State#grid_state{ static = [NewObject|Static], tick=NewList };
						dyn -> error(wrong_time_type)
					end;
            true ->
                    send_all_notify(Notify, {obj_changed, NewObject}),
					State#grid_state{ tick = [NewObject|NewList] }
            end
        end;
    {value, O, NewList} ->
        ?DEBUG("changed in dynamic_objects ~p", [O]),
        #object{type=OldType} = O,
        if (OldType =/= NewType) ->
                    send_all_notify(Notify, {obj_changed_type, NewObject});
        true ->
                    send_all_notify(Notify, {obj_changed, NewObject})
        end,
        State#grid_state{ dyn = [NewObject|NewList] }
    end.

%%--------------------------------------------------------------------------------------------------
% поставить объекту позицию
object_pos(State, [], _X,_Y) -> State;
object_pos(State, List, X,Y) when is_list(List) ->
	[ObjID|T] = List,
	object_pos(
	  object_pos(State, ObjID, X,Y),
	  T, X,Y);
object_pos(State, ObjID, X,Y) -> object_pos(State, ObjID, X,Y, true).

object_pos(State, [], _X,_Y, _NeedNotify) -> State;
object_pos(State, List, X,Y, NeedNotify) when is_list(List) ->
	[ObjID|T] = List,
	object_pos(
	  object_pos(State, ObjID, X,Y, NeedNotify),
	  T, X,Y, NeedNotify);
object_pos(State, ObjID, X,Y, NeedNotify) ->
    % update pos
	?DEBUG("object_pos ~p ~p, ~p notify ~p",[ObjID, X,Y, NeedNotify]),
    #grid_state{ tick = Ticks, notify=Notify, dyn = Dyns, static = Static, neighbors=NS, dynamic_pids=DP } = State,
    case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
    case lists:keytake(ObjID, #object.id, Ticks) of
    false ->
    case lists:keytake(ObjID, #object.id, Static) of
    false ->
                ?DEBUG("WARNING! session object not found ~p", [ObjID]),
                State;
    {value, O, NewList} ->
                #object{coord={OldX,OldY,Lv}, type=Type, params=OldParams} = O,
                ONew = O#object{ coord={X,Y,Lv}, params=lists:keydelete(line_move, 1, OldParams) },
				if
					(NeedNotify == true) -> send_all_notify(Notify, {obj_pos, ONew});
					(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_pos, ONew});
					true -> ok
				end,
                case objects_server:is_object_save_db(Type) of true -> db_server:object_update_pos({OldX,OldY,Lv},ONew); _ -> ok end,
                SS = State#grid_state{ static = [ONew|NewList] },
				case lists:keyfind(links, 1, OldParams) of
					false -> SS;
					{links, LinksList} ->
						object_pos(SS, LinksList, X,Y, NeedNotify)
				end
    end;
    {value, O, NewList} ->
                #object{coord={OldX,OldY,Lv}, type=Type, params=OldParams} = O,
                ONew = O#object{ coord={X,Y,Lv}, params=lists:keydelete(line_move, 1, OldParams) },
				if
					(NeedNotify == true) -> send_all_notify(Notify, {obj_pos, ONew});
					(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_pos, ONew});
					true -> ok
				end,
                case objects_server:is_object_save_db(Type) of true -> db_server:object_update_pos({OldX,OldY,Lv},ONew); _ -> ok end,
                SS = State#grid_state{ tick = [ONew|NewList] },
				case lists:keyfind(links, 1, OldParams) of
					false -> SS;
					{links, LinksList} ->
						object_pos(SS, LinksList, X,Y, NeedNotify)
				end
    end;
    {value, O, NewList} ->
                #object{coord={OldX,OldY,Lv}, type=Type, params=OldParams} = O,
                ONew = O#object{ coord={X,Y,Lv}, params=lists:keydelete(line_move, 1, OldParams) },
				if
					(NeedNotify == true) -> send_all_notify(Notify, {obj_pos, ONew}), 
											send_all_dyns(NS, DP, {dyn_object_pos, ONew});
					(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_pos, ONew}), 
											 send_all_dyns(NS, DP, {dyn_object_pos, ONew});
					true -> ok
				end,
				
                case objects_server:is_object_save_db(Type) of true -> db_server:object_update_pos({OldX,OldY,Lv},ONew); _ -> ok end,
				
                SS = State#grid_state{ dyn = [ONew|NewList] },
				case lists:keyfind(links, 1, OldParams) of
					false -> SS;
					{links, LinksList} ->
						object_pos(SS, LinksList, X,Y, NeedNotify)
				end
    end.

%% --------------------------------------------------------------------
% set param to object, return new state
object_set_param_notify(State, Object, Param) when is_record(Object, object)->
	#object{id=ObjID} = Object,
	%?DEBUG("object_set_param_notify ~p ~p",[ObjID, Param]),
    #grid_state{ notify = Notify } = State,
    send_all_notify(Notify, {obj_set_param, Object, Param}),
    object_set_param(State, ObjID, Param);

object_set_param_notify(State, ObjID, Param) ->
    object_set_param(State, ObjID, Param, true).

%% --------------------------------------------------------------------
%
object_set_param(State, [], _Param) -> State;
object_set_param(State, List, Param) when is_list(List) ->
	[ObjID|T] = List,
	object_set_param(
	  		object_set_param(State, ObjID, Param),
					 T, Param);
object_set_param(State, ObjID, Param) ->
	object_set_param(State, ObjID, Param, false).

object_set_param(State, [], _Param, _NeedNotify) -> State;
object_set_param(State, List, Param, NeedNotify) when is_list(List) ->
	[ObjID|T] = List,
	object_set_param(
	  		object_set_param(State, ObjID, Param, NeedNotify),
					 T, Param, NeedNotify);
object_set_param(State, ObjID, Param, NeedNotify) ->
    #grid_state{ tick = Ticks, dyn = Dyns, static = Static, notify = Notify, neighbors=NS, dynamic_pids=DP } = State,
    {ParamName, ParamData} = Param,
	?DEBUG("object_set_param ~p ~p",[ObjID, Param]),

	FunObject = fun(#object{type=Type, coord=Coord, params=OldParams } = O, TimeType, TailList) ->
				NewCoord =
                        if ParamName == line_move ->
                            {_,X,Y,_,_} = ParamData,
                            {_,_,Lv} = Coord,
                            %?DEBUG("session: its line_move param. update obj coord ~p ~p", [X,Y]),
                            {X,Y,Lv};
                        true -> Coord
                        end,

                ONew = O#object{coord=NewCoord, params=set_param(OldParams, Param) },
				if
					(NeedNotify == true) -> send_all_notify(Notify, {obj_set_param, ONew, Param}),
											send_all_dyns(NS, DP, {dyn_object_pos, ONew});
					(is_list(NeedNotify)) -> send_all_notify(NeedNotify, {obj_set_param, ONew, Param}),
											 send_all_dyns(NS, DP, {dyn_object_pos, ONew});
					true -> ok
				end,

                case objects_server:is_object_save_db(Type) of 
					true -> if (NewCoord =/= Coord) -> db_server:object_update_pos(Coord,ONew); true -> ok end; 
					_ -> ok 
				end,
				SS = case TimeType of
						 static -> State#grid_state{ static = [ONew|TailList] };
						 tick -> State#grid_state{ tick = [ONew|TailList] };
						 dyn -> State#grid_state{ dyn = [ONew|TailList] }
				end,

				case lists:keyfind(links, 1, OldParams) of
					false -> SS;
					{links, LinksList} ->
						{NX, NY, _} = NewCoord,
						object_pos(SS, LinksList, NX,NY, NeedNotify)
				end
		  end,


    case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
        case lists:keytake(ObjID, #object.id, Ticks) of
        false ->
            case lists:keytake(ObjID, #object.id, Static) of
            false ->
				%?WARNING_MSG("WARNING! object not found ~p", [ObjID]),
                State;
            {value, O, NewList} -> FunObject(O, static, NewList)
	        end;

    	   	{value, O, NewList} -> FunObject(O, tick, NewList)
      	end;

    		{value, O, NewList} -> FunObject(O, dyn, NewList)
    end.

%% --------------------------------------------------------------------
% remove param from object, return new state
object_remove_param_notify(State, ObjID, ParamName) ->
    #grid_state{ notify = Notify } = State,
    send_all_notify(Notify, {obj_remove_param, ObjID, ParamName}),
    object_remove_param(State, ObjID, ParamName).

object_remove_param(State, ObjID, ParamName) ->
    #grid_state{ tick = Ticks, dyn = Dyns, static = Static } = State,
	?DEBUG("object_remove_param ~p param_name=~p",[ObjID, ParamName]),

    case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
        case lists:keytake(ObjID, #object.id, Ticks) of
        false ->
            case lists:keytake(ObjID, #object.id, Static) of
            false ->
                ?DEBUG("WARNING! object not found ~p", [ObjID]),
                State;
            {value, O, NewList} ->
                #object{params=OldParams} = O,
                ONew = O#object{params=lists:keydelete(ParamName, 1, OldParams)},
                ?DEBUG("object_remove_param static ~p", [ONew]),
                State#grid_state{ static = [ONew|NewList] }
            end;
        {value, O, NewList} ->
                #object{params=OldParams} = O,
                ONew = O#object{params=lists:keydelete(ParamName, 1, OldParams)},
                ?DEBUG("object_remove_param tick ~p", [ONew]),
                State#grid_state{tick  = [ONew|NewList] }
        end;
    {value, O, NewList} ->
                #object{params=OldParams} = O,
                ONew = O#object{params=lists:keydelete(ParamName, 1, OldParams)},
                ?DEBUG("object_remove_param dynamic ~p", [ONew]),
                State#grid_state{dyn  = [ONew|NewList] }
    end.


%%--------------------------------------------------------------------------------------------------
% обновить тик объекты с указанным временем
update_tick_objects(#grid_state{tick=Tick, notify=N} = State, DT, Time) ->
	?DEBUG("update_tick_objects dt=~p",[DT]),
	NewTick = update_tick_objects(DT, Time, Tick, N, []),
	State#grid_state{tick=NewTick}.

update_tick_objects(_DT, _Time, [], _, Acc) -> Acc;
update_tick_objects(DT, Time, [Object|T], Notify, Acc) ->
	?DEBUG("upd tick obj: ~p",[Object]),
	{Result, #object{id=ObjID,coord=OldC, type=NewType} = NewObj} = objects_server:update_tick_object(DT, Time, Object),
	%?DEBUG("result=~p",[Result]),
	case Result of
		true ->

		    % update in db
		    case objects_server:is_object_save_db(NewType) of 
				true -> db_server:object_update_pos(OldC, NewObj); 
				_ -> ok 
			end,
			
			case objects_server:visual_state_have(NewType) of
				true -> send_all_notify(Notify, {obj_visual_state, NewObj});
				false -> ok
			end,
			
			if (Object#object.type =/= NewType) ->
            	send_all_notify(Notify, {obj_changed_type, NewObj});
			true ->	
				send_all_notify(Notify, {obj_changed, NewObj})
			end,
			
			update_tick_objects(DT, Time, T, Notify, [NewObj|Acc]);

		false ->
			update_tick_objects(DT, Time, T, Notify, [NewObj|Acc]);
		die ->
			% объект умер. надо удалить отовсюду
			?DEBUG("object die! remove it ~p",[NewObj]),
			case map_server:get_sg(OldC) of
				coord_error ->
					?ERROR_MSG("get_sg = error coord=~p",[OldC]),
					update_tick_objects(DT, Time, T, Notify, [NewObj|Acc]);
				ObjSg ->
					db_server:object_remove(ObjSg, ObjID),
		    		send_all_notify(Notify, {obj_remove, ObjID}),

					update_tick_objects(DT, Time, T, Notify, Acc)
			end
	end.

%%--------------------------------------------------------------------------------------------------
player_add(
	#grid_state{coord=GC, neighbors=NS} = State,
	#object{id=CharID, coord={X,Y,Lv}, type=Type} = Object,
	AccountID, From, Collision) ->

	?DEBUG("player add ~p ~p ~p",[X,Y,Lv]),

	% нужно ли проверять коллизию?
	case Collision of
	true ->
	case check_move_collision(State, X, Y, X, Y, none, Type, CharID, From, ?MOVE_LAND) of
		lock_fail ->
			% неудача при блокировке. надо будет повторить попытку через какое то время
			From ! {fail_retry},
			State;

		% результат модуля коллизий и список залоченных гридов
		{Result, LockList, LockArea, _} ->
			case Result of
				{no_collision} ->
					%% конечная точка в моем гриде?
					case map_server:get_sg_grid(X, Y, Lv) of
						GC ->
							if length(LockList) > 0 -> ?DEBUG("unlock!!!------------------------------------"); true -> ok end,
							%% сразу шлем "разблокировку" всем затронутым гридам
							lists:foreach(fun (CGC) ->
									case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
										false ->
											?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
											error(not_my_neighbor_grid);
										#neighbor_rec{pid=P} ->
											P ! {unlock, self(), LockArea}
									end
									end, LockList),
							% уведомим
							From ! {ack_player_add_ok},
							%% ставим объекту позицию
							object_add(State#grid_state{dynamic_pids=[
														 #dynamic_pid{account_id=AccountID, objid=CharID, pid=From} |
														 State#grid_state.dynamic_pids]}, Object);
						_ ->
							%% 	конечная точка в другом гриде
							% явная ошибка. т.к. это не запрос на движение. и запрос должен идти только в мой грид
							error(add_player_point_not_my)
					end;
				_ ->
					if length(LockList) > 0 -> ?DEBUG("unlock!!!-----------------------------------"); true -> ok end,
					%% сразу шлем "разблокировку" всем затронутым гридам
					lists:foreach(fun (CGC) ->
							case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
								false ->
									?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
									error(not_my_neighbor_grid);
								#neighbor_rec{pid=P} ->
									P ! {unlock, self(), LockArea}
							end
							end, LockList),
					% есть коллизия с чем то. добавить нельзя
					From ! {ack_player_add_fail},
					State
			end

	end;
	false ->
		From ! {ack_player_add_ok},
		object_add(State#grid_state{dynamic_pids=[
														 #dynamic_pid{account_id=AccountID, objid=CharID, pid=From} |
														 State#grid_state.dynamic_pids
												 ]}, 
								   Object)
	end.

%% --------------------------------------------------------------------------------------------------------------------------
player_spawn_object(State, From, Type, X, Y, Q, HP, ObjState) ->
    #grid_state{ level = Lv, coord = GC } = State,
	% проверить что координаты моего грида
	case check_coords(State, X, Y) of
		true ->
    case cutils:get_object_collision(Type, player) of
        true -> % только если объект дает коллизию - проверим ее
		?DEBUG("player_spawn_object: check collision type=~p x=~p y=~p", [Type, X,Y]),
		case check_collision(State, Type, X, Y, ?MOVE_LAND) of
			{fail_retry} ->
				if From=/=none -> From ! {ack, {spawned_fail}}; true -> ok end, State;
        %case check_move_collision(State, none, Type, X, Y, X, Y) of
		%case collision:process(Static, Tick, Dyn, Grids, 0, Type, X, Y, X, Y, Lv, 0) of
            {no_collision} ->
				case objects_server:can_spawn_on_tile(Type, get_tile(State, X, Y)) of
					true ->
                		NewObjID = world_server:get_next_id(),
		                if From=/=none -> From ! {ack, {spawned_id, NewObjID}}; true -> ok end,
        		        Obj = #object{ id=NewObjID, type=Type, coord={X,Y,Lv}, q=Q, hp={HP, HP}, params=[], state=ObjState  },
                		object_spawn(State, Obj);
					false ->
						?DEBUG("cant spawn on this tile"),
						if From=/=none -> From ! {ack, {spawned_fail}}; true -> ok end, State
				end;
            _ ->
				?DEBUG("player_spawn_object: obj collision type=~p",[Type]),
                if From=/=none -> From ! {ack, {spawned_fail}}; true -> ok end, State
        end;
        false -> % если объект не дает коллизий - сразу спавним
			case objects_server:can_spawn_on_tile(Type, get_tile(State, X, Y)) of
				true ->
                	NewObjID = world_server:get_next_id(),
	                if From=/=none -> From ! {ack, {spawned_id, NewObjID}}; true -> ok end,
    	            Obj = #object{ id=NewObjID, type=Type, coord={X,Y,Lv}, q=Q, hp={HP, HP}, params=[], state=ObjState  },
        	        object_spawn(State, Obj);
				false ->
					?DEBUG("cant spawn on this tile"),
					From ! {ack, {spawned_fail}}, State
			end
	end;
		false ->
			% координаты для спавна не в нашем гриде
	      	if From=/=none -> From ! {none, GC}; true -> ok end, State
    end.

%%--------------------------------------------------------------------------------------------------
% return: {fail_retry} | {no_collision}
check_collision(State, 
	Type, X, Y, MoveType) ->
	check_collision(State, Type, X, Y, MoveType, 0).
check_collision(#grid_state{level=Lv, coord=GC, neighbors=NS} = State, 
	Type, X, Y, MoveType, ObjID) ->
	
	% узнаем гриды в которых будет вестись обсчет коллизии
	Grids = cutils:get_object_grids(Type, X, Y, X, Y, Lv),
	case lists:member(GC, Grids) of
		true ->
			% узнаем есть ли коллизия если поставить объект заданного типа в указанном направлении
			move_request(State, none, ObjID, Type, none, X, Y, X, Y, none, MoveType);
		false ->
			% координаты для обсчета находятся в другом гриде. сделаем запрос в него
			% берем первые координаты из списка
			[HGC|_] = Grids,
			case lists:keyfind(HGC, #neighbor_rec.coord, NS) of
				false ->
					% нужный грид не найден в соседях текущего грида. (либо косяк в соседях, либо косяк в расчетах)
					error(check_collision_neighbor_not_found);
				#neighbor_rec{pid=P} ->
					?INFO_MSG("check_collision in other grid self=~p, pid=~p",[self(), P]),
					P ! {check_collision, self(), Type, X, Y, MoveType},
					receive 
						{check_collision_ack, Ack} -> Ack;
						{check_collision, From, _, _, _, _} -> 
							From ! {check_collision_ack, {fail_retry}},
							{fail_retry}
					after 1000 ->
						error(timeout_check_collision_ack)
					end
			end
	end.
	
	
	

%%--------------------------------------------------------------------------------------------------
% VirtualType - {type, x, y},  type может быть none
move_request(
  #grid_state{level=Lv, coord=GC, neighbors=NS, notify=MyNotify} = State,
  From, ObjIDTuple, Type, VirtualType, OldX, OldY, X, Y, SetParam, MoveType
			) ->
	?DEBUG("move_request from=~p ~p to=~p ~p gc=~p param=~p vtype=~p",[OldX, OldY, X, Y, GC, SetParam, VirtualType]),
	ObjID = case ObjIDTuple of
				{ID, _MoveType, _MoveParams} -> ID; % used only by player process
				ID -> ID
			end,
	case check_move_collision(State, OldX, OldY, X, Y, VirtualType, Type, ObjIDTuple, From, MoveType) of
		lock_fail ->
			?WARNING_MSG("lock_fail"),
			% неудача при блокировке. надо будет повторить попытку через какое то время
			case From of
				none -> {fail_retry};
				_ ->
					From ! {fail_retry},
					State
			end;

		% результат модуля коллизий и список залоченных гридов
		{Result, LockList, LockArea, LockGrids} ->
			?DEBUG("result ~p",[Result]),
			case From of
				none ->
					% отправитель не указан - надо просто вернуть результат
					%% сразу шлем "разблокировку" всем затронутым гридам
					lists:foreach(fun (CGC) ->
									case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
										false ->
											?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
											error(not_my_neighbor_grid);
										#neighbor_rec{pid=P} ->
											P ! {unlock, self(), LockArea}
									end
									end, LockList),
					Result;
				_ ->
			case Result of
				{no_collision} ->
					%% конечная точка в моем гриде?
					case map_server:get_sg_grid(X, Y, Lv) of
						GC ->
							%?DEBUG("its my grid"),
							if length(LockList) > 0 -> ?DEBUG("unlock!!! ~p--------------------------------------------------",[length(LockList)]); true -> ok end,
							%% сразу шлем "разблокировку" всем затронутым гридам
							lists:foreach(fun (CGC) ->
									case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
										false ->
											?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
											error(not_my_neighbor_grid);
										#neighbor_rec{pid=P} ->
											P ! {unlock, self(), LockArea}
									end
									end, LockList),

							% уведомим
							From ! Result,
							%% ставим объекту позицию
		                    case SetParam of
        		                none -> State;
                		        pos ->
                        		    object_pos(State, ObjID, X, Y);
		                        Param ->
        		                    object_set_param_notify(State, ObjID, Param)
                		    end;

						%% 	конечная точка в другом гриде
						ThatGC ->
							if length(LockList) > 0 -> ?DEBUG("unlock!!! ~p--------------------------------------------------",[length(LockList)]); true -> ok end,

							% возьмем объект у меня
							{Taked, StateFinal} = take_object(State, ObjID),
							% корректно разблокируем все гриды
							unlock_grids(NS, MyNotify, LockGrids, LockArea, ThatGC, Taked, SetParam, ObjID, X, Y),
							% уведомим запрашивающего
							From ! Result,
							% вернем стейт
							StateFinal
					end;
				_ ->
					% есть коллизия
					% получаем координаты с учетом коллизии
					{CX, CY} = case Result of
								   {collision_tile, XX, YY, _} -> {XX, YY};
								   {collision_virtual, XX, YY} -> {XX, YY};
								   {collision_obj, XX, YY, _, _} -> {XX, YY}
							   end,
					%% конечная точка в моем гриде?
					case map_server:get_sg_grid(CX, CY, Lv) of
						GC ->
							% конечная точка в моем гриде
							?DEBUG("its my grid"),
							if length(LockList) > 0 -> ?DEBUG("unlock!!! ~p--------------------------------------------------",[length(LockList)]); true -> ok end,
							%% сразу шлем "разблокировку" всем затронутым гридам
							lists:foreach(fun (CGC) ->
									case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
										false ->
											?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
											error(not_my_neighbor_grid);
										#neighbor_rec{pid=P} ->
											P ! {unlock, self(), LockArea}
									end
									end, LockList),

							% уведомим
							From ! Result,
							%% ставим объекту позицию
		                    object_pos(State, ObjID, CX, CY);

						ThatGC ->
							if length(LockList) > 0 -> ?DEBUG("unlock!!! ~p--------------------------------------------------",[length(LockList)]); true -> ok end,
							%% 	конечная точка в другом гриде
							% возьмем объект у меня
							{Taked, StateFinal} = take_object(State, ObjID),
							% коректно разблокируем все гриды
							unlock_grids(NS, MyNotify, LockGrids, LockArea, ThatGC, Taked, none, ObjID, CX, CY),
							% уведомим запрашивающего
							From ! Result,
							% вернем стейт
							StateFinal
					end
			end
			end

	end.



%%--------------------------------------------------------------------------------------------------
% запрос на обсчет коллизии
% Virtual = объект который стоит в конечной точке движения, если none - значит его нет
% вернет {Result, List, Data} | lock_fail
% где результат - это выдача модуля коллизий
% лист - список заблокированных гридов {Sg, Grid}
% Data - список стейтов затронутых гридов #grid_state
check_move_collision(
  #grid_state{level=Lv, coord=GC, locked=LA}=State,
  X, Y, ToX, ToY, VirtualType, Type, ObjID, From, MoveType) ->
	% если начальная точка не в моем гриде - краш
	Grids = cutils:get_object_grids(Type, X, Y, ToX, ToY, Lv),
	case lists:member(GC, Grids) of
		false ->
			?CRITICAL_MSG("start_point_not_my grids=~p gc=~p", [Grids, GC]),
			error(start_point_not_my);
		true ->
			% область для блокировки
			LockArea = #grid_lock_area{area=coord:make_rect(X, Y, ToX, ToY), owner=From},
			% смотрим на вектор запрашиваем затронутые гриды с учетом размеров объекта который движется
			case Grids of
				[GC] ->
					% если попали в заблокированную область
					case is_locked(LockArea, LA) of
						true ->
							% скажем что тут коллизия
							{collision_obj, X, Y, none, 0};
						false ->
							% это только мой грид - обсчитываем коллизию сразу и выдаем результат
							% вернем результат модуля коллизий с пустым списком. заблокированных гридов нет. это чисто мой грид
							{collision:process([State], ObjID, Type, VirtualType, X, Y, ToX, ToY, Lv, MoveType), [], none, []}
					end;
				
				_ -> case lists:member(GC, Grids) of
						 false -> error({end_point_not_my, GC, Grids});
						 true ->
							 	% блокируем гриды (кроме себя) и ждем от них сообщений
								LockGrids = lists:delete(GC, Grids),
								request_collision_grids(State, LockGrids, LockArea),
								case collision_wait_grids(State, LockGrids, []) of
									lock_fail ->
										% ошибка блокировки. вернем запрашивающему фейл и выйдем в нормальное состояние
										lock_fail;
									Data ->
										% стейты соседей получены. обсчитываем коллизии
										% вернем результат модуля коллизий вместе со список заблокированных гридов
										{collision:process([State|Data], ObjID, Type, VirtualType, X, Y, ToX, ToY, Lv, MoveType), LockGrids, LockArea, Data}


								end
					 end
			end
	end.


%%--------------------------------------------------------------------------------------------------
collision_wait_grids(_State, [], Acc) ->
	% все гриды получены - начинаем обсчет
	Acc;
collision_wait_grids(
  State, Grids, Acc) ->
	receive
		{lock_data, From, LockArea} ->
			#grid_state{locked=LA} = State,
			% заблокирована ли эта область?
			case is_locked(LockArea, LA) of
				true -> 
					From ! {lock_fail}, 
					lock_fail;
				false -> 
					% шлем стейт без соседей
					From ! {grid_state, State},
					% добавляем область в залоченные 
					collision_wait_grids(State#grid_state{locked=[LockArea|LA]}, Grids, Acc)
			end;
		
		{lock_fail} ->
			% другой грид сообщает об ошибке блокировки. надо попытатся в следующий раз
			lock_fail;
		
		{get_state, From} ->
			From ! {grid_state, State}, 
			collision_wait_grids(State, Grids, Acc);

		{grid_state, #grid_state{coord=GC} = RemoteState} ->
			% запоминаем стейт соседа
			S1 = case lists:keytake(GC, #neighbor_rec.coord, State#grid_state.neighbors) of
				false ->
					% пришел стейт не от моего соседа - крит
					?CRITICAL_MSG("collision_wait_grids : grid state not from my neighbor! ~p",[GC]),
					State;
				{value, N, Tail} ->
					% обновим состояние соседа
					State#grid_state{neighbors=[
										N#neighbor_rec{is_loaded=true, state_recv_time=time_server:get_time(), state=RemoteState#grid_state{neighbors=[]}}
																 |Tail]}
			end,

			case lists:member(GC, Grids) of
				false ->
					collision_wait_grids(S1, Grids, Acc);
				true ->
					collision_wait_grids(S1, lists:delete(GC, Grids), [RemoteState|Acc])
			end
	after 800 -> 
			% слишком долго ждем стейта. что-то не так, крашимся
			?CRITICAL_MSG("timeout_wait_collision_grids"),
			lock_fail  
	end.


%%--------------------------------------------------------------------------------------------------
get_objects_in_rect(State, From, VisibleZone) ->
    #grid_state{ tick = Ticks, dyn = Dyns, static = Static} = State,
    send_objects_in_rect(From, Ticks, VisibleZone),
    send_objects_in_rect(From, Dyns, VisibleZone),
%%     ?DEBUG("session: get static in rect ~p ~p", [Static,VisibleZone]),
    send_objects_in_rect(From, Static, VisibleZone).

%%--------------------------------------------------------------------------------------------------
get_objects_in_list(_State, _From, []) -> ok;
get_objects_in_list(State, From, [ObjID|T]) ->
	case find_object(State, ObjID) of
		none -> get_objects_in_list(State, From, T);
		Object ->
			send_obj_pos(From, Object),
			get_objects_in_list(State, From, T)
	end.

%%--------------------------------------------------------------------------------------------------
% find object in session state return Object | none
find_object(#grid_state{ tick = Ticks, dyn = Dyns, static = Static }, ObjID) ->
            case lists:keyfind(ObjID, #object.id, Dyns) of
            false ->
                case lists:keyfind(ObjID, #object.id, Ticks) of
                false ->
                    case lists:keyfind(ObjID, #object.id, Static) of
                    false ->
                            %?DEBUG("WARNING find_object: OBJECT NOT FOUND ~p", [ObjID]),
                            none;
                    Object -> Object
                    end;
                Object -> Object
                end;
            Object -> Object
            end.

%%--------------------------------------------------------------------------------------------------
% искать объект у себя и у соседей через стейты по ид
find_object_neighbors([], _) -> none;
find_object_neighbors(List, ObjID) when is_list(List) ->
	[H|T] = List,
	case find_object(H, ObjID) of
		none -> find_object_neighbors(T, ObjID);
		O -> O
	end;
find_object_neighbors(#grid_state{neighbors=NS} = State, ObjID) ->
	case find_object(State, ObjID) of
		none ->
			% не нашли у себя - ищем в соседях
			find_object_neighbors(
			  	lists:map(fun(#neighbor_rec{state=S}) -> S end, NS),
				ObjID);
		O -> O
	end.

%%--------------------------------------------------------------------------------------------------
% find object by objid, send packet to client
request_obj_info(
  #grid_state{ tick = Ticks, dyn = Dyns, static = Static },
   From, ObjID) ->
            % find object
            case lists:keyfind(ObjID, #object.id, Dyns) of
            false ->
                case lists:keyfind(ObjID, #object.id, Ticks) of
                false ->
                    case lists:keyfind(ObjID, #object.id, Static) of
                    false ->
                            %?DEBUG("WARNING request_obj_info: OBJECT NOT FOUND ~p", [ObjID]),
							ok;
                    Object -> send_obj_info(From, Object)
                    end;
                Object -> send_obj_info(From, Object)
                end;
            Object -> send_obj_info(From, Object)
            end.

%%--------------------------------------------------------------------------------------------------
% взять объект из стейта.
% вернет {{[объект], [DynPid]}, state}
take_object(State, []) -> State;
take_object(State, List) when is_list(List) -> take_object(State, List, [], []);
take_object(State, ObjID) -> take_object(State, [ObjID], [], []).

take_object(State, [], AccO, AccD) -> {{AccO, AccD}, State};
take_object(State, L, AccO, AccD) when is_list(L) ->
	[H|T] = L,
	{{O, D}, S1} = take_object(State, H, [], []),
	take_object(S1, T,
				lists:umerge(lists:usort(O), lists:usort(AccO)),
				lists:umerge(lists:usort(D), lists:usort(AccD))
			   );

% взять объект по его ид
% вернет взятый объект с учетом его связей и добавит в аккумулятор
take_object(#grid_state{ tick = Ticks, dyn = Dyns, static = Static, dynamic_pids = DynPids } = State, ObjID, AccO, AccD) ->
	FunObject = fun (#object{params=Params} = Object, Tail, TimeType) ->
			{AccD1, S1} = case lists:keytake(ObjID, #dynamic_pid.objid, DynPids) of
				false ->
					{AccD,
					 	case TimeType of
							static -> State#grid_state{static=Tail};
							tick -> State#grid_state{tick=Tail};
							dyn -> State#grid_state{dyn=Tail}
						end
					};
				{value, D, DTail} ->
					{[D|AccD],
					 	case TimeType of
							static -> State#grid_state{static=Tail, dynamic_pids=DTail};
							tick -> State#grid_state{tick=Tail, dynamic_pids=DTail};
							dyn -> State#grid_state{dyn=Tail, dynamic_pids=DTail}
						end
					}
			end,

			% смотрим есть ли связи у объекта
			case lists:keyfind(links, 1, Params) of
								 false ->
									 { {[Object|AccO], AccD1}, S1};
								 {links, LinksList} ->
									 take_object(S1, LinksList, [Object|AccO], AccD1)
			end

	end,


	case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
    case lists:keytake(ObjID, #object.id, Ticks) of
    false ->
    case lists:keytake(ObjID, #object.id, Static) of
    false ->
		error({take_object_not_found, ObjID});
    	{value, Object, Tail} -> FunObject(Object, Tail, static)
    end;
    	{value, Object, Tail} -> FunObject(Object, Tail, tick)
    end;
        {value, Object, Tail} -> FunObject(Object, Tail, dyn)
    end.

%%--------------------------------------------------------------------------------------------------
% положить объект в стейт. вернет стейт
put_object(State, {LO, LD}) ->
	put_object_obj(
	  put_object_dyn(State, LD),
	  LO).

put_object_dyn(State, []) -> State;
put_object_dyn(State, List) when is_list(List) ->
	[H|T] = List,
	put_object_dyn(
	  put_object_dyn(State, H),
	  T);
put_object_dyn(#grid_state{dynamic_pids=DynPids} = State, #dynamic_pid{objid=ObjID} = D) ->
	case find_object(State, ObjID) of
		none ->
			% такого объекта еще нет в гриде
			State#grid_state{dynamic_pids=[D|DynPids]};
		_ ->
			% такой объект уже есть в гриде
			error(put_objdyn_exist)
	end.


put_object_obj(State, []) -> State;
put_object_obj(State, List) when is_list(List) ->
	[H|T] = List,
	put_object_obj(
	  put_object_obj(State, H),
	  T);
put_object_obj(State, #object{id=ObjID, type=Type} = Object) ->
	case find_object(State, ObjID) of
		none ->
	% такого объекта еще нет в гриде
    case objects_server:get_time_type(Type) of
        static->
            #grid_state{ static = List } = State,
            State#grid_state{ static = add_object_to_list(List, Object) };
        tick ->
            #grid_state{ tick = List } = State,
            State#grid_state{ tick = add_object_to_list(List, Object) };
        dyn ->
            #grid_state{ dyn = List } = State,
            State#grid_state{ dyn = add_object_to_list(List, Object) }
	end;
		_ ->
			% такой объект уже есть в гриде
			error(put_obj_exist)
	end.

%%--------------------------------------------------------------------------------------------------
% апдейтим таймер на обработку процессором
update_process_timer(#grid_state{time_updated=Last, 
								 neighbors=NS,  
								 process_tries_count=TriesCount
								} = S, Time) ->
  	%?DEBUG("update_process_timer last=~p req=~p dt=~p", [Last, TimeRequested, Time-Last]),
	if
		% наступило время для апдейта грида.
	  	(Time-Last >= ?GRID_MIN_UPDATE_TIME)->
			?DEBUG("need process grid by timer!"),
			if 
				% проверим сколько попыток уже было
				TriesCount >= 3 -> error(process_grid_max_tries);
				true ->
					% запросим стейты у соседей
					GridsList = lists:map(fun (#neighbor_rec{pid=P, coord=PGC}) ->
										   P ! {get_state, self()},
										   PGC
								  end, NS),
					case wait_grids_for_process(S, GridsList) of
						wait_timeout ->
							% не дождались. возможно ошибка, увеличим счетчик попыток
							?WARNING_MSG("timeout try process ~p ns=~p",[TriesCount, length(NS)]),
							S#grid_state{process_tries_count=TriesCount+1, time_updated=Time};
						S1 -> 
							%?DEBUG("update_process_timer: all states get"),
							S2 = process_grid(S1),
							S2#grid_state{process_tries_count=0, time_updated=Time}
					end
						
			end;
		% время для апдейта еще не наступило
		true ->
			S
	end.

wait_grids_for_process(State, []) -> State;
wait_grids_for_process(State, Grids) ->
	receive
		{get_state, From} ->
			From ! {grid_state, State},
			wait_grids_for_process(State, Grids);

		{grid_state, #grid_state{coord=GC} = RemoteState} ->
			% запоминаем стейт соседа
			S1 = case lists:keytake(GC, #neighbor_rec.coord, State#grid_state.neighbors) of
				false ->
					% пришел стейт не от моего соседа - крит
					?CRITICAL_MSG("collision_wait_grids : grid state not from my neighbor! ~p",[GC]),
					State;
				{value, N, Tail} ->
					% обновим состояние соседа
					State#grid_state{neighbors=[
								N#neighbor_rec{is_loaded=true, state_recv_time=time_server:get_time(), state=RemoteState#grid_state{neighbors=[]}}
																 |Tail]}
			end,

			case lists:member(GC, Grids) of
				false ->
					wait_grids_for_process(S1, Grids);
				true ->
					wait_grids_for_process(S1, lists:delete(GC, Grids))
			end
	after 300 ->
			wait_timeout
	end.

%%--------------------------------------------------------------------------------------------------
update_tick_objects_timer(#grid_state{time_tick_updated=Last} = S, DT, Time)
  % если прошло время для игрового тика - обновляем
  when (Time-Last > ?GAME_TICK) ->
	% запомним время апдейта
	update_tick_objects(S#grid_state{time_tick_updated=Time}, DT, Time);
% время для тика еще прошло
update_tick_objects_timer(S,_,_) ->
	S.

%%--------------------------------------------------------------------------------------------------
% TODO : update_unload_timer
update_unload_timer(_S) -> ok.


%%--------------------------------------------------------------------------------------------------
object_say(State, Say) ->
	case Say of
		{area, ObjID, Msg} ->
			send_all_notify(get_neighbor_notify(State), {area_say, ObjID, Msg}),
			State;
		% TODO : say private, village
		{private, _CharID, _Nick, _Msg} -> State;
		{village, _CharID, _Msg} -> State
	end.

%%--------------------------------------------------------------------------------------------------
% handle action click on object
% Action: right_click, lift
obj_action(State, Action, From, FromObjID, FromState, ObjID) ->
    Ack =
	case find_object(State, ObjID) of
        none ->
            %?WARNING_MSG("obj_action: OBJECT NOT FOUND ~p", [ObjID]),
			object_not_found;
        #object{type=Type} = Object ->
		case Action of
			destroy ->
				case objects_server:can_destroy(Object) of
					false -> {none};
					Ticks -> {action, destroy, Ticks, ObjID}
				end;
			repair ->
				case objects_server:get_repair(Object) of
					{true, Ticks, _, _, _} -> {action, repair, Ticks, ObjID};
					false -> {none}
				end;
			lift -> case objects_server:is_liftable(Type) of 
						true -> {action, lift, 0, ObjID};
						false -> {none}
					end;
			_ ->
		% ВЕЩЬ -------------------------
		case inventory:is_item(Type) of
			true ->
				case Action of
                    right_click -> {action, pickup, 0, ObjID};
                    _ -> {none}
                end;
			false ->
		% РАСТЕНИЕ ------------------------
		case farming:is_plant(Type) of
			true ->
				case Action of
					harvest ->
						% выросло ли растение до стадии сбора урожая
						case farming:is_ripe(Object) of
							true -> {action, harvest, farming:get_harvest_ticks(Object), ObjID};
							false -> {none}
						end;
					right_click ->
						% выросло ли растение до стадии сбора урожая
						case farming:is_ripe(Object) of
							true -> 
								case skills:get_skill(FromState, farming) of
									false -> {none};
									_ -> {context, ["harvest"]}
								end;
							false -> {none}
						end;
					_ ->
						{none}
				end;
			false ->
		% СТРОЙКА -------------------------
		case build:is_build(Type) of
			true ->
				case Action of
                    right_click -> {action, open, 0, ObjID};
					{slot_click, N} -> {action, {slot_click, N}, 0, ObjID};
					{slot_wheel, N} -> {action, {slot_wheel, N}, 0, ObjID};
					build -> {action, build, build:get_ticks(Type), ObjID};
                    _ -> {none}
                end;
			false ->
		case build:is_context_have(Type) of
			true -> case Action of right_click -> {context, build:get_context(State, Object)}; _ -> {none}	end;
			false ->
				
        case Type of
            player -> case Action of
						right_click -> {target};
						_ -> {none}
					  end;
			
			claim -> case Action of
					{claim_init} -> 
						% клиент шлет действие
						{action, {claim_init}, 10000, ObjID};
					{claim_expand, R, E} ->
						{action, {claim_expand, R, E}, 2000, ObjID};						
                    right_click -> {action, open, 0, ObjID};
					_ -> {none}
                end;
			gatex_fence ->
					case Action of
					    right_click -> {action, open_close, 0, ObjID};
					    _ -> {none}
					end;
			gatey_fence ->
					case Action of
					    right_click -> {action, open_close, 0, ObjID};
					    _ -> {none}
					end;
			rabbit ->
					case Action of
					    right_click -> {action, capture, 0, ObjID};
					    _ -> {none}
					end;
            log -> case Action of
                    right_click -> {context, ["billet", "board"]};
                    _ -> {none}
                end;
			logy -> case Action of
                    right_click -> {context, ["billet", "board"]};
                    _ -> {none}
                end;
			stone -> case Action of
                    right_click -> {context, ["get_stone"]};
					make_runestone -> {action, make_runestone, 1000, ObjID};
					_ -> {none}
                end;
			runestone -> case Action of
					{add_text, Text} -> {action, {add_text, Text}, 2000, ObjID};
                    right_click -> {action, open, 0, ObjID};
					_ -> {none}
                end;
			bonfire -> game_bonfire:obj_action(State, Action, FromState, From, Object);
            box -> case Action of
                    right_click -> objects_server:open_object(Object, FromObjID);
					{inv_click, _Click} -> {action, Action, 0, ObjID};%{handled, obj_inventory_click(State, FromState, From, Object, main, Click)};
                    _ -> {none}
                   end;
            jar -> case Action of
                    right_click -> objects_server:open_object(Object, FromObjID);
					{inv_click, _Click} -> {action, Action, 0, ObjID};%{handled, obj_inventory_click(State, FromState, From, Object, main, Click)};
                    _ -> {none}
                   end;
			chair -> case Action of
					right_click -> {action, sit, 0, ObjID};
					{stand_up, _, _} -> {action, Action, 0, ObjID};
                    _ -> {none}
                   end;
            stump ->
                case Action of
                    right_click ->
						case equip:type_in_hands(stone_axe, FromState) of
							true -> {context, ["remove_stump"]};
							false -> {none}
						end;
                    _ -> {none}
                end;
			plant_wild ->
				case Action of
                    right_click -> {context, ["pick"]};
					_ -> {none}
                end;
			string_wild ->
				case Action of
                    right_click -> {context, ["pick"]};
					_ -> {none}
                end;

			%----------------------------------------------------------------------
			fir_tree ->
				case Action of
                    right_click -> {context, ["chop","take_branch"]};
                    _ -> {none}
                end;
			oak_tree ->
				case Action of
                    right_click -> {context, ["chop","take_branch"]};
                    _ -> {none}
                end;
			pine_tree ->
				case Action of
                    right_click -> {context, ["chop","take_branch"]};
                    _ -> {none}
                end;
			apple_tree ->
				case Action of
                    right_click -> {context, ["chop","take_branch","take_apple"]};
                    _ -> {none}
                end;
			pear_tree ->
				case Action of
                    right_click -> {context, ["chop","take_branch"]};
                    _ -> {none}
                end;
			%-----------------------------------
            _ -> {none}
        end end end end end end
    end,
	case Ack of
		object_not_found -> From ! {none, State#grid_state.coord}, State;
		{handled, S} -> 
			From ! {none, State#grid_state.coord},			
			S;
		_ -> From ! {ack, Ack}, State
	end.

%%--------------------------------------------------------------------------------------------------
% handle context action for object
obj_context_action(State, From, FromState, ObjID, Action) ->
    case find_object(State, ObjID) of
        none ->
            ?DEBUG("WARNING session: obj_context_action: OBJECT NOT FOUND ~p", [ObjID]),
			From ! {none, State#grid_state.coord}, State;
        #object{type=Type} = Object ->
			%----------------------------------------------------------------------------
			Ack =
			case farming:is_plant(Type) of
				true -> case Action of
					"harvest" -> {action, harvest, farming:get_harvest_ticks(Object), ObjID};
					_ -> none
					end;
				false ->
					
			case build:is_context_have(Type) of
				true -> build:context_action(State, Object, Action);
				false ->
					
			case Type of
            player -> none;
			stone -> case Action of
						 "get_stone" -> {action, get_stone, 5000, ObjID};
						 _ -> none
					 end;
			plant_wild -> case Action of
						 "pick" -> {action, pick, 5000, ObjID};
						 _ -> none
					 end;
			string_wild -> case Action of
						 "pick" -> {action, pick, 5000, ObjID};
						 _ -> none
					 end;	
			fir_tree -> case Action of
                         "chop" -> case equip:type_in_hands(stone_axe, FromState) of
										true -> {action, chop, 10000, ObjID};
										false -> {sysmsg, "need axe in hands"}
								   end;
                         "take_branch" -> {action, take_branch, 2000, ObjID};
						 _ -> none
					 end;
			oak_tree -> case Action of
                         "chop" -> case equip:type_in_hands(stone_axe, FromState) of
										true -> {action, chop, 20000, ObjID};
										false -> {sysmsg, "need axe in hands"}
								   end;
                         "take_branch" -> {action, take_branch, 2000, ObjID};
						 _ -> none
					 end;
			pine_tree -> case Action of
                         "chop" -> case equip:type_in_hands(stone_axe, FromState) of
										true -> {action, chop, 10000, ObjID};
										false -> {sysmsg, "need axe in hands"}
								   end;
                         "take_branch" -> {action, take_branch, 2000, ObjID};
						 _ -> none
					 end;
			pear_tree -> case Action of
                         "chop" -> case equip:type_in_hands(stone_axe, FromState) of
										true -> {action, chop, 10000, ObjID};
										false -> {sysmsg, "need axe in hands"}
								   end;
                         "take_branch" -> {action, take_branch, 2000, ObjID};
						 _ -> none
					 end;
			apple_tree -> case Action of
                         "chop" -> case equip:type_in_hands(stone_axe, FromState) of
										true -> {action, chop, 10000, ObjID};
										false -> {sysmsg, "need axe in hands"}
								   end;
                         "take_branch" -> {action, take_branch, 2000, ObjID};
						 "take_apple" -> {action, take_apple, 2000, ObjID};
						 _ -> none
					 end;
			log ->
				case Action of
                	"billet" ->
						case equip:type_in_hands(stone_axe, FromState) of
							true -> {action, billet, 2000, ObjID};
							false -> {sysmsg, "need axe in hands"}
						end;
                	"board" ->
						case equip:type_in_hands(stone_axe, FromState) of
							true -> {action, board, 3000, ObjID};
							false -> {sysmsg, "need axe in hands"}
						end;
					_ -> none
				end;
			logy ->
				case Action of
                	"billet" ->
						case equip:type_in_hands(stone_axe, FromState) of
							true -> {action, billet, 2000, ObjID};
							false -> {sysmsg, "need axe in hands"}
						end;
                	"board" ->
						case equip:type_in_hands(stone_axe, FromState) of
							true -> {action, board, 3000, ObjID};
							false -> {sysmsg, "need axe in hands"}
						end;
					_ -> none
				end;
            _ -> case Action of
					"remove_stump" -> {action, remove_stump, 20000, ObjID};
				 	_-> none
                 end
        end end end,
		From ! {ack, Ack},
    	State
    end.

%%--------------------------------------------------------------------------------------------------
% object action is completed. must handle it return msg {action_continue, NewName, NewTicks} | {action_end}
obj_action_completed(State, From, FromObjID, FromState, ActionName, ObjID) ->
    ?DEBUG("obj_action_completed ~p ~p", [ActionName, ObjID]),
    case find_object(State, ObjID) of
        none ->
            %?DEBUG("WARNING obj_action_completed: OBJECT NOT FOUND ~p", [ObjID]),
			From ! {none, State#grid_state.coord},
            State;
        #object{type=Type, coord={X,Y,Lv}, state=ObjectState, q=Q, hp={CurHP,CurSHP}, params=Params } = Object ->
			case ActionName of
				destroy ->
					case objects_server:can_destroy(Object) of
						false -> From ! {ack, {action_end}}, State;
						Ticks ->
							Str = player:get_strength(FromState),
							NewHP = CurHP - Str,
							if NewHP =< 0 ->
								From ! {ack, {action_end}},
								% get Sg
                            	Sg = map_server:get_sg(X,Y,Lv),
    	                        db_server:object_remove(Sg, ObjID),
            	                object_remove(State, ObjID);
							true ->
								From ! {ack, {action_continue, destroy, Ticks}},
								object_changed(State, ObjID, {X,Y,Lv}, Object#object{hp={NewHP,CurSHP}})
							end
					end;
				repair ->
					case objects_server:get_repair(Object) of
						{true, Ticks, SlotType, Count, HPRepair} ->
							From ! {ack, {check_items, SlotType, Count, self()}},
							receive
								{check_items, false} ->
									From ! {ack, {action_end}},
									From ! {sys_msg, "havnt items to repair this object"},
									State;
								{check_items, true} ->
									?DEBUG("repair obj!"),
									HP1 = CurHP + HPRepair,
									NewHP = if HP1 >= CurSHP ->
										From ! {ack, {action_end}}, CurSHP;
									true ->
										From ! {ack, {action_continue, repair, Ticks}}, HP1
									end,
									% убираем вещи из инвентаря
									From ! {take_items, none, SlotType, Count},
									% обновляем хп
									object_changed(State, ObjID, {X,Y,Lv}, Object#object{hp={NewHP,CurSHP}})
							after 5000 ->
									?WARNING_MSG("timeout wait player ack"),
									From ! {ack, {action_end}}, State
							end;
						_ -> From ! {ack, {action_end}}, State
					end;
                lift -> case objects_server:is_liftable(Type) of true ->
                        ?DEBUG("lift!"),
                        % player end action. no more nothing to do
                        From ! {ack, {action_end}},
						% если можно вообще поднять - поднимаем. если нельзя тут просто ничего делать не надо
						lift_up(State, Object, From, FromState);
					false -> From ! {ack, {action_end}}, State
					end;

                {lift_down, ToX, ToY} -> case objects_server:is_liftable(Type) of true ->
                        ?DEBUG("lift down!"),
                        % player end action
                        From ! {ack, {action_end}},
						% надо проверить коллизию в том месте куда хотим поставить объект
						case check_collision(State, Type, ToX, ToY, ?MOVE_LAND) of
							{fail_retry} ->
								State;
				            {no_collision} ->
								lift_down(State, Object, From, FromState, ToX, ToY);
				            _ ->
								?DEBUG("lift down: obj collision type=~p",[Type]),
				                State
				        end;
					false -> From ! {ack, {action_end}}, State
					end;
				_ ->
			% это иневнтарная вещь?
			case inventory:is_item(Type) of true ->
					case ActionName of
                        pickup ->
                            From ! {ack, {action_end}},
                            ?DEBUG("pickup ~p",[ObjectState]),
                            From ! {pickup, ObjectState},
                            State;
                        _ -> From ! {ack, {action_end}}, State
                    end;
				false ->
			% это растение?
			case farming:is_plant(Type) of true ->
					case ActionName of
						harvest ->
							From ! {ack, {action_end}},
							% созрело ли растение
							case farming:is_ripe(Object) of
								true ->
									S1 = object_remove(State, ObjID),
									db_server:object_remove(map_server:get_sg(X,Y,Lv), ObjID),
									{Items, SpawnItems} = farming:harvest(Object, FromState),
									From ! {player_add_items, Items},
									From ! {gain_exp, #exp{nature=15}},
									From ! {gain_ap, farming, 8},
									
									object_drop(S1, SpawnItems, X, Y);
								false ->
									State
							end;
						_ -> From ! {ack, {action_end}}, State
					end;
				false ->
			% это стройка?
			case build:is_build(Type) of
				true ->
					case ActionName of
                    open ->
                        ?DEBUG("open build < ~p > object ~p ~p",[Type, ObjID,Type]),
                        From ! {ack, {action_end}},
                        From ! {obj_visual_state, Object},
                        State;
					
                    build ->
                        ?DEBUG("build < ~p > ~p",[Type, ObjID]),
                        build:building_object(State, From, FromState, Object);
					{slot_click, N} ->
						?DEBUG("slot_click ~p",[N]),
						build:slot_click(State, N, Object, From, FromState);
					{slot_wheel, N} ->
						build:slot_wheel(State, N, Object, From, FromState);
                    _ -> From ! {ack, {action_end}}, State
                    end;
				false ->
			% все остальное
            case Type of
				player ->
					PlayerPid = get_player_pid(State, ObjID),
					case PlayerPid of
						none -> From ! {ack, {action_end}}, State;
						_ ->
							PlayerPid ! {action_complete, From, FromObjID, ActionName},
							From ! {ack, {action_end}},
							State
					end;
				claim -> case ActionName of
                    open ->
                        % player end action. no more nothing to do
                        From ! {ack, {action_end}},
						% шлем на клиент визуальное состояние объекта
						From ! {obj_visual_state, Object},
						State;
					{claim_init} ->
						From ! {ack, {action_end}},
						case ObjectState of
							none -> State;
							% только если это клайм владельца
							#obj_claim{owner_id=FromObjID} ->
								% координаты сгенерим
								TX = X div ?TILE_SIZE, TY = Y div ?TILE_SIZE,
								LT = TX - 1, TT = TY -1, RT = TX + 1, BT = TY + 1,
								NewClaimRect = {LT, TT, RT, BT},
								?DEBUG("new claim rect ~p",[NewClaimRect]),
								% надо проверить коллизии с другими клаймами
								case check_claim_size(State, #claim_personal{rect=NewClaimRect, owner_id=0}) of
									true -> 
										?DEBUG("check_claim_size = true"),
										NewClaim = 
										#claim_personal{object_id=ObjID, owner_id=FromObjID, rect=NewClaimRect, 
														time_last_action=time_server:get_time()},
										% проверим существует ли клайм 
										case db_server:claim_get(FromObjID) of
											#claim_personal{rect=OldClaimRect} = ClaimExist ->
												?DEBUG("claim exist ~p", [ClaimExist]),
												% заменим если существует
												db_server:claim_changed(NewClaim),
												% по старому ректу всем скажем удалить клайм
												send_all_notify(get_claim_grids(OldClaimRect, Lv), {claim_remove, FromObjID}),
												% по новому - всем добавить
												send_all_notify(get_claim_grids(NewClaimRect, Lv), {claim_change, NewClaim}),
												From ! {obj_visual_state, Object},
												State;
											_ ->
												db_server:claim_add(NewClaim),
												send_all_notify(get_claim_grids(NewClaimRect, Lv), {claim_change, NewClaim}),
												From ! {obj_visual_state, Object},
												State
										end;
												
									false -> 
										?DEBUG("check_claim_size = false"),
										From ! {sys_msg, "cant init claim"},
										State
								end;
							_ -> From ! {sys_msg, "are you hacker?"}, State
						end;
					{claim_expand, NewClaimRect, {NEC, NEI, NEN} = SetExp} ->
						?DEBUG("claim_expand ~p ~p",[NewClaimRect, SetExp]),
						From ! {ack, {action_end}},
						case ObjectState of
							% только если это клайм владельца
							#obj_claim{owner_id=FromObjID} ->
								% надо проверить коллизии с другими клаймами
								case check_claim_size(State, #claim_personal{rect=NewClaimRect, owner_id=FromObjID}) of
									true -> 
										?DEBUG("check_claim_size = true"),
										% проверим существует ли клайм 
										case db_server:claim_get(FromObjID) of
											#claim_personal{rect=OldClaimRect, exp=#exp{combat=OEC, industry=OEI, nature=OEN}} = ClaimExist ->
												?DEBUG("claim exist ~p", [ClaimExist]),
												case check_claim_exp(OldClaimRect, NewClaimRect, SetExp, FromState#player_state.exp) of
												true ->
													NewClaim = 
													#claim_personal{object_id=ObjID, owner_id=FromObjID, rect=NewClaimRect, 
																	exp = #exp{combat = OEC + NEC, industry = OEI + NEI, nature = OEN+NEN},
																	time_last_action=time_server:get_time()},
													% снимем экспу у игрока
													From ! {gain_exp, #exp{combat=-NEC, industry=-NEI, nature=-NEN}},
													% заменим если существует
													db_server:claim_changed(NewClaim),
													% по старому ректу всем скажем удалить клайм
													send_all_notify(get_claim_grids(OldClaimRect, Lv), {claim_remove, FromObjID}),
													% по новому - всем добавить
													send_all_notify(get_claim_grids(NewClaimRect, Lv), {claim_change, NewClaim}),
													From ! {obj_visual_state, Object},
													State;
												false ->
													From ! {sys_msg, "havnt exp to expand claim"},
													State
												end;
											_ ->
												From ! {sys_msg, "claim not exist!"},
												State
										end;
									false ->
										?DEBUG("check_claim_size = false"),
										From ! {sys_msg, "cant expand claim"},
										State
								end;
							_ -> State
						end
					end;
				rabbit -> case ActionName of
					capture ->
						From ! {ack, {action_end}},
						S1 = object_remove(State, ObjID),
						From ! {player_add_items, [inventory:spawn_item(rabbit_catched, 10+randoms:get(30), 1)]},
						From ! {gain_exp, #exp{nature=10}},
						From ! {gain_ap, hunting, 8},
						S1;
					_ -> From ! {ack, {action_end}}, State
					end;
				gatex_fence -> case ActionName of
					open_close -> 
						From ! {ack, {action_end}},
						#obj_gate{opened=IsOpened} = ObjectState,
						NewOpened = if IsOpened == true -> false; true -> true end,
						?DEBUG("fence opened=~p",[NewOpened]),
						grid:object_changed(
								State, ObjID, {X,Y,Lv},
								Object#object{ state=#obj_gate{opened = NewOpened} }
										 	);
					_ -> From ! {ack, {action_end}}, State
					end;
				gatey_fence -> case ActionName of
					open_close -> 
						From ! {ack, {action_end}},
						#obj_gate{opened=IsOpened} = ObjectState,
						NewOpened = if IsOpened == true -> false; true -> true end,
						?DEBUG("fence opened=~p",[NewOpened]),
						grid:object_changed(
								State, ObjID, {X,Y,Lv},
								Object#object{ state=#obj_gate{opened = NewOpened} }
										 	);
					_ -> From ! {ack, {action_end}}, State
					end;
				stone -> case ActionName of
					get_stone ->
						%From ! {ack, {action_end}},
						From ! {ack,{action_continue, get_stone, 5000}},
						From ! {gain_exp, #exp{industry=1,nature=5}},
						From ! {gain_ap, stone_cutting, 2},
						From ! {player_add_items, [inventory:spawn_item(stone_piece, 10, 1)]}, State;
					make_runestone ->
						From ! {ack, {action_end}},
						From ! {gain_ap, stone_cutting, 8},
						From ! {gain_exp, #exp{industry=15,nature=2}},

						object_changed(
								State, ObjID, {X,Y,Lv},
								#object{id=ObjID, type=runestone, coord={X,Y,Lv}}
										 );
						
                    _ -> From ! {ack, {action_end}}, State
                    end;
				bonfire -> game_bonfire:obj_action_completed(State, ActionName, From, FromState, Object);
				runestone -> case ActionName of
                    open ->
                        % player end action. no more nothing to do
                        From ! {ack, {action_end}},
						% шлем на клиент визуальное состояние объекта
						From ! {obj_visual_state, Object},

						State;
					{add_text, Text} ->
						From ! {ack, {action_end}},
						NewText = string:strip(string:left(case ObjectState of
							none -> Text;
							_ -> ObjectState ++ [13] ++ Text
						end, 100)),
						From ! {gain_exp, #exp{industry=1,nature=1}},
						O1 = Object#object{state=NewText},
						object_changed(
								State, ObjID, {X,Y,Lv},
								O1
										 );
					_ -> From ! {ack, {action_end}}, State
					end;
				plant_wild -> case ActionName of
					pick ->
						From ! {ack, {action_end}},
						From ! {gain_exp, #exp{nature=10}},
						
						TypeWild = case randoms:get(3) of
							1 -> seed_wild;
							2 -> seed_carrot;
							3 -> seed_wheat
					    end,

						From ! {player_add_items, [inventory:spawn_item(TypeWild, 1, 1)]},

                        Sg = map_server:get_sg(X,Y,Lv),
                        db_server:object_remove(Sg, ObjID),
                        object_remove(State, ObjID);
                    _ -> From ! {ack, {action_end}}, State
                    end;
				string_wild -> case ActionName of
					pick ->
						From ! {ack, {action_end}},
						From ! {gain_exp, #exp{nature=10}},
						

						From ! {player_add_items, [inventory:spawn_item(string, randoms:get(15), 1)]},

                        Sg = map_server:get_sg(X,Y,Lv),
                        db_server:object_remove(Sg, ObjID),
                        object_remove(State, ObjID);
                    _ -> From ! {ack, {action_end}}, State
                    end;					
				chair -> case ActionName of
					{stand_up, ToX, ToY} ->
						From ! {ack, {action_end}},
						stand_up(State, Object, From, FromState, ToX, ToY);
					sit -> 
						From ! {ack, {action_end}},
						case have_param(Params, links) of
							false -> sit_to(State, Object, From, FromState);
							_ -> State
						end;
                    _ -> From ! {ack, {action_end}}, State
                    end;
                box -> case ActionName of
                    open ->
                        ?DEBUG("open inventory object ~p ~p",[ObjID,Type]),
                        From ! {ack, {action_end}},						
						From ! {obj_visual_state, Object},
                        object_opened_add(State, Object, FromObjID);
					{inv_click, Click} ->
						From ! {ack, {action_end}},
						obj_inventory_click(State, FromState, From, Object, main, Click);
					{context_item, Action, ItemObjID} ->
						obj_inventory_context(State, FromState, From, Object, main, Action, ItemObjID);
                    _ -> From ! {ack, {action_end}}, State
                    end;
                jar -> case ActionName of
                    open ->
                        ?DEBUG("open inventory object ~p ~p",[ObjID,Type]),
                        From ! {ack, {action_end}},						
						From ! {obj_visual_state, Object}, State;
%%                         object_opened_add(State, Object, FromObjID);
					{inv_click, Click} ->
						From ! {ack, {action_end}},
						obj_inventory_click(State, FromState, From, Object, main, Click);
					{context_item, Action, ItemObjID} ->
						obj_inventory_context(State, FromState, From, Object, main, Action, ItemObjID);
                    _ -> From ! {ack, {action_end}}, State
                    end;
                log ->
                    case ActionName of
                        billet ->
						case equip:type_in_hands(stone_axe, FromState) of
						true ->
                            ?DEBUG("to billet"),
                            From ! {ack, {action_end}},
							From ! {gain_exp, #exp{industry=10,nature=20}},
                            % get Sg
                            Sg = map_server:get_sg(X,Y,Lv),
                            % remove tree from db
                            db_server:object_remove(Sg, ObjID),
                            % delete ?DEBUG
                            S1 = object_remove(State, ObjID),
                            % spawn billet
                            OFFSET = 5,
                            ID1 = world_server:get_next_id(),
                            I1 = #item{id=ID1, type=billet, q=Q, amount=1},
							HP = objects_server:get_hp(billet, Q, I1),
                            Obj1 = #object{id=ID1, type=billet, coord={X+OFFSET,Y+OFFSET,Lv}, q=Q, hp={HP,HP}, state=I1 },
                            St1 = object_spawn(S1, Obj1),

                            ID2 = world_server:get_next_id(),
                            I2 = #item{id=ID2, type=billet, q=Q, amount=1},
                            Obj2 = #object{id=ID2, type=billet, coord={X-OFFSET,Y+OFFSET,Lv}, q=Q, hp={HP,HP}, state=I2 },
                            St2 = object_spawn(St1, Obj2),

                            ID3 = world_server:get_next_id(),
                            I3 = #item{id=ID3, type=billet, q=Q, amount=1},
                            Obj3 = #object{id=ID3, type=billet, coord={X+OFFSET,Y-OFFSET,Lv}, q=Q, hp={HP,HP}, state=I3  },
                            St3 = object_spawn(St2, Obj3),

                            ID4 = world_server:get_next_id(),
                            I4 = #item{id=ID4, type=billet, q=Q, amount=1},
                            Obj4 = #object{id=ID4, type=billet, coord={X-OFFSET,Y-OFFSET,Lv}, q=Q, hp={HP,HP}, state=I4  },
                            object_spawn(St3, Obj4);
						false -> From ! {ack, {action_end}}, State end;
						board ->
						case equip:type_in_hands(stone_axe, FromState) of
						true ->
							?DEBUG("to board"),
                            From ! {ack, {action_end}},
							From ! {gain_exp, #exp{industry=10,nature=20}},
							Sg = map_server:get_sg(X,Y,Lv),
                            db_server:object_remove(Sg, ObjID),
                            S1 = object_remove(State, ObjID),

							ID1 = world_server:get_next_id(),
                            I1 = #item{id=ID1, type=board, q=10, amount=1},
							HP = objects_server:get_hp(board, Q, I1),
                            Obj1 = #object{id=ID1, type=board, coord={X,Y,Lv}, q=Q, hp={HP,HP}, state=I1 },
                            object_spawn(S1, Obj1);
						false -> From ! {ack, {action_end}}, State end;
                        _ -> From ! {ack, {action_end}}, State
                    end;
                logy ->
                    case ActionName of
                        billet ->
						case equip:type_in_hands(stone_axe, FromState) of
						true ->
                            ?DEBUG("to billet"),
                            From ! {ack, {action_end}},
							From ! {gain_exp, #exp{industry=10,nature=20}},
                            % get Sg
                            Sg = map_server:get_sg(X,Y,Lv),
                            % remove tree from db
                            db_server:object_remove(Sg, ObjID),
                            % delete ?DEBUG
                            S1 = object_remove(State, ObjID),
                            % spawn billet
                            OFFSET = 5,
                            ID1 = world_server:get_next_id(),
                            I1 = #item{id=ID1, type=billet, q=10, amount=1},
							HP = objects_server:get_hp(billet, Q, I1),
                            Obj1 = #object{id=ID1, type=billet, coord={X+OFFSET,Y+OFFSET,Lv}, q=Q, hp={HP,HP}, state=I1 },
                            St1 = object_spawn(S1, Obj1),

                            ID2 = world_server:get_next_id(),
                            I2 = #item{id=ID2, type=billet, q=10, amount=1},
                            Obj2 = #object{id=ID2, type=billet, coord={X-OFFSET,Y+OFFSET,Lv}, q=Q, hp={HP,HP}, state=I2 },
                            St2 = object_spawn(St1, Obj2),

                            ID3 = world_server:get_next_id(),
                            I3 = #item{id=ID3, type=billet, q=10, amount=1},
                            Obj3 = #object{id=ID3, type=billet, coord={X+OFFSET,Y-OFFSET,Lv}, q=Q, hp={HP,HP}, state=I3  },
                            St3 = object_spawn(St2, Obj3),

                            ID4 = world_server:get_next_id(),
                            I4 = #item{id=ID4, type=billet, q=10, amount=1},
                            Obj4 = #object{id=ID4, type=billet, coord={X-OFFSET,Y-OFFSET,Lv}, q=Q, hp={HP,HP}, state=I4  },
                            object_spawn(St3, Obj4);
						false -> From ! {ack, {action_end}}, State end;

						board ->
						case equip:type_in_hands(stone_axe, FromState) of
						true ->
							?DEBUG("to board"),
                            From ! {ack, {action_end}},
							From ! {gain_exp, #exp{industry=10,nature=20}},
							Sg = map_server:get_sg(X,Y,Lv),
                            db_server:object_remove(Sg, ObjID),
                            S1 = object_remove(State, ObjID),

							ID1 = world_server:get_next_id(),
                            I1 = #item{id=ID1, type=board, q=10, amount=1},
							HP = objects_server:get_hp(board, Q, I1),
                            Obj1 = #object{id=ID1, type=board, coord={X,Y,Lv}, q=Q, hp={HP,HP}, state=I1 },
                            object_spawn(S1, Obj1);
						false -> From ! {ack, {action_end}}, State
						end;
                        _ -> From ! {ack, {action_end}}, State
                    end;
				stump ->
					case ActionName of
                        remove_stump ->
						case equip:type_in_hands(stone_axe, FromState) of
						true ->
                            From ! {ack, {action_end}},
							From ! {gain_exp, #exp{industry=5,nature=8}},
                            % get Sg
                            Sg = map_server:get_sg(X,Y,Lv),
                            % remove tree from db
                            db_server:object_remove(Sg, ObjID),
                            % delete ?DEBUG
                            S1 = object_remove(State, ObjID),
                            % spawn billet
                            OFFSET = 5,
                            ID1 = world_server:get_next_id(),
                            I1 = #item{id=ID1, type=billet, q=10, amount=1},
							HP = objects_server:get_hp(billet, Q, I1),
                            Obj1 = #object{id=ID1, type=billet, coord={X,Y+OFFSET,Lv}, q=Q, hp={HP,HP}, state=I1  },
                            St1 = object_spawn(S1, Obj1),

                            ID2 = world_server:get_next_id(),
                            I2 = #item{id=ID2, type=billet, q=10, amount=1},
                            Obj2 = #object{id=ID2, type=billet, coord={X,Y-OFFSET,Lv}, q=Q, hp={HP,HP}, state=I2  },
                            object_spawn(St1, Obj2);
						false -> From ! {ack, {action_end}}, State end;
						_ -> From ! {ack, {action_end}}, State
					end;
				%------------------------------------------
                fir_tree -> trees:action_complete(State, ActionName, Object, From, FromState, FromObjID);
				oak_tree -> trees:action_complete(State, ActionName, Object, From, FromState, FromObjID);
				pine_tree -> trees:action_complete(State, ActionName, Object, From, FromState, FromObjID);
				pear_tree -> trees:action_complete(State, ActionName, Object, From, FromState, FromObjID);
				apple_tree -> trees:action_complete(State, ActionName, Object, From, FromState, FromObjID);


				%-------------------------------
                _ -> From ! {ack, {action_end}}, State
            end end end end end
    end.

%%--------------------------------------------------------------------------------------------------
% найти объект в заданной области и используя функцию фильтрации
find_filter_objects([], _, _) -> none;
find_filter_objects(List, Fun, Rect) when is_list(List) ->
	[#object{type=Type, coord={X,Y,_}}=O|Tail] = List,
	case Rect of
		none ->
			case Fun(Type) of
				true -> O;
				false ->
					find_filter_objects(Tail, Fun, Rect)
			end;
		_ ->
	case coord:get_in_rect({X,Y}, Rect) of
		false ->
			find_filter_objects(Tail, Fun, Rect);
		true ->
	case Fun(Type) of
		true ->
			O;
		false ->
			find_filter_objects(Tail, Fun, Rect)
	end
	end
	end;

find_filter_objects(#grid_state{dyn=Dyn, static=Static, tick=Tick}, Fun, Rect) ->
	ObjList = lists:append([Static, Tick, Dyn]),
	find_filter_objects(ObjList, Fun, Rect).

%%--------------------------------------------------------------------------------------------------
% add id to opened list of object
object_opened_add(State, Object, FromObjID) ->
    ?DEBUG("object_opened_add ~p",[FromObjID]),
    #object{id=ObjID, params=Params} = Object,
    % find in params
    case lists:keyfind(opened, 1, Params) of
        false ->
            % set new param
            Param = {opened, [FromObjID]},
            object_set_param_notify(State, ObjID, Param);
        {opened, List} ->
            % find id in list
            case lists:member(FromObjID, List) of
                % add id to list
                false ->
                    Param = {opened, [FromObjID|List]},
                    object_set_param_notify(State, ObjID, Param);
                true ->
                    % id already in list. nothing todo.
                    ?DEBUG("SESSION: warining. open object already opened ~p player: ~p",[ObjID, FromObjID]),
                    State
        end
    end.

%%--------------------------------------------------------------------------------------------------
% remove object with confirm message
object_remove_confirm(State,From,ObjID) ->
    #grid_state{ tick = Ticks,
              	dyn = Dyns,
              	static = Static,
              	dynamic_pids = DynPids,
				coord = GC
				  } = State,
    % send to all players
    ?DEBUG("dyn pids ~p", [DynPids]),
	lists:foreach(fun(#dynamic_pid{pid=P}) -> P ! {obj_remove, ObjID} end, DynPids),
    % find object
    case lists:keytake(ObjID, #object.id, Dyns) of
    false ->
        case lists:keytake(ObjID, #object.id, Ticks) of
        false ->
            case lists:keytake(ObjID, #object.id, Static) of
            false ->
                ?DEBUG("WARNING! session object not found ~p", [ObjID]),
                From ! {none, GC},
                State;
            {value, O, NewList} ->
                ?DEBUG("object removed from session static_objects ~p", [O]),
                #object{coord={X,Y,Lv}} = O,
                From ! {ack, {object_remove_confirm_exist_ok, X,Y,Lv}},
                State#grid_state{ static = NewList }
            end;
        {value, O, NewList} ->
            ?DEBUG("object removed from session tick_objects ~p", [O]),
            #object{coord={X,Y,Lv}} = O,
            From ! {ack, {object_remove_confirm_exist_ok, X,Y,Lv}},
            State#grid_state{ tick = NewList }
        end;
    {value, O, NewList} ->
        ?DEBUG("object removed from session dynamic_objects ~p", [O]),
        % need kill object process
        case get_player_pid(DynPids, ObjID) of
            none -> ok;
            Pid -> Pid ! {finish}
        end,
        #object{coord={X,Y,Lv}} = O,
        From ! {ack, {object_remove_confirm_exist_ok, X,Y,Lv}},
        State#grid_state{ dyn = NewList }
    end.

%%--------------------------------------------------------------------------------------------------
stand_up(#grid_state{coord=GC} = State, 
		 #object{type=ObjType, coord={X,Y,Lv}, id=ObjID} = Object, 
		 FromPlayerPid, 
		 #player_state{charid=CharID}, 
		 ToX, ToY) ->
	?DEBUG("stand_up ~p ~p ~p",[Object, ToX, ToY]),
	DO = case ObjType of
		chair ->
			% на какое расстояние от объекта будут ставится игрока
			11;
		_ -> 20
	end,
	DX = ToX - X, DY = ToY - Y,
	D = coord:get_distance(0, 0, DX, DY),
	SX = X + round(DX / D * DO),
	SY = Y + round(DY / D * DO),
	
	case check_collision(State, player, SX, SY, ?MOVE_LAND, CharID) of
								{fail_retry} ->
									State;
					            {no_collision} ->	
	
		% куда поставить игрока?
		ToGC = map_server:get_sg_grid({SX, SY, Lv}),
		% надо узнать разные ли гриды у того кто поднимает и у этого объекта
		if			
			% все происходит в том же гриде
			GC == ToGC ->
				% убираем линк на объект
				S1 = unlink_object(State, follow, CharID),
				FromPlayerPid ! {stand_up, ObjID, SX, SY},
				object_pos(S1, CharID, SX, SY, true);
			true ->
				FromPlayerPid ! {sys_msg, "cant stand up here"},
				State
		end;
		_ -> State % any collision
	end.

%%--------------------------------------------------------------------------------------------------
% сесть в объект (слинковаться с ним)
sit_to(#grid_state{coord=GC} = State,
		#object{id=ObjID 
%% 				params=Params, coord={OX, OY, _}, type=ObjType 
			   } = 
				   Object,
		FromPlayerPid,
		FromState) ->
	% узнаем в каком гриде игрок
	Player_GC = map_server:get_sg_grid(FromState#player_state.coord),
%% 	Player_GPid = player_server:get_work_grid(FromState),
	#player_state{charid = CharID} = FromState,
	% надо узнать разные ли гриды у того кто поднимает и у этого объекта
	if
		% все происходит в том же гриде
		Player_GC == GC ->
			case find_object(State, CharID) of
				none ->
					% игрок в гриде не найден
					error(liftup_player_not_found);
				_ ->
					% линукем игрока и объект  
					S1 = link_objects(State, ObjID, CharID),
					FromPlayerPid ! {linked_to, Object},
					object_set_param(S1, CharID, {direction, objects_server:get_direction(Object)}, true)
					
			end;

		% если гриды разные
		true ->
			FromPlayerPid ! {sys_msg, "cant sit here"},
			State
	end.
	

%%--------------------------------------------------------------------------------------------------
% поднять объект
lift_up(#grid_state{coord=GC} = State,
		#object{id=ObjID, params=Params} = Object,
		FromPlayerPid,
		FromState) ->
	?DEBUG("lift_up ~p", [Object]),

	% объект уже закреплен за кем то?
	case have_param(Params, follow) of
		true -> 
			FromPlayerPid ! {sys_msg, "cant lift up now"},
			State;
		false ->
	% объект свободен. и его можно смело тащить
	% узнаем в каком гриде игрок
	Player_GC = map_server:get_sg_grid(FromState#player_state.coord),
	#player_state{charid = CharID} = FromState,
	% надо узнать разные ли гриды у того кто поднимает и у этого объекта
	if
		% все происходит в том же гриде
		Player_GC == GC ->
			case find_object(State, CharID) of
				none ->
					% игрок в гриде не найден
					error(liftup_player_not_found);
				_ ->
					% если игрок уже имеет связи - это явная ошибка
					case have_param(Params, links) of
						false ->
							% линукем игрока и объект который поднимаем
							FromPlayerPid ! {lift_up, Object},
							S1 = object_remove_param_notify(State, ObjID, opened),
							link_objects(S1, CharID, ObjID);
						_ ->
							error(liftup_already_have_links)
					end
			end;

		% если гриды разные
		true ->
			FromPlayerPid ! {sys_msg, "cant lift up here"},
			State
	end
	end.

%%--------------------------------------------------------------------------------------------------
lift_down(
  	#grid_state{level=Lv 
%% 				neighbors=NS
			   } = State,
	#object{id=ObjID, type=ObjType, coord=ObjCoord}, FromPlayerPid, 
	#player_state{coord={PX,PY,_}} = FromState, 
	ToX, ToY
		 ) ->

	% куда ставим объект
	ToGC = map_server:get_sg_grid({ToX, ToY, Lv}),
	% направление
	A = math:atan2(PY-ToY, PX-ToX),
	A1 = if A<0 -> 2*math:pi()+A; true -> A end,
	Dir = round((2*A1) / (math:pi()/2) + 3) rem 8,	
	?DEBUG("lift down direction: ~p",[Dir]),


	% узнаем в каком гриде игрок
	Player_GC = map_server:get_sg_grid(FromState#player_state.coord),
	% надо узнать разные ли гриды у того кто поднимает и у этого объекта
	if
		% все происходит в том же гриде
		Player_GC == ToGC ->
			case find_object(State, ObjID) of
				 false -> error(object_not_found);
				 _ -> 
					case objects_server:can_lift_down_on_tile(ObjType, get_tile(State, ToX, ToY)) of
						false ->
							?DEBUG("cant lift down here"),
							State;
						true ->
							% убираем линк на объект
							S1 = unlink_object(State, follow, ObjID),
							DO = find_object(S1, ObjID),
							S2 = case objects_server:set_direction(DO, Dir) of
									 false -> S1;
									 DObj -> object_changed(S1, ObjID, ObjCoord, DObj)
								 end,
							FromPlayerPid ! {lift_down, ObjID},
							object_pos(S2, ObjID, ToX, ToY, true)
					end
			end;
		true ->
			FromPlayerPid ! {sys_msg, "cant lift down here"},
			State
%% 			% ставим в другой грид
%% 			ToPid = case lists:keyfind(ToGC, #neighbor_rec.coord, NS) of
%% 				false -> error({lift_down_havnt_to_pid, ToGC});
%% 				#neighbor_rec{pid=P} -> P
%% 			end,
%% 			{LB, TB, RB, BB} = cutils:get_object_bounds(ObjType),
%% 			LockArea = #grid_lock_area{area={ToX+LB, ToY+TB, ToX+RB, ToY+BB}, owner=FromPlayerPid},
%% 			% шлем его тому гриду где находится игрок
%% 			ToPid ! {lock_data, self(), LockArea},
%% 			receive
%% 				{lock_data, _, _} -> State;
%% 				
%% 				{lock_fail} ->
%% 					% другой грид сообщает об ошибке блокировки. надо попытатся в следующий раз
%% 					State;
%% 
%% 				{grid_state, #grid_state{coord=RGC} = RemoteState} ->
%% 					% запоминаем стейт соседа
%% 					S2 = case lists:keytake(RGC, #neighbor_rec.coord, State#grid_state.neighbors) of
%% 						false ->
%% 							% пришел стейт не от моего соседа - крит
%% 							?CRITICAL_MSG("grid state not from my neighbor! ~p",[RGC]),
%% 							State;
%% 						{value, N, Tail} ->
%% 							% обновим состояние соседа
%% 							State#grid_state{neighbors=[
%% 										N#neighbor_rec{is_loaded=true, state_recv_time=time_server:get_time(), state=RemoteState#grid_state{neighbors=[]}}
%% 																 |Tail]}
%% 					end,
%% 					
%% 					case objects_server:can_lift_down_on_tile(ObjType, get_tile(RemoteState, ToX, ToY)) of
%% 					true ->
%% 						S22 = unlink_object(S2, follow, ObjID),					
%% 						% take объект который несли
%% 						{Taked, S3} = take_object(S22, ObjID),
%% 						ToPid ! {lift_down, ObjID, Taked, ToX, ToY, LockArea},
%% 						FromPlayerPid ! {lift_down, ObjID},
%% 						S3;
%% 					false ->
%% 						?DEBUG("cant lift down here"),
%% 						S2
%% 					end
%% 			after 1500 ->
%% 					error(timeout_wait_lock_liftup)
%% 			end
	end.
	
%%--------------------------------------------------------------------------------------------------
% связать 2 объекта. подчиненный всегда будет двигатся за главным
link_objects(State, MainObjID, SlaveObjID) ->
	?DEBUG("link objects ~p ~p",[MainObjID, SlaveObjID]),
	S1 = object_set_param(State, SlaveObjID, {follow, MainObjID}, true),
	case find_object(State, MainObjID) of
		none -> error(link_main_object_not_found);
		#object{coord={MX,MY,_}, params=Params} ->
			% ищем связи в параметрах объекта
			NewParam = case lists:keytake(links, 1, Params) of
				false -> {links, [SlaveObjID]};
				{value, {links, OldList}, _} ->
					% нашли уже существующие связи внутри объекта, надо добавить в список новый ид если его еще там нет
					{links, lists:umerge(lists:usort(OldList), [SlaveObjID])}
			end,
			object_set_param(
			  	object_pos(S1, SlaveObjID, MX, MY, false),
			MainObjID, NewParam, true)
	end.

%%--------------------------------------------------------------------------------------------------
% разлинкует объекты, только убирает параметры, позицию НЕ ставит.
% обязательно поставить объекту позицию после вызова этой функции
unlink_object(State, follow_list, []) -> State;
unlink_object(State, follow_list, [H|T]) ->
	unlink_object(
	  	unlink_object(State, follow, H),
		follow_list, T);

unlink_object(State, follow, FollowObjID) ->
	case find_object(State, FollowObjID) of
		none ->
			% такого объекта нет. ошибка
			error(unlink_obj_not_found);
		#object{params=Params} ->
			case lists:keyfind(follow, 1, Params) of
				false -> error(unlink_param_not_found);
				{follow, MainObjID} ->
					case find_object(State, MainObjID) of
						none -> error(unlink_obj_main_not_found);
						#object{params=MainParams} ->
							case lists:keyfind(links, 1, MainParams) of
								false -> error(unlink_obj_main_havnt_links);
								{links, [FollowObjID]} ->
									% была только одна связь на этот объект. удаляем параметр
									S1 = object_remove_param_notify(State, FollowObjID, follow),
									object_remove_param_notify(S1, MainObjID, links);
								{links, LinksList} ->
									S1 = object_remove_param_notify(State, FollowObjID, follow),
									NewLinksList = lists:delete(FollowObjID, LinksList),
									object_set_param_notify(S1, MainObjID, {links, NewLinksList})
							end
					end
			end
	end.


%%--------------------------------------------------------------------------------------------------
player_close_obj(State, From, ObjID) ->
    ?DEBUG("player_close_obj ~p ~p", [From, ObjID]),
    #grid_state{ dynamic_pids = D } = State,
    % find player charid
    case lists:keyfind(From, #dynamic_pid.pid, D) of
        false -> State;
        #dynamic_pid{objid=CharID} ->
            ?DEBUG("player found"),
            % find object
            case find_object(State, ObjID) of
                none -> State;
                #object{type=Type, coord=Coord} = Object ->
                    ?DEBUG("object finded"),
					% если это стройка
					S1 = case build:is_build(Type) of
						true ->
							% если стройка пустая
							case build:is_empty(Object) of
								% удалим объект
								true ->
									ObjSg = map_server:get_sg(Coord),
									db_server:object_remove(ObjSg, ObjID),
									object_remove(State, ObjID);
								false -> State
							end;
						false -> State
					end,
                    % remove from opened in object
                    object_opened_del(S1, Object, CharID)
            end
    end.

%%--------------------------------------------------------------------------------------------------
% remove from opened list of object
object_opened_del(State, Object, FromObjID) ->
    #grid_state{ notify = Notify } = State,
    #object{id=ObjID, params=Params } = Object,
    % find opened param
    case lists:keyfind(opened, 1, Params) of
        false ->
			%?DEBUG("no opened param!"),
            State;
        {opened, List} ->
            case lists:member(FromObjID, List) of
                true ->
                    ?DEBUG("id in opened list"),
                    NewList = lists:delete(FromObjID, List),
                        if length(NewList) == 0 ->
                               send_all_notify(Notify, {obj_remove_param, ObjID, opened}),
                               object_remove_param(State, ObjID, opened);
                           true ->
                               object_set_param_notify(State, ObjID, {opened, NewList})
                        end;
                false ->
                    ?DEBUG("no id in opened list!"),
                    State
            end
    end.

%% --------------------------------------------------------------------
plow_tile(#grid_state{coord= {Sg, Grid} = GC, tiles=Tiles, notify = Notify, level = Lv} = State, 
		  X, Y) ->
	case map_server:get_sg_grid_index(X, Y, Lv) of
		coord_error -> State;
		{TSg, TGrid, Index} -> 
		if
		GC == {TSg, TGrid} ->
			% тайл в этом гриде. надо обновить
			#tile{type=OldType} = cutils:get_tile(Tiles, Index),
			?DEBUG("plow tile: ~p", [OldType]),
			case map_server:can_plow(OldType) of
				false -> State;
				true ->
					?DEBUG("plow tile!"),
					Tile = #tile{type=plowed},
					NewTiles = cutils:set_tile(Tiles, Index, Tile),
					db_server:map_set_grid(Sg, Grid, NewTiles),
					S1 = State#grid_state{tiles=NewTiles},
					send_all_notify(Notify, {grid_state, S1}),
					S1
			end;
		true ->
			% это тайл не из нашего грида. пропускаем.
			State
		end
	end.
	
claim_remove(#grid_state{claims=Old, notify=N} = State, FromObjID) -> 
	case lists:keytake(FromObjID, #claim_personal.owner_id, Old) of
		false -> ?DEBUG("claim_remove : no such claim"), State;
		{value, _, Tail} ->
			send_all_notify(N, {claim_remove, FromObjID}),
			State#grid_state{claims=Tail}
	end.
claim_change(#grid_state{claims=Old, notify=N} = State, Claim) ->
	send_all_notify(N, {claim_change, Claim}),
	case lists:keytake(Claim#claim_personal.owner_id, #claim_personal.owner_id, Old) of
		false -> ?DEBUG("claim_change : no such claim, add"), State#grid_state{claims=[Claim|Old]};
		{value, _, Tail} ->
			State#grid_state{claims=[Claim|Tail]}
	end.
claim_send_all(State, From) when is_record(State, grid_state) ->
	#grid_state{claims=L} = State,
	claim_send_all(L, From);
claim_send_all([], _From) -> ok;
claim_send_all([H|T], From) -> From ! {claim_change, H}, claim_send_all(T, From).

% ##################################################################################################
% SYSTEM
% ##################################################################################################

%% --------------------------------------------------------------------
% return new obj list
add_object_to_list(List, Object) -> lists:keystore(Object#object.id, #object.id, List, Object).

%%--------------------------------------------------------------------------------------------------
% отправить сообщение всем процессам из списка
send_all_notify([], _) -> ok;
send_all_notify([H|T], Msg) -> H ! Msg, send_all_notify(T, Msg).

send_all_dyns(NS, Dyns, Msg) ->
	send_all_dyns(Dyns, Msg),
	lists:foreach(fun
					 (#neighbor_rec{pid=NP}) ->
						  NP ! {send_all_dyns, Msg}
				  end, NS).

send_all_dyns([], _) -> ok;
send_all_dyns([#dynamic_pid{pid=P}|T], Msg) -> P ! Msg, send_all_dyns(T, Msg).  

%%--------------------------------------------------------------------------------------------------
% заспавнить динамический объект
spawn_dynamic_object(DynPids, Object) ->
	case lists:keyfind(Object#object.id, #dynamic_pid.objid, DynPids) of
		false ->
			case Object#object.type of
				player -> ?CRITICAL_MSG("ERROR! no player pid! ~p", [Object#object.id]), error(error_spawn_player_pid);
				_ -> object:spawn_dynamic_process(self(),Object)
			end;
		#dynamic_pid{pid=P} -> {exist, P}
	end.

%%--------------------------------------------------------------------------------------------------
get_tick_count() ->
    {_,S,M} = now(),
    (S*1000)+(M div 1000).

%%--------------------------------------------------------------------------------------------------
% запросить блокировку соседей для коллизии
request_collision_grids(#grid_state{neighbors=NS}, List, LockArea) ->
	lists:foreach(fun (CGC) ->
		case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
			% если грида нет в списке соседей - краш
			false -> error({not_neighbor_grid, CGC});
			% если грид есть в списке соседей - шлем ему мессагу
			#neighbor_rec{pid=P} -> P ! {lock_data, self(), LockArea}
		end
		end, List).

%%--------------------------------------------------------------------------------------------------
% отправить объекты в области
send_objects_in_rect(_From, [], _Rect) -> ok;
send_objects_in_rect(From, ListObj, Rect) ->
    [Object | T] = ListObj,
    #object{coord={X,Y,_Lv}} = Object,
    case a1_utils:get_in_rect({X,Y}, Rect) of
        true ->
            send_obj_pos(From, Object),
            send_objects_in_rect(From, T, Rect);
        false -> send_objects_in_rect(From, T, Rect)
    end.

%%--------------------------------------------------------------------------------------------------
send_obj_pos(From, Object) ->
	#object{params=Params} = Object,
    case lists:keyfind(line_move, 1, Params) of
        false -> From ! {obj_pos, Object};
        LP -> From ! {obj_set_param, Object, LP}
    end.

%%--------------------------------------------------------------------------------------------------
% send object info to client
send_obj_info(From, Object) ->
    #object{type=Type, params=Params, hp=HP } = Object,
    % clear all params
    %From ! {obj_clear_params, ObjID},
    % send pos, list of params
    case lists:keyfind(line_move, 1, Params) of
        false -> From ! {obj_pos, Object};
        _P -> ok
    end,
    % drawable get from object. NOT from params list.
    case objects_server:get_drawable(Object) of
        false -> ok;
        A -> From ! {obj_set_param, Object, A}
    end,
	case farming:is_plant(Type) of
		true -> send_params(From, Object, farming:get_add_param(Object));
		false -> ok
	end,
	case objects_server:get_direction(Object) of
		none -> ok;
		D -> From ! {obj_set_param, Object, {direction, D} }
	end,
	From ! {obj_set_param, Object, {obj_hp, HP} },
	% всегда показывать (не скрывать возле игрока)
	case objects_server:get_always_show(Type) of
		true -> 
			From ! {obj_set_param, Object, {ashow, 0} };
		_ ->
			ok
	end,
    send_params(From, Object, Params).

%%--------------------------------------------------------------------------------------------------
% send params list to player
send_params(_From, _, []) -> ok;
send_params(From, Object, Params) ->
    [Param|T] = Params,
    From ! {obj_set_param, Object, Param},
    send_params(From, Object, T).

%%--------------------------------------------------------------------------------------------------
% add param to param list
set_param(OldParams, {ParamName, Data}) ->
	case ParamName of
		exp -> OldParams;
		_ ->
    		case lists:keytake(ParamName, 1, OldParams) of
        		false ->
            		[{ParamName, Data}|OldParams];
		        {value, _Param, NewList} ->
        		    [{ParamName, Data}|NewList]
			end
    end.

%%--------------------------------------------------------------------------------------------------
have_param(Params, ParamName) ->
	case lists:keyfind(ParamName, 1, Params) of
		false -> false;
		P -> P
	end.




%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
%%--------------------------------------------------------------------------------------------------
% проверить что координаты моего грида
check_coords(#grid_state{level = Lv, coord = GC}, X, Y) ->
	case map_server:get_sg_grid(X, Y, Lv) of
		GC -> true;
		_ -> false
	end.

%%--------------------------------------------------------------------------------------------------
% получить тайл по координатам
% вернет тайл
get_tile(#grid_state{level=Lv, tiles=MapData, coord=GC}, X, Y) ->
	{Sg,Grid,Index} = map_server:get_sg_grid_index(X, Y, Lv),
	if
		GC == {Sg, Grid} -> 
			cutils:get_tile(MapData, Index);
		true ->
			?CRITICAL_MSG("get_tile : not this grid coord"),
			error(get_tile_not_this_grid)
	end.


%%--------------------------------------------------------------------------------------------------
% разблокировать гриды в коллизии и передать нужному гриду взятый объект и стейт
unlock_grids(NS, MyNotify, LockGrids, LockArea, ThatGC, Taked, SetParam, ObjID, X, Y) ->
	?DEBUG("unlock grids ~p taked ~p",[ThatGC, Taked]),
	% надо передать его в залоченый грид. в тот куда он попал.
	lists:foreach(fun (#grid_state{coord=CGC, notify=ThatNotify}) ->
		% нашли тот грид куда он попал
		if (CGC == ThatGC) ->
			% смержим его подписчиков и моих. и вышлем всем мессагу о передвижении объекта
			AllNotify = lists:umerge(
									lists:usort(ThatNotify),
									lists:usort(MyNotify)
									),

			?DEBUG("no_collision grid end point ~p all notify: ~p = [~p + ~p]",[ThatGC, AllNotify, ThatNotify, MyNotify]),
			% шлем ему "разблокировку" с сообщением о передвижении
			case lists:keyfind(ThatGC, #neighbor_rec.coord, NS) of
				false ->
						?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
						error(not_my_neighbor_grid);
			#neighbor_rec{pid=P} ->
				% разлочим его и скажем чтобы поставил объект в себя, уведомил всех кого нужно
				P ! {move_request_unlock, LockArea, Taked, ObjID, X, Y, SetParam, AllNotify}

			end;

		true ->
			% это другой грид. в который объект не попал
			% просто разблокируем его
			case lists:keyfind(CGC, #neighbor_rec.coord, NS) of
			false ->
						?CRITICAL_MSG("not my neighbor grid ~p", [CGC]),
						error(not_my_neighbor_grid);
			#neighbor_rec{pid=P} ->
					P ! {unlock, self(), LockArea}
			end
		end
 	end, LockGrids),
	?DEBUG("unlock done").

get_player_pid(#grid_state{dynamic_pids=D}, ObjID) ->
	case lists:keyfind(ObjID, #dynamic_pid.objid, D) of
		false -> none;
		#dynamic_pid{account_id=0} -> error(get_player_pid_zero_account_id);
		#dynamic_pid{pid=P} -> P
	end.

    
get_neighbor_notify(#grid_state{neighbors=NS, notify=Notify}) ->
	SNU = lists:usort(Notify),
	?DEBUG("my notify ~p",[SNU]),
	All_N = lists:umerge([
		  SNU |
			  lists:map(fun (#neighbor_rec{state=#grid_state{notify=NN}}) ->
								 ?DEBUG("nei notify = ~p",[NN]),
								 NU = lists:usort(NN),
								 ?DEBUG("nei usorted ~p",[NU]),
								 NU
								 end, NS)]),
	?DEBUG("final nei ~p", [All_N]),
	All_N.

is_locked(_, []) -> false;
is_locked(LockArea, [#grid_lock_area{area=R1}|T]) ->
	case coord:is_rect_intersect(R1, LockArea) of
		true -> true;
		false ->
			is_locked(LockArea, T)
	end.

try_lock_data(State, From, LockArea) ->
	#grid_state{locked=LA} = State,
	% заблокирована ли эта область?
	case is_locked(LockArea, LA) of
		true -> 
			From ! {lock_fail}, 
			State;
		false -> 
			% шлем стейт без соседей
			From ! {grid_state, State},
			% добавляем область в залоченные 
			State#grid_state{locked=[LockArea|LA]}
	end.
unlock_area(#grid_state{locked=L}=S, LockArea) -> 
	?DEBUG("unlock area ~p -------------------------------------------------------------------",[LockArea]),
	S#grid_state{locked=lists:delete(LockArea, L)}.

% -------------------------------------------------------------------------
% загрузить клаймы затрагивающие этот грид
% вернет список
load_claims(Sg, Grid) ->
	parse_claims(db_server:claim_load(Sg, Grid), []).
parse_claims([], Acc) -> Acc;
parse_claims([H|Tail], Acc) ->
	[OwnerID, ObjectID, L,T,R,B, EC,EI,EN] = H,
	parse_claims(Tail, [#claim_personal{owner_id=OwnerID, object_id=ObjectID, rect={L,T,R,B}, exp=#exp{combat=EC, industry=EI, nature=EN}} | Acc]).

% -------------------------------------------------------------------------
% проверить можно ли сделать клайм заданного размера, или изменить размер
check_claim_size(State, Claim) when is_record(State, grid_state) ->
	#grid_state{claims = List} = State,
	check_claim_size(List, Claim);
check_claim_size([], _) -> true;
check_claim_size([#claim_personal{owner_id=HO, rect=HR}|T], #claim_personal{rect=R, owner_id=Owner} = Claim) ->
	if
		(HO == Owner) -> check_claim_size(T, Claim);
		true ->
			case coord:is_rect_intersect(coord:add_rect_size(HR, 2), R) of
				true -> false;
				_ -> check_claim_size(T, Claim)
			end
	end.

% -------------------------------------------------------------------------
% получить список ид процессов гридов попадающих в рект клайма. только реально существующие процессы
get_claim_grids({L,T,R,B}, Lv) ->
	ListGC = get_claim_gridsy(Lv, L div ?GRID_SIZE, R div ?GRID_SIZE, T div ?GRID_SIZE, B div ?GRID_SIZE, []),
	get_grids_pids(ListGC, []).
get_grids_pids([], Acc) -> Acc;
get_grids_pids([H|T], Acc) -> 
	case world_server:get_grid_exist(H) of
		false -> get_grids_pids(T, Acc);
		P -> get_grids_pids(T, [P|Acc])
	end.
get_claim_gridsy(Lv, X1, X2, CY, EndY, Acc) ->
	if 
		(CY > EndY) ->
			Acc;
		true ->
			get_claim_gridsy(Lv, X1, X2, CY+1, EndY, 
							 get_claim_gridsx(Lv, CY, X1, X2, []) ++ Acc)
	end.
get_claim_gridsx(Lv, CY, CX, EndX, Acc) ->
	if 
		(CX > EndX) -> Acc;
		true -> 
			get_claim_gridsx(Lv, CY, CX+1, EndX, [ map_server:get_sg_grid(CX*?GRID_SIZE*?TILE_SIZE, CY*?GRID_SIZE*?TILE_SIZE, Lv) | Acc ])
	end.

check_claim_exp(OldRect, NewRect, {EC, EI, EN}, #exp{combat=PC, industry=PI, nature=PN}) ->
	if 
		(PC >= EC) andalso (PI >= EI) andalso (PN >= EN) ->
			{OL, OT, OR, OB} = OldRect,
			{L,T,R,B} = NewRect,
			TotalExp = EC+EI+EN,
			OldArea = (OR - OL + 1) * (OB - OT + 1),
			NewArea = (R - L + 1) * (B - T + 1),
			ReqExp = (NewArea - OldArea) * 100,
			?DEBUG("req exp=~p   client exp=~p",[ReqExp, TotalExp]),
			if 
				TotalExp >= ReqExp -> true;
				true -> false
			end;
		true -> false
	end.

% -------------------------------------------------------------------------
% клик в инвентаре объекта
% InvTag - специфичный для объекта идентификатор инвентаря
obj_inventory_click(State, FromState, From, Object, InvTag, Click) ->
	#player_state{item_hand=Hand, coord={PX, PY, _}} = FromState,
	
	
	case inventory:item_click2(inventory:get_object_inv_type(Object#object.type, InvTag),  
							   inventory:get_object_inventory(Object, InvTag), 
							   Hand, 
							   Click#inv_click.inv_objid, 
							   Click#inv_click.objid, 
							   Click#inv_click.x, 
							   Click#inv_click.y, 
							   Click#inv_click.btn,
							   Click#inv_click.mod) of
		{NeedInv, NewInv, NeedHand, NewHand} ->
			?DEBUG("item: ack: ~p ~p ~p ~p", [NeedInv, NewInv, NeedHand, NewHand]),
			
			% меняем руку
			if 
				NeedHand -> From ! {set_hand, NewHand, Click#inv_click.ox, Click#inv_click.oy}; 
				true -> ok
			end,
			
			% обновляем инвентарь
			O1 = inventory:set_object_inventory(Object, NewInv, InvTag),
			
			% надо уведомить всех кто открыл меня - новый visual state
			% пока уведомим всех. все равно обновится только у прилинкованных игроков
			
			object_changed(State, Object#object.id, Object#object.coord, O1);
		
		% дропнуть вещь на землю
		{drop, NewInv, #item{id=DropObjID} = DropItem} ->
			db_server:inventory_remove(DropObjID),
            S1 = object_drop(State, DropItem, round(PX), round(PY)),
			
			O1 = inventory:set_object_inventory(Object, NewInv, InvTag),

			object_changed(S1, Object#object.id, Object#object.coord, O1);
		
		{context, _ItemObjID, []} ->
			?DEBUG("item: context empty"),
			State;
		
		{context, ItemObjID, List} ->
			?DEBUG("item: context ~p", [List]),
			From ! {item_context, Object#object.id, ItemObjID, List}, 
			State;
		
		SomeResult ->
			?DEBUG("some result ~p",[SomeResult]), State
	end.

% -------------------------------------------------------------------------
% выбрали пункт в контекстном меню вещи в инвентаре
obj_inventory_context(State, FromState, From, Object, InvTag, Action, ItemObjID) ->
	InvList = inventory:get_object_inventory(Object, InvTag),
    % find item in inventory
    case inventory:take_item(InvList, ItemObjID) of
        false ->
			From ! {sys_msg, "no item in inventory"},
			?DEBUG("WARNING player: no such item in inventory"), State;
        {Item, _} -> case inventory:item_context(Item, Action, InvList, Object#object.id, FromState) of
            none ->
                ?DEBUG("action: none"),
                State;
						 
			{state, SNew, NewInv} ->
				From ! {ack, {state, SNew}},
				% обновляем инвентарь
				O1 = inventory:set_object_inventory(Object, NewInv, InvTag),
				
				% надо уведомить всех кто открыл меня - новый visual state
				% пока уведомим всех. все равно обновится только у прилинкованных игроков
		
				object_changed(State, Object#object.id, Object#object.coord, O1);
						 
			{state_func, F, NewInv} ->
				From ! {ack, {state_func, F}},
				% обновляем инвентарь
				O1 = inventory:set_object_inventory(Object, NewInv, InvTag),
				
				% надо уведомить всех кто открыл меня - новый visual state
				% пока уведомим всех. все равно обновится только у прилинкованных игроков
		
				object_changed(State, Object#object.id, Object#object.coord, O1);
			
			Other -> 
				From ! {ack, Other},
				State
        end
    end.









