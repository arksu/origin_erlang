-module(player_server).

-export([start_link/0, start0/0]).
-export([
		 is_account_online/1,
		 update_online/0,
		 spawn_player/1,
		 handle_packet/3,
		 player_start/1,
		 get_visible_zone/2,
		 get_work_grid/1,
		 net_send_loop/1,
         get_tick_count/0,
         get_distance/4,
		 send_grid_msg_ack/2,
		 try_request_move/3,
		 
		 player_update_inventory/2,
		 player_eat/2,
		 player_gain_exp/2,
		 player_add_items/2
		]).

-include("defines.hrl").
-include("base_io.hrl").
-include("net.hrl").
-include("map.hrl").
-include("types.hrl").
%% -include("player_net.hrl"). 


start_link() ->
    P = spawn_link(?MODULE, start0, []), % List of #player_server_rec
    register(player_server, P),
    {ok, P}.

start0() ->
    process_flag(trap_exit, true),
    %io:fwrite("~p started~n" ,[?MODULE]),
	db_server:update_online_count(0),
	?INFO_MSG("player server start"),
    main_loop([]).

%-----------------------------------------------------------------------------------
main_loop(Players_List) ->
    receive
    {spawn, From, Socket} ->
        {_,S,_} = now(),
		PlayerState = #player_state{last_tick = S, socket = Socket},
        P = spawn_link(?MODULE, player_start, [PlayerState]),
	    From ! {spawned, P},
        main_loop([#player_server_rec{pid=P, socket = Socket} | Players_List]);

    {tick, DT} ->
		lists:foreach(fun
						 (#player_server_rec{pid=P}) ->
							  P ! {tick, DT}
					  end, Players_List),
        main_loop(Players_List);

    {ping_tick, DT} ->
		lists:foreach(fun
						 (#player_server_rec{pid=P}) ->
							  P ! {ping_tick, DT}
					  end, Players_List),
        main_loop(Players_List);
		
	% игрок уведомляет что он загружен
	{player_loaded, Pid, AccountID} ->
		case lists:keytake(Pid, #player_server_rec.pid, Players_List) of
			false -> 
				error(player_rec_not_exist);
			{value, #player_server_rec{account_id=AccountID}, _} ->
				error(player_loaded_account_already_exist);
			{value, PRec, T} ->
				main_loop( [PRec#player_server_rec{account_id = AccountID}|T] )
		end;
		
	% есть ли такой аккаунт онлайн?
	{is_account_online, AccountID, From} ->
		case lists:keysearch(AccountID, #player_server_rec.account_id, Players_List) of
			false -> From ! {is_account_online_ack, AccountID, false};
			_ -> From ! {is_account_online_ack, AccountID, true}
		end,
		main_loop(Players_List);
		
	% обновить онлайн 
	{update_online} ->
		% обновить количество игроков в базе
		% по идее уже и не нужно, т.к. онлайн обновляется сразу если крашится процесс игрока
		update_online(Players_List, 0),
		main_loop(Players_List);
	
	% запросить список всех игроков
	{get_players_list, From} ->
		From ! {get_players_list_ack, Players_List},
		main_loop(Players_List);

    {'EXIT', Pid, R} ->
        ?DEBUG("player process exit ~p", [R]),
		NewList = lists:keydelete(Pid, #player_server_rec.pid, Players_List), 
		update_online(NewList, 0),
        main_loop( NewList );

    {_From, stop} ->
        exit(stop_player_server) %% propagate EXIT to all Pids
    end.

%-----------------------------------------------------------------------------------
update_online([], Acc) -> db_server:update_online_count(Acc);
update_online([#player_server_rec{account_id=none}|T], Acc) -> update_online(T, Acc);
update_online([_|T], Acc) -> update_online(T, Acc+1).
update_online() -> ?MODULE ! {update_online}.

%-----------------------------------------------------------------------------------
is_account_online(AccountID) ->
	?MODULE ! {is_account_online, AccountID, self()},
	receive 
		{is_account_online_ack, AccountID, Result} -> Result
	after 1000 ->
			error(timeout_is_account_online)
	end.

%-----------------------------------------------------------------------------------
% CONNECTED: spawn player and return pid
spawn_player(Socket) ->
    player_server ! {spawn, self(), Socket},
    receive
        {spawned, Pid} ->
			?INFO_MSG("spawned player process ~p",[Pid]),
            Pid
    after 5000 ->
			?CRITICAL_MSG("player_server timeout!"),
            error_spawn_player_process
    end.

%-----------------------------------------------------------------------------------
% RECV DATA: process packet and return ok | err.
handle_packet(Pid, Socket, Bin) ->
	{_L, B0} = read_word(Bin),
    {Id, B1} = read_byte(B0),
%%     io:fwrite("game parse packet id=~p~n", [Id]),
    % need check pid
    Pid ! {recv, self(), Socket, Id, B1},
    receive
        {handled, Result} ->
            Result
    after 50000 ->
		?CRITICAL_MSG("player parse packet timeout!"),
        error_parse_packet % no process with this pid or very busy
    end.

%-----------------------------------------------------------------------------------
net_send_loop(Socket) ->
	receive
		{send, Data} ->
			%?DEBUG("player send: ~p", [Data]),
			case gen_tcp:send(Socket, Data) of
				ok -> net_send_loop(Socket);
				{error, Reason} ->
					?DEBUG("player send error: ~p",[Reason]),
					exit({net_send_error, Reason})
			end;
		{kill} -> ok
	end.

%-----------------------------------------------------------------------------------
% start player process, spawn send process
player_start(State) ->
	P = spawn_link(?MODULE, net_send_loop, [State#player_state.socket]),
	%P = none,
	?DEBUG("send pid spawned: ~p",[P]),
	player_loop(State#player_state{send_pid = P}).

%-----------------------------------------------------------------------------------
% work player loop
player_loop(#player_state{inited=Inited, send_pid=SendPid} = State) ->
    NewState =
	case Inited of
		true ->
	receive
		{set_hand, NewHand, OX, OY} ->
			db_server:inventory_update(NewHand, State#player_state.charid),
			player_set_hand(State, NewHand, OX, OY); 
		{item_context, ObjID, ItemObjID, List} ->
			#player_state{send_pid=SendPid} = State,
 			player_net:send_context_menu(SendPid, -1, -1, List),
			State#player_state{ context = {inventory_obj, ObjID, ItemObjID, List} };
			
			
		
		{claim_remove, OwnerID} -> player_net:send_claim_remove(SendPid, OwnerID), State;
		{claim_change, Claim} -> player_net:send_claim_change(SendPid, Claim), State;
		
		% нас привязали к другому объекту
		{linked_to, Object} ->
			State#player_state{linked_to = Object, control=none};
		{stand_up, ObjID, SX, SY} ->
			#player_state{linked_to = LinkedToObject, coord={_,_,Lv}} = State,
			case LinkedToObject of
				none -> State;
				#object{id=ObjID} -> State#player_state{linked_to=none, control=self, coord={SX,SY,Lv}}
			end;
		
		% админ нас телепортирует куда-то
		{admin_teleport_to, CName, X, Y, Lv} ->
			#player_state{name = MyName} = State,
			?DEBUG("admin_teleport_to ~p ~p",[MyName, CName]),
			if 
				(MyName == CName) -> player_teleport(State, X, Y, Lv);
				true -> State
			end;
			
		% визуализировать состояние объекта на клиенте
		{obj_visual_state, Object} ->
			obj_visual_state(State, Object);
		
		% админ спрашивает кто это? надо выслать стейт
		{who, From} ->
			From ! {who_ack, State},
			State;
		
		% грид вернул свой стейт 
		{grid_state, #grid_state{tiles=Tiles, coord={Sg,Grid}=GC}} ->
			#player_state{coord=MyCoord, grids=Grids} = State,
			case lists:keyfind(GC, #player_grid.coord, Grids) of
				false -> State;
				_ ->
					% грид в списке наших рабочих гридов - надо выслать игроку
            		MyGrids = a1_utils:get_grids(MyCoord),
		            % send to client
        		    {X,Y,_} = map_server:get_map_coord(Sg, Grid),
					?DEBUG("send map data"),
                 	player_net:send_map_data(get_send_pid(State), MyGrids, X, Y, Tiles),
            		State
			end;

		% съесть вещь
		{eat, Item} ->
			player_eat(State, Item);

		% взять вещи и вернуть
		{take_items, From, SlotType, Count} ->
			player_take_items_remote(State, From, SlotType, Count);

		% кто то закончил действие надо мной
		{action_complete, From, FromObjID, ActionName} ->
			player_action_complete(State, From, FromObjID, ActionName);

		% добавить баф на игрока
		{buff_add, Buff} ->
			player_buff_add(State, Buff);

		% удалить баф (cancel, debuff)
		{buff_delete, Type} ->
			player_buff_delete(State, Type);

		% надо показать отлетающий текст
		{fly_text, X, Y, Code, Msg} ->
			fly_text(State, X, Y, Code, Msg);

		{sys_msg, Text} ->
			player_net:send_system_msg(SendPid, Text),
			State;

		% получили экспу
		{gain_exp, Exp} ->
			player_gain_exp(State, Exp);
		% получили очки действий в скилл
		{gain_ap, SkillName, AP} ->
			skills:skill_gain_ap(State, SkillName, AP);

        % pickup item
        {pickup, Item} ->
            player_pickup(State, Item);
		% добавить вещи в инвентарь
		{player_add_items, Items} ->
			player_add_items(State, Items);
		
		{dyn_object_pos, _O} -> State;

		% подняли объект. указана максимальная скорость с которой можем двигаться (абсолютная)
		{lift_up, Object} ->
			% TODO : lift up player
			State#player_state{lift_object=Object};

		{lift_down, ObjID} ->
			#object{id=LiftObjID} = State#player_state.lift_object,
			if
				LiftObjID == ObjID ->
					S1 = player_set_cursor(State, arrow, ""),
					S1#player_state{lift_object=none};
				true -> State
			end;

        {area_say, ObjID, Msg} ->
            area_say(State, ObjID, Msg);

        {obj_clear_params, ObjID} ->
            obj_clear_params(State, ObjID);

        {obj_set_param, Object, Param} ->
            obj_set_param(State, Object, Param);

        {obj_changed, Object} ->
            obj_changed(State, Object);

        {obj_changed_type, Object} ->
            obj_changed_type(State, Object);

        {obj_remove_param, ObjID, ParamName} ->
            obj_remove_param(State, ObjID, ParamName);

        {obj_remove, ObjID} ->
            obj_remove(State, ObjID);

        % session send obj, if it new for me - must request info
        % if pos changed - send packet to client
        {obj_pos, Object} ->
%%             ?DEBUG("player: obj_pos objid ~p x ~p y ~p", [ObjID, X, Y]),
            obj_pos(State, Object);

		{none, _} -> State;
        {tick, DT} ->
            player_tick(State, DT);
        {ping_tick, DT} ->
            ping_tick(State, DT);
        {recv, From, Socket, Id, Bin} ->
            %?DEBUG("recv packet id=~p", [Id]),
            case process_packet(Socket, Id, Bin, State) of
                {err, Reason} ->
                    From ! {handled, err},
                    lasterror({process_packet_error, Reason});
                State2 ->
                    From ! {handled, ok},
                    State2
            end;
        Other ->
            ?DEBUG("PLAYER: unhandled msg: ~p", [Other]),
            State
    end;
		false ->
		receive
		{tick, _} ->
			?WARNING_MSG("player tick not inited!"),
			State;
        {ping_tick, _DT} ->
			?WARNING_MSG("ping tick not inited!"),
            State;
		{recv, From, Socket, Id, Bin} ->
            ?DEBUG("recv packet id=~p", [Id]),
            case process_packet(Socket, Id, Bin, State) of
                {err, Reason} ->
                    From ! {handled, err},
                    lasterror({process_packet_error, Reason});
                State2 ->
                    From ! {handled, ok},
                    State2
            end;
        Other ->
            ?DEBUG("PLAYER: unhandled msg: ~p", [Other]),
            State
		end
	end,

    player_loop(NewState).


%-----------------------------------------------------------------------------------
% взять вещи из инвентаря и вернуть
player_take_items_remote(#player_state{inventory = InvList} = State, From, SlotType, Count) ->
	case inventory:take_items(InvList, SlotType, Count, []) of
		false -> 
			case From of none -> ok; _ -> From ! {taked_items, false} end, State;
		{TakedItems, NewInv} ->
			case From of none -> ok; _ -> From ! {taked_items, TakedItems} end,
			% уничтожаем вещи в бд
			inventory:destroy_items(TakedItems),
			player_update_inventory(State, NewInv)
	end.
%-----------------------------------------------------------------------------------
% взять вещь указанного типа в руку
player_take_item_to_hand(#player_state{inventory = InvList, charid=CharID} = State, SlotType) ->
	case inventory:take_items(InvList, SlotType, 1, []) of
		false -> 
			player_set_hand(State, none, 0, 0);
		{[TakedItem], NewInv} -> 
			?DEBUG("taked item ~p",[TakedItem]),
			% ставим взятую вещь в руку
			NewHand = inventory:set_hand(TakedItem),
			% обновим в бд
			db_server:inventory_update(NewHand, CharID),

			player_update_inventory(player_set_hand(State, NewHand, 16, 16), NewInv)
	end.

%-----------------------------------------------------------------------------------
% выполнено действие направленное на меня другим объектом
player_action_complete(#player_state{hp=HP, grids=Grids, charid=CharID} = State, From, _FromObjID, ActionName) ->
	case ActionName of
		{player, war_debuff, _} ->
			?DEBUG("player set me debuff!"),
			%From ! {gain_exp, #exp{combat=5}},
			From ! {gain_ap, bushido, 1},
			% добавим бафф на себя
			B = #buff{duration=buff:get_duration(war_debuff),
					  time=buff:get_duration(war_debuff),
					  target_id=State#player_state.charid,
					  type=war_debuff},
			player_buff_add(State, B);
		{player, punch, EnemyState} ->
			% смотрим есть ли у врага бафф на урон, или дебаф
			#player_state{buffs = Buffs} = EnemyState,
			AttackMod = case lists:keymember(war_buff, #buff.type, Buffs) of
				true ->
					% если есть бафф - урон 15
					-15;
				false ->
					% смотрим есть ли дебаф
					case lists:keymember(war_debuff, #buff.type, Buffs) of
						true ->
							% есть дебаф - урон 5
							-5;
						false ->
							% нет никаких бафов - урон 10
							-10
					end
			end,
			NewHP = if HP+AttackMod < 0 ->
					% меня убили
					0;
				true ->
					% еще не убили. есть хп
					HP+AttackMod
			end,
			From ! {gain_exp, #exp{combat=5}},
			From ! {gain_ap, brawling, 5},
			% меня ударили. снимаем себе хп
			send_all_grids(Grids, {obj_set_param, get_player_object(State), {hp, NewHP}}),
			db_server:player_update_hp(CharID, NewHP),
			if (NewHP == 0) ->
				exit(player_killed_in_battle, punch);
			true ->
				State#player_state{hp=NewHP}
			end;

		_ -> State
	end.

%-----------------------------------------------------------------------------------
fly_text(State, X, Y, Code, Msg) ->
	?DEBUG("PLAYER: fly_text"),
	#player_state{visible_zone=Rect, send_pid=SendPid} = State,

	case get_in_rect({X,Y}, Rect) of
		true -> player_net:send_fly_text(SendPid, X, Y, Code, Msg);
		false -> ok
	end,
	State.

%-----------------------------------------------------------------------------------
% игрок получил экспу
player_gain_exp(State, Exp) ->
	?DEBUG("PLAYER: player_gain_exp"),
	#exp{combat=GainCombat,industry=GainIndustry,nature=GainNature} = Exp,
	#player_state{exp=OldExp, charid=CharID } = State,
	#exp{combat=Combat,industry=Industry,nature=Nature} = OldExp,
	get_work_grid(State) ! {obj_set_param, get_player_object(State), {exp,{GainCombat, GainIndustry, GainNature}}},

	NewExp = #exp{
				  	combat=Combat+GainCombat,
					industry=Industry+GainIndustry,
					nature=Nature+GainNature
				 },
	db_server:player_update_exp(CharID, NewExp),
	S1 = State#player_state{exp=NewExp},
	player_net:send_stat(S1),
	S1.

%-----------------------------------------------------------------------------------
% pickup item from earth
player_pickup(State, Item) ->
    #player_state{ grids = Grids, charid = CharID } = State,
    #item{id=ObjID} = Item,
    % check free space in me
    Where = player_have_free_space(State, Item),
    if Where == false -> State;
    true ->
    case send_grid_msg_ack(Grids, {obj_remove_confirm, self(), ObjID}) of
		none ->
			State;
        {object_remove_confirm_exist_ok, X,Y,Lv} ->
            % get Sg
            Sg = map_server:get_sg(X,Y,Lv),
            % remove tree from db
            db_server:object_remove(Sg, ObjID),
            case Where of
                hand ->
					% добавляем в бд вещь
                    db_server:inventory_add(inventory:set_hand(Item), CharID),
					% ставим вещь в руки игроку
                    player_set_hand(State, Item, 10, 10);
                {inv, PosX, PosY} ->
					player_add_inventory(State, Item, PosX, PosY)
            end
	end
    end.

%-----------------------------------------------------------------------------------
% добавить все вещи из списка
% вещь не должна находится нигде в бд т.к. она добавляется туда
player_add_items(State, []) -> State;
player_add_items(State, Items) ->
	[H|T] = Items,
	player_add_items(player_add_item(State, H), T).

%-----------------------------------------------------------------------------------
% добавить существующую вещь в инвентарь игрока
% вещь не должна находится нигде в бд т.к. она добавляется туда
player_add_item(State, Item) ->
    #player_state{ charid = CharID,
				   grids = Grids,
				   coord = {X,Y,_Lv}
				    } = State,
    % проверяем свободное место
    Where = player_have_free_space(State, Item),
    if Where == false ->
		% если ко мне положить некуда - спавним на землю
		send_all_grids(Grids, {object_drop, Item, round(X), round(Y)}),
		% обязательно надо сбросить действие
		player_reset_action(State);
    true ->
        case Where of
            hand ->
				NewItem = inventory:set_hand(Item),
				db_server:inventory_add(NewItem, CharID),
                player_set_hand(State, Item, 10, 10);
            {inv, PosX, PosY} -> player_add_inventory(State, Item, PosX, PosY)
        end
    end.

%-----------------------------------------------------------------------------------
% add item to player's inventory with pos
player_add_inventory(State, Item, X, Y) ->
    ?DEBUG("player_add_inventory ~p",[Item]),
    #player_state{ inventory = Inventory, charid = CharID } = State,
    NewItem = inventory:set_coord(Item, X, Y),
    NewInv = [NewItem|Inventory],
    db_server:inventory_add(NewItem, CharID),
    player_update_inventory(State, NewInv).

%-----------------------------------------------------------------------------------
% have free space?
% return {inv, PosX, PosY} | hand | false
player_have_free_space(State, Item) ->
    #player_state{ inventory = Inv, equip = Equip, item_hand = Hand} = State,
    {IW,IH} = inventory:get_player_inventory_size(Equip),
    % check inventory
    case inventory:find_free_space(Inv, IW,IH, Item) of
        {ok, PosX, PosY} ->
            ?DEBUG("have free space in inventory"),
            {inv, PosX, PosY};
        false ->
            ?DEBUG("have NOT free space in inventory"),
            % check hand
            case Hand of
                none ->
                    ?DEBUG("take to hand"),
                    hand;
                _ -> false
            end
    end.

%-----------------------------------------------------------------------------------
% handle session msg . player say in area chat
area_say(State, ObjID, Msg) ->
    ?DEBUG("area_say ~p ~p", [ObjID, Msg]),
    #player_state{charid = MyObjID, send_pid=SendPid,
                  visible_list = VisibleList } = State,
    case ObjID of
        MyObjID -> State; % if it me. nothing to do
        _ -> % not me
            case lists:keyfind(ObjID, #visible_obj.id, VisibleList) of
                false -> % nothing do, its not find
                    State;
                _ ->
                    player_net:send_area_say(SendPid, ObjID, Msg),
                    State
            end
    end.

%-----------------------------------------------------------------------------------
player_visible_remove(#player_state{visible_buffer=VisibleBuf,
									visible_client = VisibleClient,
									visible_list = VisibleList,
									send_pid = SendPid
									} = St, ObjID) ->
    % объект надо удалить отовсюду
	NewBuf = lists:keydelete(ObjID, #visible_obj.id, VisibleBuf),
	NewClient = case lists:keytake(ObjID, #visible_obj.id, VisibleClient) of
		false -> VisibleClient;
		{value, _, T} ->
			player_net:send_obj_remove(SendPid, ObjID),
			T
	end,
	NewVis = lists:keydelete(ObjID, #visible_obj.id, VisibleList),
	St#player_state{visible_buffer=NewBuf, visible_client=NewClient, visible_list=NewVis}.


%-----------------------------------------------------------------------------------
% handle obj set param
obj_set_param(State, #object{id=ObjID, params=Params, type=Type}, Param) ->
    #player_state{ visible_client = VisibleClient,
				   visible_buffer = VisibleBuf,
                   linked_objid = LinkedObjID,
				   visible_zone=Rect,
                   send_pid=SendPid } = State,
    {ParamName, ParamData} = Param,

	S1 = if ParamName == line_move ->
		L = if ObjID == LinkedObjID -> none; true -> LinkedObjID end,
		{_,PX,PY,_,_} = ParamData,
		case get_in_rect({PX,PY}, Rect) of
			true ->
				% обновим позицию в буфере
				NewBuf = case lists:keytake(ObjID, #visible_obj.id, VisibleBuf) of
						false -> [#visible_obj{
											   id=ObjID, x=PX, y=PY, typeid=cutils:get_object_typeid(Type), stealth=get_stealth_params(Params)
											  }|VisibleBuf];
						{value, VO1, T} -> [VO1#visible_obj{
															x=PX, y=PY, stealth=get_stealth_params(Params)
														   }|T]
				end,
				State#player_state{visible_buffer=NewBuf, linked_objid=L};
			false ->
				State#player_state{linked_objid = L}
		end;
	true -> State
	end,

	%?DEBUG("obj_set_param ~p ~p", [ObjID, Param]),
    % find obj in visible list objects
    case lists:keytake(ObjID, #visible_obj.id, VisibleClient) of
        % no such obj in client
        false ->
            % nothing do, its not find
            S1;
        % obj finded
        {value, VO, NewList} ->
            % check param
            S2 = case ParamName of
                inventory -> S1;
                gui -> S1;
				exp ->
					?DEBUG("its exp param ~p",[ParamData]),
					{GainCombat, GainIndustry, GainNature} = ParamData,
					player_net:send_gain_exp(SendPid, ObjID, GainCombat, GainIndustry, GainNature),
					S1;

                _ ->
					player_net:send_obj_set_param(SendPid, ObjID, Param), S1
            end,
            % update pos of object in visible objects,
            % if obj not visible any more - just dont anything
            if ParamName == line_move ->						
                   {_,X,Y,_,_} = ParamData,
				   case get_in_rect({X,Y}, Rect) of
						true ->
                   			% and save
                   			S2#player_state{ visible_client = [VO#visible_obj{ x=X, y=Y, stealth=get_stealth_params(Params) }|NewList] };
					   	false ->
							player_visible_remove(S2, ObjID)
				   end;
               true ->
                            % other param. save old coords
%%                             ObjNew = #visible_obj{ id=ObjID, x=OldX, y=OldY},
%%                             % and save in list
%%                             S1#player_state{ visible_buffer = [ObjNew|NewList] }
				   S2
            end
    end.
%-----------------------------------------------------------------------------------
% handle obj remove param
obj_remove_param(State, ObjID, ParamName) ->
    #player_state{ visible_client = VisibleClient,
				   linked_objid = Linked,
                   send_pid=SendPid } = State,
	
	S1 = case ParamName of
		opened ->
			if 
				Linked == ObjID -> player_close_obj(State#player_state{linked_objid=none}, ObjID);
				true -> State
			end;
		_ -> State
	end,
				
    % find obj in visible list objects
    case lists:keyfind(ObjID, #visible_obj.id, VisibleClient) of
        % no such obj in client
        false ->
            % nothing do, its not find
            S1;
        % obj finded
        _ ->
            player_net:send_obj_remove_param(SendPid, ObjID, ParamName),
            S1
    end.

%-----------------------------------------------------------------------------------
% handle obj clear params
obj_clear_params(State, ObjID) ->
    #player_state{ visible_list = VisibleList,
                   send_pid=SendPid } = State,
    % find obj in visible list objects
    case lists:keyfind(ObjID, #visible_obj.id, VisibleList) of
        % no such obj in client
        false ->
            % nothing do, its not find
            State;
        % obj finded
        _ ->
            player_net:send_obj_clear_params(SendPid, ObjID),
            State
    end.

%-----------------------------------------------------------------------------------
% handle obj remove message
obj_remove(State, ObjID) ->
    #player_state{
				   control = Control, state = {MoveState, MoveParam} = MState,
				   linked_objid = LinkedObjID,
				   target_objid = TargetID,
                   lift_object = LiftObject,
				   linked_to = LinkedToObject } = State,
    Lifted = case LiftObject of
                 none -> none;
                 #object{id=LiftObjID} -> LiftObjID
             end,
    % find obj in visible list objects
    St1 = player_visible_remove(State, ObjID),

	LO = if ObjID == LinkedObjID -> none; true -> LinkedObjID end,
    St21 = player_close_obj(if ObjID == LinkedObjID -> player_reset_action(St1); true -> St1 end, ObjID),
	St2 = St21#player_state{ linked_objid = LO },
	St3 = if ObjID == TargetID -> player_set_target(St2, none); true -> St2 end,
    L = if ObjID == Lifted -> none; true -> LiftObject end,

    LT = case LinkedToObject of
			 none -> C = Control, LinkedToObject;
			 _ -> #object{id=LinkedTo} = LinkedToObject,
				if ObjID == LinkedTo ->
		                C = if Control == ObjID -> self; true -> Control end,
		                none;
		            true ->
		                C = Control, LinkedToObject
		         end
		 end,
    MS = case MoveState of
             move_follow ->
                 {_,FollowObjID} = MoveParam,
                 if
					 	FollowObjID == ObjID -> {stay, none};
              			true -> MState
                 end;
             _ -> {MoveState, MoveParam}
         end,
	Target = if (ObjID == State#player_state.target_objid) -> none; true -> State#player_state.target_objid end,
    St3#player_state{ control = C, lift_object = L, linked_to = LT, linked_objid = LO, state = MS, target_objid = Target }.

%-----------------------------------------------------------------------------------
% handle obj pos message
obj_pos(State, #object{id=ObjID, coord={X,Y,Lv}, type=Type, params=Params}) ->
    #player_state{ visible_list = VisibleList,
                   visible_zone = Rect,
				   grids = Grids,
				   linked_objid = LinkedObjID,
				   visible_buffer = VisibleBuf,
				   visible_client = VisibleClient,
				   coord={_,_,MyLv},
                   send_pid=SendPid } = State,
%% 	L = LinkedObjID,
%% 	L = if (ObjID == LinkedObjID) -> none; true -> LinkedObjID end,
%% 	S0 = State#player_state{linked_objid=L},
	%?DEBUG("obj_pos ~p",[O]),
	if (MyLv =/= Lv) -> State; true ->
	% объект в области видимости?
	case get_in_rect({X,Y}, Rect) of
		true ->
			L = LinkedObjID,
%% 			L = if (ObjID == LinkedObjID) -> none; true -> LinkedObjID end,
			
			% обновим позицию в буфере
			NewBuf = case lists:keytake(ObjID, #visible_obj.id, VisibleBuf) of
				false -> [#visible_obj{
						id=ObjID, x=X, y=Y, typeid=cutils:get_object_typeid(Type), stealth=get_stealth_params(Params)
									  }|VisibleBuf];
				{value, VO, T} -> [VO#visible_obj{x=X, y=Y, stealth=get_stealth_params(Params)}|T]
			end,

			% объект реально видим?
			NewClient = case lists:keymember(ObjID, #visible_obj.id, VisibleList) of
				true ->
					%?DEBUG("obj in visible list"),
					% уже есть в клиенте?
					case lists:keytake(ObjID, #visible_obj.id, VisibleClient) of
						{value, VOC, ClientTail} ->
							player_net:send_obj_move(SendPid, ObjID, X, Y),
							[VOC#visible_obj{x=X, y=Y}|ClientTail];
							%VisibleClient;
						false ->
							player_net:send_obj_type(SendPid, ObjID, Type),
							send_all_grids(Grids, {request_obj_info, self(), ObjID}),
							[#visible_obj{
								id=ObjID, x=X, y=Y, typeid=cutils:get_object_typeid(Type), stealth=get_stealth_params(Params)
										 }|VisibleClient]
					end;
				false ->
					VisibleClient
			end,
			State#player_state{visible_buffer=NewBuf, visible_client=NewClient, linked_objid=L};
		false ->
			L = LinkedObjID,
%% 			L = if (ObjID == LinkedObjID) -> none; true -> LinkedObjID end,
			player_visible_remove(State#player_state{linked_objid=L}, ObjID)
	end
	end.


%-----------------------------------------------------------------------------------
% handle obj changed message
obj_changed(State, #object{id=ObjID, coord={X,Y,Lv}, type=Type, params=Params}) ->
	?DEBUG("obj_changed ~p",[ObjID]),
    #player_state{ visible_buffer = VisibleBuf,
                   visible_zone = Rect,
				   visible_list = VisibleList,
				   visible_client = VisibleClient,
                   grids = Grids,
				   coord = {_,_,MyLv},
                   send_pid=SendPid } = State,

	if (MyLv =/= Lv) -> State; true ->

    case get_in_rect({X,Y}, Rect) of
    % obj not in visible zone
    false ->
        player_visible_remove(State, ObjID);
    % obj visible
    true ->
		% обновим позицию в буфере
		NewBuf = case lists:keytake(ObjID, #visible_obj.id, VisibleBuf) of
			false -> [#visible_obj{id=ObjID, x=X, y=Y, typeid=cutils:get_object_typeid(Type), stealth=get_stealth_params(Params)}|VisibleBuf];
			{value, VO, T} -> [VO#visible_obj{x=X, y=Y, stealth=get_stealth_params(Params)}|T]
		end,
		% объект реально видим?
		NewClient = case lists:keymember(ObjID, #visible_obj.id, VisibleList) of
			true ->
				player_net:send_obj_type(SendPid, ObjID, Type),
        		send_all_grids(Grids, {request_obj_info, self(), ObjID}),
        		case lists:keytake(ObjID, #visible_obj.id, VisibleClient) of
		        % no such obj in client
        		false ->
        		    [#visible_obj{id=ObjID, x=X, y=Y} | VisibleClient];
		        {value, #visible_obj{x=OldX,y=OldY} = VO2, NewList} ->
        		    if (OldX =/= X) or (OldY =/= Y) ->
                		player_net:send_obj_move(SendPid, ObjID, X, Y),
						[VO2#visible_obj{x=X, y=Y, stealth=get_stealth_params(Params)} | NewList];
					true ->
						VisibleClient
					end
				end;
			false ->
				VisibleClient
        end,
		State#player_state{visible_buffer=NewBuf, visible_client=NewClient}
    end
	end.
%-----------------------------------------------------------------------------------
% handle obj changed type message
obj_changed_type(State, #object{id=ObjID, coord={X,Y,Lv}, type=Type, params=Params}) ->
    #player_state{ visible_list = VisibleList,
				   visible_buffer = VisibleBuf,
				   visible_client = VisibleClient,
                   visible_zone = Rect,
                   grids = Grids,
                   linked_objid = LinkedObjID,
				   coord = {_,_,MyLv},
                   send_pid=SendPid } = State,

	if (MyLv =/= Lv) -> State; true ->

    NewLinked = if LinkedObjID == ObjID -> none; true -> LinkedObjID end,
    S0 = player_close_obj(if LinkedObjID == ObjID -> player_reset_action(State); true -> State end, ObjID),
	S1 = S0#player_state{ linked_objid = NewLinked },

    case get_in_rect({X,Y}, Rect) of
    % obj not in visible zone
    false ->
        player_visible_remove(S1, ObjID);
    % obj visible
    true ->
		% обновим позицию в буфере
		NewBuf = case lists:keytake(ObjID, #visible_obj.id, VisibleBuf) of
			false -> [#visible_obj{id=ObjID, x=X, y=Y, typeid=cutils:get_object_typeid(Type), stealth=get_stealth_params(Params)}|VisibleBuf];
			{value, VO, T} -> [VO#visible_obj{x=X, y=Y, stealth=get_stealth_params(Params)}|T]
		end,
		% объект реально видим?
		NewClient = case lists:keymember(ObjID, #visible_obj.id, VisibleList) of
			true ->
				player_net:send_obj_type(SendPid, ObjID, Type),
        		send_all_grids(Grids, {request_obj_info, self(), ObjID}),
        		case lists:keytake(ObjID, #visible_obj.id, VisibleClient) of
		        % no such obj in client
        		false ->
        		    [#visible_obj{id=ObjID, x=X, y=Y, stealth=get_stealth_params(Params)} | VisibleClient];
		        {value, #visible_obj{x=OldX,y=OldY} = VO2, NewList} ->
        		    if (OldX =/= X) or (OldY =/= Y) ->
                		player_net:send_obj_move(SendPid, ObjID, X, Y),
						[VO2#visible_obj{x=X, y=Y, stealth=get_stealth_params(Params)} | NewList];
					true ->
						VisibleClient
					end
				end;
			false ->
				VisibleClient
        end,
		S1#player_state{visible_buffer=NewBuf, visible_client=NewClient}
    end
	end.

%-----------------------------------------------------------------------------------
% вышлем на клиент состояние объекта для его визуализации
obj_visual_state(#player_state{send_pid=SendPid, linked_objid=Linked} = State, 
				 #object{id=ObjID} = O) ->
	case Linked of
		ObjID -> player_net:send_object_visual_state(SendPid, O);
		_ -> ok
	end,
	State.

%-----------------------------------------------------------------------------------
% клиент ответил на состояние объекта
obj_visual_state_ack(State, ObjID, ObjAction) ->
	?DEBUG("obj_visual_state_ack ~p", [ObjAction]),
	if 
		ObjID == State#player_state.linked_objid ->
			player_do_action(State, ObjAction, ObjID, 100, 100);
		true ->
			State
	end.
  
%-----------------------------------------------------------------------------------
% обновить глобальный откат скиллов (действий)
player_update_reuse(#player_state{reuse_time={Time, Len},
								  next_action=Next,
								  action = Action,
								  charid = CharID,
								  send_pid=SendPid} = State, DT) ->
	if Time > 0 ->
		NewTime = if (Time-DT) < 0 -> 0; true -> Time-DT end,
		% если откат закончился
		State1 = if NewTime == 0 ->
			case Action of
				none ->
					case Next of
						none -> State;
						_ ->
							% запускаем следующее действие если оно установлено, убираем след действие из стейта
							?DEBUG("run next action ~p",[Next]),
							player_action(State#player_state{next_action=none}, Next)
					end;
				_ ->
					State
			end;
		true ->
			State
		end,

		player_net:send_reuse(SendPid, {NewTime, Len}),
		db_server:player_update_reuse(CharID, NewTime, Len),
		State1#player_state{reuse_time={NewTime, Len}};
	true ->
		State
	end.

%-----------------------------------------------------------------------------------
% обновить восстановление здоровья
player_update_hp_regen(#player_state{hp=HP, hp_regen_timer=HPTimer, charid=CharID, grids=Grids} = State, DT) ->
	if (HP > 100) ->
		?DEBUG("set 100 hp"),
		send_all_grids(Grids, {obj_set_param, get_player_object(State), {hp, 100}}),
		db_server:player_update_hp(CharID, 100),
		State#player_state{hp_regen_timer = 0, hp=100};
	(HP == 100) ->
		State#player_state{hp_regen_timer = 0};
	true ->
		{NewTimer, HPAdd} = regen_value(HPTimer+DT, player:get_hp_regen_speed(State), 0),
		if (HPAdd =/= 0) ->
			NewHP = if (HP+HPAdd) > 100 -> 100; true -> HP+HPAdd end,
			%?DEBUG("hp regen=~p timer=~p",[HPAdd, NewTimer]),
			send_all_grids(Grids, {obj_set_param, get_player_object(State), {hp, NewHP}}),
			db_server:player_update_hp(CharID, NewHP),
			State#player_state{hp=NewHP, hp_regen_timer = NewTimer};
		true ->
			State#player_state{hp_regen_timer = NewTimer}
		end
	end.
%-----------------------------------------------------------------------------------
% обновить восстановление стамины
player_update_stamina_regen(#player_state{
			stamina=Stamina,
			stamina_regen_timer=Timer,
			stamina_consume_timer = ConsumeTimer,
			charid=CharID,
			send_pid=SendPid} = State, DT) ->
	if (Stamina > 100) ->
		?DEBUG("set 100 stamina"),
		player_net:send_player_param(SendPid, stamina, 100),
		db_server:player_update_stamina(CharID, 100),
		State#player_state{stamina_regen_timer = 0, stamina=100};
	true ->
		%?DEBUG("update stamina speed=~p timer=~p consume=~p",[get_current_speed(State), Timer, ConsumeTimer]),
		{NewConsumeTimer, D} = regen_value(ConsumeTimer, 2000, 0),
		{NewTimer, A} = regen_value(Timer+DT, player:get_stamina_regen_speed(State), 0),
		Add = A - D,
		%?DEBUG("update stamina new timer=~p timer=~p",[Timer, NewTimer]),
		if (Add =/= 0) ->
			NewValue = if (Stamina+Add) > 100 -> 100; true -> Stamina+Add end,
			if (NewValue =/= Stamina) ->
				%?DEBUG("stamina regen=~p timer=~p",[Add, NewTimer]),
				player_net:send_player_param(SendPid, stamina, NewValue),
				db_server:player_update_stamina(CharID, NewValue),
				State#player_state{stamina=NewValue, stamina_regen_timer = NewTimer, stamina_consume_timer = NewConsumeTimer};
			true ->
				State#player_state{stamina_regen_timer = NewTimer, stamina_consume_timer = NewConsumeTimer}
			end;
		true ->
			State#player_state{stamina_regen_timer = NewTimer, stamina_consume_timer = NewConsumeTimer}
		end
	end.
%-----------------------------------------------------------------------------------
regen_value(Acc, Speed, Val) when Acc > Speed -> regen_value(Acc-Speed, Speed, Val+1);
regen_value(Acc, Speed, Val) when Acc < -Speed -> regen_value(Acc+Speed, Speed, Val-1);
regen_value(Acc,_,Val) -> {Acc, Val}.

%-----------------------------------------------------------------------------------
% обновить области видимости
player_update_visible(#player_state{visible_timer=Timer} = St, DT) when (Timer+DT < ?VISIBLE_UPDATE_TIME) ->
	St#player_state{visible_timer=Timer+DT};
player_update_visible(#player_state{visible_buffer=Buf,
									visible_list=OldList,
									visible_timer=Timer,
									coord={X,Y,_},
									charid=CharID,
									target_objid = TargetID,
									grids = Grids
								   } = State, _) ->
	%?DEBUG("old list ~p",[OldList]),
	{NewList, Added, Deleted} = visible:process(Buf, OldList, X, Y, player:get_perception(State), CharID, time_server:get_time()),
	%?DEBUG("new list ~p",[NewList]),
	% запросим новые объекты у сессии
	send_all_grids(Grids, {get_objects_in_list, self(), Added}),

	% удалим старые объекты
	S0 = player_visible_client_remove(State, Deleted),
%% 	lists:map(fun(ObjID) ->  send_obj_remove(SendPid, ObjID) end, Deleted),

	S1 = case lists:keymember(TargetID, #visible_obj.id, Deleted) of
			 true -> player_set_target(S0, none);
			 false -> S0
		 end,

	%?DEBUG("visible:process = ~p",[V]),
	S1#player_state{visible_timer=Timer-?VISIBLE_UPDATE_TIME, visible_list = NewList}.

player_visible_client_remove(St, []) -> St;
player_visible_client_remove(St, List) when is_list(List) ->
	[ObjID|T] = List,
	player_visible_client_remove(
	  	player_visible_client_remove(St, ObjID),
		T
						 );
player_visible_client_remove(#player_state{visible_client=VisibleClient,
										   send_pid = SendPid
										   } = St, ObjID) ->
	case lists:keytake(ObjID, #visible_obj.id, VisibleClient) of
		false ->
			St;
		{value, _, T} ->
			player_net:send_obj_remove(SendPid, ObjID),
			St#player_state{visible_client=T}
	end.

%-----------------------------------------------------------------------------------
% return new state
player_tick(#player_state{buffs=Buffs, reuse_time={ReuseTime,_}} = St, DT) ->
	FinalState =
	player_update_visible(
	player_update_stamina_regen(
	player_update_hp_regen(
	player_update_reuse(
	player_update_buffs(
			player_update_move(St),
	 		Buffs, DT, []),
			DT),
			DT),
			DT),
			DT),
	case ReuseTime of 0 -> player_update_action(FinalState, DT);
		_ -> FinalState
	end.

%-----------------------------------------------------------------------------------
% return new state
ping_tick(St, _DT) ->
    #player_state{last_tick = T, coord = CC, grids = Grids} = St,
    % check session lives, if it die - die self =)
    get_work_grid(CC, Grids) ! {is_live, self()},
    receive
                                  {live} -> ok
    after 1000 ->
                                  lasterror({error, grid_die_is_live})
    end,
    {_,N,_} = now(),
    % sec timeout
    case (N-T) > ?PING_TIMEOUT of
                true ->
                    ?DEBUG("player ping timeout!"),
                    lasterror(ping_timeout);
                false ->
%%                     ?DEBUG("player ping"),
                    St
    end.

%-----------------------------------------------------------------------------------
% return {err, Reason} | State
process_packet(_Socket, Id, Bin, St) ->
    case Id of
        ?GAMESERVER_COOKIE ->
            ?DEBUG("recv GAMESERVER_COOKIE"),
            #player_state{ state = PlayerState } = St,
            case PlayerState of
                {none, _} ->
                    case db_server:game_check_cookie(Bin) of
                        err ->
                            {err, bad_cookie};
                        CharID ->
                            ?DEBUG("cookie correct, spawn char id=~p~n", [CharID] ),
                            % need process login char into world
                            player_login(CharID, St)
                    end;
                _ ->
                    {err, cookie_wrong_state}
            end;
        ?GAMESERVER_MAP_CLICK ->
            {OBJID, B1} = read_int(Bin),
            {X, B2} = read_int(B1),
            {Y, B3} = read_int(B2),
            {MX, B4} = read_int(B3),
            {MY, B5} = read_int(B4),
            {Btn, B6} = read_byte(B5),
            {M, _} = read_byte(B6),
            player_map_click(St, OBJID,X,Y,MX,MY,Btn, M);
        ?GAMESERVER_CONTEXT_ACTION ->
            {Action, _} = read_string(Bin),
            player_context_action(St, Action);
        ?GAMESERVER_CLIENT_SAY ->
            {Channel, B1} = read_int(Bin),
            {Nick, B2} = read_string(B1),
            {Msg, _} = read_string(B2),
            player_say(St, Channel, Nick, Msg);
        ?GAMESERVER_ACTION ->
            {Action, _} = read_string(Bin),
			?DEBUG("recv pkt action ~p",[Action]),
            player_action(St, Action);

        ?GAMESERVER_CRAFT_CLICK ->
            {CraftName, B1} = read_string(Bin),
            {Click, _} = read_string(B1),
            player_craft_click(St, list_to_atom(CraftName), list_to_atom(Click));
        ?GAMESERVER_TARGET_RESET ->
            player_set_target(St, none);
		?GAMESERVER_SET_SPEED ->
			{Speed, _} = read_int(Bin),
			player_set_speed(St, Speed);
		?GAMESERVER_BUG_REPORT ->
            {Subj, B1} = read_string(Bin),
            {Text, _} = read_string(B1),
			player_bug_report(St, Subj, Text);
        ?GAMESERVER_PING ->
%%             ?DEBUG("recv GAMESERVER_PING"),
            send_packet(St#player_state.send_pid, ?GAMESERVER_PONG, [write_int(time_server:get_time())]),
            {_,N,_} = now(),
            St#player_state{last_tick = N};
		?GAMESERVER_DIALOG_OPEN ->
			{Dlg, _B1} = read_string(Bin),
			#player_state{dialogs = D} = St,
			?DEBUG("dialog open ~p ~p",[D, Dlg]),
			dialog_opened(St, Dlg),
			St#player_state{dialogs = case lists:member(Dlg, D) of
										  true -> D;
										  false -> [Dlg|D]
									  end };
		?GAMESERVER_DIALOG_CLOSE ->
			{Dlg, _B1} = read_string(Bin),
			#player_state{dialogs = D} = St,
			?DEBUG("dialog close ~p ~p",[D, Dlg]),
			St#player_state{dialogs = lists:delete(Dlg, D)};	
		?GAMESERVER_KNOWLEDGE_INC ->
			{Knw, _B1} = read_string(Bin),
			skills:knowledge_buy(St, inc, Knw);
		?GAMESERVER_KNOWLEDGE_DEC ->
			{Knw, _B1} = read_string(Bin),
			skills:knowledge_buy(St, dec, Knw);
		?GAMESERVER_SKILL_BUY ->
			{SkillName, B1} = read_string(Bin),
			{KnwName, _B2} = read_string(B1),
			skills:skill_buy(St, KnwName, SkillName);
		?GAMESERVER_OBJECT_VISUAL_STATE_ACK ->
			{ObjID, B1} = read_int(Bin),
			{Term, _B2} = read_term(B1),
			obj_visual_state_ack(St, ObjID, Term);
		?GAMESERVER_INVENTORY_CLICK ->
			{ObjID, B1} = read_int(Bin),
			{InvObjID, B11} = read_int(B1),
			{Btn, B2} = read_byte(B11),
			{Mod, B3} = read_byte(B2),
			{IX, B4} = read_byte(B3),
			{IY, B5} = read_byte(B4),
			{OX, B6} = read_word(B5),
			{OY, _} = read_word(B6),
			player_inventory_click(St, ObjID, InvObjID, Btn, Mod, IX, IY, OX, OY);
		?GAMESERVER_EQUIP_CLICK ->
			{ObjID, B1} = read_int(Bin),
			{EquipID, B11} = read_int(B1),
			{Btn, B2} = read_byte(B11),
			{Mod, B3} = read_byte(B2),
			{OX, B6} = read_word(B3),
			{OY, _} = read_word(B6),
			player_equip_click(St, ObjID, EquipID, Btn, Mod, OX, OY);
			
        _ ->
            ?DEBUG("WARNING player: unhandled packet id ~p", [Id]),
			St
    end.

%-----------------------------------------------------------------------------------
player_equip_click(St, _ObjID, EquipID, Btn, _Mod, OX, OY) ->
	case equip:handle_click(St, equip:get_slot(EquipID), Btn) of
		{H1, NewEquip} ->
			NewHand = inventory:set_hand(H1),
			db_server:inventory_update(NewHand, St#player_state.charid),

			S1 = player_set_hand(St#player_state{equip=NewEquip}, NewHand, OX, OY),

			player_net:send_equip(St#player_state.send_pid, NewEquip),
			get_work_grid(S1) ! {obj_set_param, get_player_object(S1), player:get_equip_param(S1)},
			S1;
		none -> St
	end.

%-----------------------------------------------------------------------------------
player_inventory_click(St, ObjID, InvObjID, Btn, Mod, X, Y, OX, OY) ->
	?DEBUG("player_inventory_click ~p ~p ~p ~p ~p ~p",[ObjID, InvObjID, Btn, Mod, X, Y]),
	#player_state{charid=CharID, equip=Equip, 
				  item_hand=Hand, inventory=Inventory, send_pid = SendPid,
				  coord={PX,PY,_}, grids=Grids} = St,
	
	case inventory:item_click2({player,Equip}, Inventory, Hand, InvObjID, ObjID, X, Y, Btn, Mod) of
		{NeedInv, NewInv, NeedHand, NewHand} ->
			?DEBUG("item: ack: ~p ~p ~p ~p", [NeedInv, NewInv, NeedHand, NewHand]),
			
			S1 = 			
			% меняем руку
			if 
				NeedHand ->
					db_server:inventory_update(NewHand, CharID),
					player_set_hand(St, NewHand, OX, OY); 
				true -> St
			end,
			% обновляем инвентарь
			player_update_inventory(S1, NewInv);
		
		{drop, NewInv, #item{id=DropObjID} = DropItem} ->
			db_server:inventory_remove(DropObjID),
            send_all_grids(Grids, {object_drop, DropItem, round(PX), round(PY)}),
			player_update_inventory(St, NewInv);
		
		{context, _ItemObjID, []} ->
			?DEBUG("item: context empty"),
			St#player_state{ context = none};
		
		{context, ItemObjID, List} ->
			?DEBUG("item: context ~p", [List]),
 			player_net:send_context_menu(SendPid, -1, -1, List),
			St#player_state{ context = {inventory,ItemObjID,List} };
	
		SomeResult ->
			?DEBUG("some result ~p",[SomeResult]), St
	end.

%-----------------------------------------------------------------------------------
% клик на кнопке крафта в клиенте Click = all | one
player_craft_click(St, CraftName, Click) ->
    ?DEBUG("player_craft_click ~p ~p",[CraftName, Click]),
	#player_state{
                  send_pid=SendPid} = St,
    {_,_ReqItems,_ReqObj, _ReqEquip, CraftTicks} = craft:get_craft(CraftName),
    case craft:check_craft(CraftName, St) of
        true ->
            St#player_state{action = {craft, CraftName, Click, CraftTicks, CraftTicks, 0}};
        {false, A1, A2, A3} ->
            if A1 == false ->
               player_net:send_system_msg(SendPid, "need linked object"); true -> ok end,
            if A2 == false ->
               player_net:send_system_msg(SendPid, "havnt requied equip"); true -> ok end,
            if A3 == false ->
               player_net:send_system_msg(SendPid, "havnt required items in inventory"); true -> ok end,
            St
    end.

%-----------------------------------------------------------------------------------
% handle player select action from list
player_action(St, Action) ->
        #player_state{skills = Skills, cursor = Cursor, context = Context,
					  access_level = AccessLevel,
					  target_objid = TargetID,
                      charid = CharID, item_hand = Hand, send_pid=SendPid } = St,
    ?DEBUG("player_action ~p",[Action]),
    % hotkeys
    case Action of
        _ ->
    % only if dont take anything
    if (Hand == none) and (Cursor == arrow) and (Context == none) ->
        % find received action
        ActionsList = player:get_actions(Skills, AccessLevel),
        case lists:keyfind(Action, 2, ActionsList) of
            % warning! no in actions list!
            false -> % !!!!! report to ADMIN =)
                St;
            % finded in actions list
            _ -> case Action of
                "lift" -> player_set_cursor(St, lift, "lift");
                "spawn_box" -> player_set_place_obj(St, {box, 0,spawn});
				"spawn_corner_home_wood" -> 		player_set_place_obj(St, {corner_home_wood, 0,spawn});
				"spawn_wally_home_wood" -> 			player_set_place_obj(St, {wally_home_wood, 0,spawn});
				"spawn_wallwindowy_home_wood" -> 	player_set_place_obj(St, {wallwindowy_home_wood, 0,spawn});
				"spawn_walldoory_home_wood" -> 		player_set_place_obj(St, {walldoory_home_wood, 0,spawn});

				"spawn_plant_wild" -> player_set_place_obj(St, {plant_wild, 0,spawn});
				"spawn_fir" -> player_set_place_obj(St, {fir_tree, 0,spawn});
				"spawn_oak" -> player_set_place_obj(St, {oak_tree, 0,spawn});
				"spawn_pine" -> player_set_place_obj(St, {pine_tree, 0,spawn});
				"spawn_pear" -> player_set_place_obj(St, {pear_tree, 0,spawn});
				"spawn_apple" -> player_set_place_obj(St, {apple_tree, 0,spawn});
				"spawn_stone" -> player_set_place_obj(St, {stone, 0,spawn});
				"spawn_runestone" -> player_set_place_obj(St, {runestone, 0,spawn});
				"spawn_rabbit" -> player_set_place_obj(St, {rabbit, 0,spawn});
					 
				"attack_buff" -> player_recv_action(St, {player, war_buff, St}, buff:get_cast_time(war_buff), CharID);
                "attack_debuff" ->
					case TargetID of
						none -> St;
						_ -> player_recv_action(St,
												{player, war_debuff, St},
												buff:get_cast_time(war_debuff),
												TargetID)
					end;
				"attack_punch" ->
					case TargetID of
						none -> St;
						_ -> player_recv_action(St, {player, punch, St}, 200, TargetID)
					end;

				"attack_godbuff" -> player_recv_action(St, {player, god_buff, St}, buff:get_cast_time(god_buff), CharID);
 
				"plow" -> player_set_cursor(St, plow, "dig");
				"destroy" -> player_set_cursor(St, destroy, "destroy");
				"repair" -> player_set_cursor(St, repair, "repair");
				"dig" -> player_set_cursor(St, dig, "dig");
				"dig_hole" -> player_set_cursor(St, dig_hole, "dig");
				"lay_stone" -> player_set_cursor(St, lay_stone, "dig");
				"lay_grass" -> player_set_cursor(St, lay_grass, "dig");
				"harvest" -> player_set_cursor(St, harvest, "harvest");
				"make_runestone" -> player_set_cursor(St, make_runestone, "dig");
				"runestone_label" -> player_set_cursor(St, runestone_label, "dig");	 

				"build_corner_home_wood" -> player_set_place_obj(St, {corner_home_wood, none, build}); % PlaceType, ItemObjId, Action
				"build_wally_home_wood" -> player_set_place_obj(St, {wally_home_wood, none, build}); % PlaceType, ItemObjId, Action
				"build_wallx_home_wood" -> player_set_place_obj(St, {wallx_home_wood, none, build}); % PlaceType, ItemObjId, Action
				"build_corner_fence" -> player_set_place_obj(St, {corner_fence, none, build}); % PlaceType, ItemObjId, Action
				"build_chair" -> player_set_place_obj(St, {chair, none, build});
				"build_claim" -> player_set_place_obj(St, {claim, none, build});
				"build_bonfire" -> player_set_place_obj(St, {bonfire, none, build});	 
				"build_jar" -> player_set_place_obj(St, {jar, none, build});
					 
                "remove" -> 		player_set_cursor(St, remove, "attack");
				"tile_water_deep" ->	player_set_cursor(St, tile_water_deep, "dig");
				"tile_water_low" ->	player_set_cursor(St, tile_water_low, "dig");
				"tile_sett" ->		player_set_cursor(St, tile_sett, "dig");
				"tile_plowed" ->	player_set_cursor(St, tile_plowed, "dig");
				"tile_forest_leaf" ->	player_set_cursor(St, tile_forest_leaf, "dig");
				"tile_forest_fir" ->	player_set_cursor(St, tile_forest_fir, "dig");
				"tile_grass" ->		player_set_cursor(St, tile_grass, "dig");
				"tile_swamp" ->		player_set_cursor(St, tile_swamp, "dig");
				"tile_dirt" ->		player_set_cursor(St, tile_dirt, "dig");
				"tile_sand" ->		player_set_cursor(St, tile_sand, "dig");
				"tile_house" ->		player_set_cursor(St, tile_house, "dig");
				"tile_cellar" ->	player_set_cursor(St, tile_cellar, "dig");
				"tile_cave" ->		player_set_cursor(St, tile_cave, "dig");
				"tile_hole" ->		player_set_cursor(St, tile_hole, "dig");

				"craft_basket" ->
                    % скрафтить корзинку из веток
                    % клиенту надо выслать список крафта. требуемых вещей и список того что получится
                    % он сам отобразит гуи контрол где надо
                    craft:send_craft(SendPid, basket),
                    St;
                "craft_stone_axe" -> 		craft:send_craft(SendPid, stone_axe), St;
				"craft_brick_clay_raw" -> 	craft:send_craft(SendPid, brick_clay_raw), St;	
				"craft_wood_shoes" -> 		craft:send_craft(SendPid, wood_shoes), St;
				"craft_pants_skin" -> 		craft:send_craft(SendPid, pants_skin), St;
					 
                "spawn_inv_fabric_bag" -> 	player_spawn_new_item(St, fabric_bag, 10);
                "spawn_inv_billet" -> 		player_spawn_new_item(St, billet, 10);
                "spawn_inv_drawing_box" -> 	player_spawn_new_item(St, drawing_box, 10);
				"spawn_inv_axe" -> 			player_spawn_new_item(St, stone_axe, 10);
				"spawn_inv_carrot" -> 		player_spawn_new_item(St, carrot, 10);
				"spawn_inv_seed_carrot" -> 	player_spawn_new_item(St, seed_carrot, 10);
                _ -> St
            end
        end;
    true -> St
    end
    end.

%-----------------------------------------------------------------------------------
% заспавнить новую вещь в инвентарь игрока
player_spawn_new_item(State, Type, Q) ->
	Item = #item{type=Type, id=none, q=Q, amount=1},
    case player_have_free_space(State, Item) of
	    false -> State;
		_ -> player_add_item(State, Item#item{id=world_server:get_next_id()} )
	end.

%-----------------------------------------------------------------------------------
% отменить текущее действие
player_reset_action(State) ->
	player_set_progress(State#player_state{action = none}, none).

%-----------------------------------------------------------------------------------
% установить откат
player_set_reuse(State, 0) -> State;
player_set_reuse(State, TimeLen) ->
	player_net:send_reuse(State#player_state.send_pid, {TimeLen, TimeLen}),
	State#player_state{reuse_time={TimeLen, TimeLen}}.

%-----------------------------------------------------------------------------------
% установить цель на объект
player_set_target(#player_state{charid=CharID, grids=Grids} = State, ObjID) ->
	case ObjID of
		none ->
			player_net:send_target(State#player_state.send_pid, none),
			State#player_state{target_objid = ObjID};
		CharID ->
			player_net:send_system_msg(State#player_state.send_pid, "cant target myself"),
			State;
		_ ->
			case send_grid_msg_ack(Grids, {find_object, self(), ObjID}) of
                {object_found, #object{type=player} = O} ->
					player_net:send_target(State#player_state.send_pid, O),
                    State#player_state{target_objid = ObjID};
				{object_found, _} ->
					player_net:send_system_msg(State#player_state.send_pid, "cant target this object"),
					State;
                none ->
					player_net:send_target(State#player_state.send_pid, none),
					State#player_state{target_objid = none}
            end
	end.

%-----------------------------------------------------------------------------------
% установить скорость передвижения
player_set_speed(State, Speed) ->
	player_net:send_current_speed(State#player_state.send_pid, Speed),
	State#player_state{speed = Speed}.

%-----------------------------------------------------------------------------------
% баг репорт игрока 
player_bug_report(St, Subj, Text) ->
	db_server:bug_report_add(St#player_state.accountid, Subj, Text),
	St.


%-----------------------------------------------------------------------------------
% set placing object and send it to client
player_set_place_obj(St, NewPlace) ->
	S1 = player_set_cursor(St, arrow, ""),
    #player_state{ send_pid=SendPid, place_obj = Place } = S1,
    if Place == NewPlace -> S1;
    true ->
        case NewPlace of
            none -> player_net:send_place_object(SendPid, "none");
            _ ->
                {Type, _,_} = NewPlace,
                player_net:send_place_object(SendPid, atom_to_list(Type))
        end,
        S1#player_state{ place_obj = NewPlace }
    end.

%-----------------------------------------------------------------------------------
% set hand to player state, and send to client if need
player_set_hand(St, Item, OX, OY) ->
    #player_state{ send_pid=SendPid, 
                   item_hand = Hand } = St,
    case Hand of
        none -> case Item of
            none ->
				% в руке ничего нет. и ставим тоже ничего
                St#player_state{ item_hand = none };
            #item{id=ObjID, type=Type} ->
				% ставим в руку что-то
                {ImageName, _} = inventory:item_get_icon_hint(Item),
				{IW, IH} = inventory:item_inventory_size(Type),
				player_net:send_set_hand(SendPid, ObjID, IW, IH, OX, OY, ImageName),

				player_set_cursor(
				  	St#player_state{ item_hand = Item },
					arrow, "")
            end;
        _ -> case Item of
            none ->
                % destroy hand control 
				player_net:send_set_hand(SendPid, none),
                St#player_state{ item_hand = none };
            #item{id=ObjID, type=Type} ->
				% ставим в руку что-то
                {ImageName, _} = inventory:item_get_icon_hint(Item),
				{IW, IH} = inventory:item_inventory_size(Type),
				player_net:send_set_hand(SendPid, ObjID, IW, IH, OX, OY, ImageName),
				player_set_cursor(
				  	St#player_state{ item_hand = Item },
					arrow, "")
            end
    end.


%-----------------------------------------------------------------------------------
% handle player say in chat
player_say(#player_state{access_level=AccessLevel,
							  send_pid=SendPid,
							  grids=Grids,
							  charid=CharID,
							  coord=CC,
							  name=Name} = St, Channel, Nick, Msg) ->
	L = string:left(Msg, 1),
	db_server:chat_log(Msg, Nick, Channel, CharID),
	case string:equal(L, "/") of
		true ->
			Command = string:substr(Msg, 2),
			?WARNING_MSG("its admin command! ~p",[Command]),
			case player:is_command_allow(list_to_atom(Command), AccessLevel) of
				true ->
					execute_command(St, Command);
				false ->
					?WARNING_MSG("havnt access to run console commands char=~p name=~p",[CharID, Name]),
					player_net:send_system_msg(SendPid, "havnt access to run console commands"),
					St
			end;
		false ->
		    case Channel of
        		0 -> % area
		            get_work_grid(CC, Grids) ! {object_say, {area, CharID, Msg}};
        		1 -> % private
		            get_work_grid(CC, Grids) ! {object_say, {private, CharID, Nick, Msg}};
        		2 -> % village
		            get_work_grid(CC, Grids) ! {object_say, {village, CharID, Msg}};
				OC ->
					?WARNING_MSG("undef say channel: ~p",[OC])
		    end,
		    St;
		SE -> 
			?WARNING_MSG("undef case ~p",[SE]),
			St
	end.

%-----------------------------------------------------------------------------------
% handle player click on map
player_map_click(St, ObjID,X,Y, MX,MY, Btn, Mod) ->
    ?DEBUG("player_map_click x=~p y=~p btn=~p", [X,Y,Btn]),
    #player_state{state = CurState, coord = MyCoord,
                  control = Control, lift_object = LiftObject, item_hand = Hand,
				  inventory = Inventory, send_pid = SendPid,
				  grids = Grids,
				  linked_to = LinkedTo,
                  cursor = Cursor, place_obj = Place } = St,
    Time = get_tick_count(),
    {MyX, MyY, MyLv} = MyCoord,
    {MoveState, _} = CurState,
    case Btn of
        ?MB_LEFT -> % --------------------------------------------------------------------------------------------------------------------
            % cancel any progress
            SInit = player_set_progress(St, none),
			case Control of
			none ->	
					if LinkedTo =/= none -> 
						   #object{id=LinkedToObjID, type=LinkedToType} = LinkedTo,
						   case LinkedToType of
							   chair -> player_do_action(SInit, {stand_up, X, Y}, LinkedToObjID, MX, MY);
							   _ ->
								   SInit
						   end;
					   true -> SInit
					end;
			self ->
            ?DEBUG("cursor: ~p; hand: ~p", [Cursor, Hand]),
            % check hand
            case Hand of none ->
                % check cursor
                case Cursor of
                % normal click
                arrow ->
                    % if need to place - do it
                    if Place =/= none -> player_place_object(SInit, Place, Mod, X,Y);
                    true ->

					%------------------------------------------------
					% если кликнули по объекту с сажатым контролом
%% 					if ObjID =/= 0 andalso Mod == ?MOD_CONTROL ->

					% выбираем цель просто по клику. без контрола
%% 					if ObjID =/= 0 ->
%% 						% выбираем цель
%% 						S1 = player_set_cursor(SInit, arrow, ""),
%% 						% щелкнули по объекту. выберем как цель
%% 						player_set_target(S1, ObjID);
%% 					true ->
					%------------------------------------------------
                    % clicked to move, check what we control
                    case Control of
                    none ->
                        SInit;
                    {object, ControlObjID} ->
                        get_grid(St, X, Y) ! {player_map_click, ControlObjID, ObjID,X,Y, MX,MY, Btn, Mod};
                    self ->
                        ?DEBUG("check_moving"),
                        % if was in move - need update pos
                        St1 = case is_moving(MoveState) of
                                true -> player_update_move(SInit);
                                false -> SInit
                        end,
                        % move to point
                        % reset any action, update moving
                        player_update_move(St1#player_state{ action = none, state = {move_point, {Time-50, X,Y} } })
                    end % case Control
                    end; % if Place
                lift ->
                    if LiftObject == none ->
                    % if click to object - lift it
                    St2 = if 
							  ObjID > 0 -> 
								  player_do_action(SInit, lift, ObjID, MX,MY); 
							  true -> 
								  SInit 
						  end,
                    % reset to default anyway
                    player_set_cursor(St2, arrow, "");
                    true -> SInit
                    end;
				select_target ->
					% выбираем цель
					S1 = player_set_cursor(SInit, arrow, ""),
					case ObjID of
						0 -> S1;
						_ ->
							% щелкнули по объекту. выберем как цель
							player_set_target(S1, ObjID)
					end;
				tile_water_deep ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=water_deep }}), SInit;
				tile_water_low ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=water_low }}), SInit;
				tile_sett ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=sett }}), SInit;
				tile_plowed ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=plowed }}), SInit;
				tile_forest_leaf ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=forest_leaf }}), SInit;
				tile_forest_fir ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=forest_fir }}), SInit;
				tile_grass ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=grass }}), SInit;
				tile_swamp ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=swamp }}), SInit;
				tile_dirt ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=dirt }}), SInit;
				tile_sand ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=sand }}), SInit;
				tile_house ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=house }}), SInit;
				tile_cellar ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=cellar }}), SInit;
				tile_cave ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=cave }}), SInit;
				tile_hole ->
					{{_,_},{_,_},{_,_}, Sg, Grid, Index} = map_server:get_grid_coord(X,Y,MyLv),
					send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=hole }}), SInit;

				% все остальные действия только если ничего не тащим над собой
				_ -> if LiftObject == none -> case Cursor of
							
				dig_hole ->
					S1 = SInit, %player_set_cursor(SInit, arrow, ""),
					ActionTime = player:get_action_time(dig),
					{TX, TY} = map_server:tilify_center(X, Y),
					A = {dig_hole, ActionTime,ActionTime, 0, TX, TY},
					if (MyX == X) and (MyY == Y) ->
                        SInit#player_state{ action = A, context = none };
                	true ->
                        St1 = case is_moving(MoveState) of
                            true -> player_update_move(S1);
                            false -> S1
                        end,
						player_update_move(St1#player_state{
                                                action = A,
                                                context = none,
                                                state =
                                                    {move_point,
                                                      {Time, TX,TY}
                                                    }
                                               })
					end; % end if
				dig ->
					S1 = SInit, %player_set_cursor(SInit, arrow, ""),
					ActionTime = player:get_action_time(dig),
					{TX, TY} = map_server:tilify_center(X, Y),
					A = {dig, ActionTime,ActionTime, 0, TX, TY},
					if (MyX == X) and (MyY == Y) ->
                        SInit#player_state{ action = A, context = none };
                	true ->
                        St1 = case is_moving(MoveState) of
                            true -> player_update_move(S1);
                            false -> S1
                        end,
						player_update_move(St1#player_state{
                                                action = A,
                                                context = none,
                                                state =
                                                    {move_point,
                                                      {Time, TX,TY}
                                                    }
                                               })
					end; % end if
				lay_stone ->
					S1 = SInit, %player_set_cursor(SInit, arrow, ""),
					ActionTime = 3000,
					{TX, TY} = map_server:tilify_center(X, Y),
					A = {lay_stone, ActionTime,ActionTime, 0, TX, TY},
					if (MyX == X) and (MyY == Y) ->
                        SInit#player_state{ action = A, context = none };
                	true ->
                        St1 = case is_moving(MoveState) of
                            true -> player_update_move(S1);
                            false -> S1
                        end,
						player_update_move(St1#player_state{
                                                action = A,
                                                context = none,
                                                state =
                                                    {move_point,
                                                      {Time, TX,TY}
                                                    }
                                               })
					end; % end if

				lay_grass ->
					
					S1 = SInit, %player_set_cursor(SInit, arrow, ""),
					{MyX,MyY,_MyLv} = MyCoord,
					case grid_get_tile(Grids, X, Y) of
						none ->
							S1;
						{get_tile_ack, #tile{type=TileType}} ->
							case map_server:can_lay_grass(TileType) of
								% не можем засеять тайл
								false -> player_net:send_system_msg(SendPid, "cant lay grass this tile"), S1;
								true ->
							case inventory:take_items(Inventory, slot_seed, 1, []) of
								false -> player_net:send_system_msg(SendPid, "havnt seed in inventory"), S1;
								_ ->
									ActionTime = 3000,
									{TX, TY} = map_server:tilify_center(X, Y),
									A = {lay_grass, ActionTime,ActionTime, 0, TX, TY},
									if (MyX == X) and (MyY == Y) ->
										% ADMIN!!! не может игрок обычными средствами сразу копать под собой.
	                                    SInit#player_state{ action = A, context = none };
	                            	true ->
	                                    St1 = case is_moving(MoveState) of
	                                        true -> player_update_move(S1);
	                                        false -> S1
	                                    end,
										player_update_move(St1#player_state{
	                                                            action = A,
	                                                            context = none,
	                                                            state =
	                                                                {move_point,
	                                                                  {Time, TX,TY}
	                                                                }
	                                                           })
									end
							end end
					end;
				harvest ->
					case ObjID of 0 -> SInit; _ -> player_do_action(SInit, Cursor, ObjID, MX, MY) end;
				make_runestone ->
					case ObjID of 0 -> SInit; _ -> player_do_action(SInit, Cursor, ObjID, MX, MY) end;
				runestone_label ->
					case ObjID of 0 -> SInit; _ -> player_do_action(SInit, Cursor, ObjID, MX, MY) end;
				destroy ->
					case ObjID of 0 -> SInit; _ -> player_do_action(SInit, Cursor, ObjID, MX, MY) end;
				repair ->
					case ObjID of 0 -> SInit; _ -> player_do_action(SInit, Cursor, ObjID, MX, MY) end;

                remove ->
					case ObjID of
						0 -> ok;
						_ ->
							% удалить еще и его инвентарь
							db_server:inventory_remove_owner(ObjID),
							get_grid(St, X, Y) ! {obj_remove, ObjID}
					end,
                    player_set_cursor(SInit, arrow, "");
				plow ->
					%S1 = player_set_cursor(SInit, arrow, ""),
					{MyX,MyY,_MyLv} = MyCoord,
					PlowTime = 2000,
					{AX, AY} = map_server:tilify_center(X, Y),
					A = {plow, PlowTime,PlowTime, 0, AX, AY},
					St1 = case is_moving(MoveState) of
                                        true -> player_update_move(SInit);
                                        false -> SInit
                                    end,
									player_update_move(St1#player_state{
                                                            action = A,
                                                            context = none,
                                                            state =
                                                                {move_point,
                                                                  {Time, AX,AY}
                                                                }
                                                           });

            	% cursor
				
                _ ->
                    % reset to default anyway
                    player_set_cursor(SInit, arrow, "")
                end;
					true -> player_set_cursor(SInit, arrow, "")
				end
				end;
			% -----------------------------------------------------
            _ -> % something in hand - need to drop it
                player_drop(St)%#player_state{action=none})
			end % case hand
            end; % case control
        ?MB_RIGHT -> % --------------------------------------------------------------------------------------------------------------------
            % cancel any progress, cursor, place
            SInit = player_set_progress(
                        player_set_cursor(
                            player_set_place_obj(St, none),
                        arrow, ""),
                    none),
			case Control of
			self ->
            % check cursor
            case Cursor of
            arrow ->
				case Hand of
				none ->
					case LiftObject of
					none ->
						case ObjID of % click on ground
						0 ->
								% click on map
								SInit;
						_ -> % on object
								?DEBUG("RMB on object id=~p",[ObjID]),
								player_do_action(SInit, right_click, ObjID, MX, MY)
						end;
					#object{id=LiftObjID, type=LiftType} -> % some lifting. need put it down.
								?DEBUG("lifting down ~p",[LiftObjID]),
								{MyX,MyY,_MyLv} = MyCoord,
								{TX, TY} = if 
											   Mod == ?MOD_SHIFT -> map_server:tilify_center(X, Y);
											   true -> {X,Y}
										   end,
								A = {lift_down, 0,0,0, LiftObjID, TX, TY},
								if (MyX == TX) and (MyY == TY) ->
										% ADMIN!!! не может игрок обычными средствами сразу положить под собой.
										SInit#player_state{ action = A, context = none };
								true ->
										St1 = case is_moving(MoveState) of
											true -> player_update_move(SInit);
											false -> SInit
										end,
										?DEBUG("lift type=~p down obj, it have collision=~p",[LiftType,cutils:get_object_collision(LiftType, player)]),

										% надо проверить коллизию перед началом движения
										case case is_rect_intersect(
													rect_from_bound(MyX,MyY, cutils:get_object_bounds(player)),
													rect_from_bound(TX,TY,cutils:get_object_bounds(LiftType))) of
												true -> cant;
												false -> can_move
										end of
											cant -> SInit;
											can_move ->
												player_update_move(St1#player_state{
																action = A,
																context = none,
																state =
																	{move_virtual_object,
																	  {Time, TX,TY, LiftType}
																	}
															   })

										end
								end
					end; % end lift
				% case Hand -------------------------------------------
				% чтото держим в руке. надо применить объект
				_ ->
					player_itemact(SInit, ObjID,X,Y, MX,MY, Btn, Mod)
				end; % end hand
			% cursor
            _ -> SInit
            end; % end cursor
			_ -> SInit
			end; % case control
        %--------------------------------------------------------------------------------------------------------------------
        _ ->
            St
    end.

player_new_action(#player_state{coord={MyX,MyY,_}, state={MoveState,_}} = State, X,Y, A, {MType,_} = NewMoveState) ->
	if (MyX == X) and (MyY == Y) ->
		   	State1 = case MType of
				move_virtual_object -> 
					State#player_state{linked_objid = virtual_collision};
				_ ->
					State
			end,
				
			State1#player_state{ action = A, context = none };
		true ->
			State1 = case is_moving(MoveState) of
				true -> player_update_move(State);
				false -> State
			end,
			player_update_move(State1#player_state{
									action = A,
									context = none,
									state = NewMoveState
								   })
	end.

%-----------------------------------------------------------------------------------
% взаимодействие вещи с чем то
% что-то держим в руке и правый клик по чему-либо
player_itemact(State,
			   ObjID,X,Y, _MX,_MY, _Btn, Mod) ->
	?DEBUG("player_itemact ~p mod=~p",[ObjID,Mod]),
	case ObjID of
	0 ->
		case Mod of
		0 ->
			% клик по земле
			player_seed(State, X, Y, false);
		?MOD_SHIFT ->
			player_seed(State, X, Y, true);
		_ -> 
			State
		end;
	_ -> % object click--------------------------
		case Mod of
		0 ->
			player_seed(State, X, Y, false);
		?MOD_SHIFT ->
			player_seed(State, X, Y, true);
		_ -> 
			State
		end
	end.

player_seed(#player_state{item_hand = Hand, grids=Grids, skills=Skills, send_pid=SendPid} = State,
			   X,Y, Continous ) ->
	#item{type = HandType} = Hand,
	?DEBUG("player_seed hand_type=~p",[HandType]),
	% проверим есть ли у нас фарминг
	case skills:get_skill(Skills, farming) of
		false -> player_net:send_system_msg(SendPid, "havnt skill farming"), State;
		_ ->
			% смотрим в какой тайл тыкнули
			case grid_get_tile(Grids, X, Y) of
				none ->
					State;
				{get_tile_ack, #tile{type=TileType}} ->
					case TileType of
						% кликнули по пашне - надо посадить
						plowed ->
							{TX, TY} = map_server:tilify_center(X, Y),
							
							% если держим в руках семечку
							case farming:is_seed(HandType) of
								true ->
									?DEBUG("try to seed"),
									player_new_action(State, TX,TY,
										{seed, TX, TY, Continous},
										% движемся к виртуальному объекту которое даст семечко
										{move_virtual_object, {get_tick_count(), TX,TY, farming:get_plant_type(HandType)}}
													 );
								false -> % в руках не семечка
									State
							end;
						_ ->
							% кликнули по другому типу тайла (не пашня)
							State
					end
			end
	end.


%-----------------------------------------------------------------------------------
% drop item in hand
player_drop(State) ->
    #player_state{ item_hand = Hand, grids = Grids,
                   coord = {X,Y,_} } = State,
    ?DEBUG("player_drop ~p",[Hand]),
    case Hand of
        none -> State;
        #item{id=ObjID} ->
            db_server:inventory_remove(ObjID),
            send_all_grids(Grids, {object_drop, Hand, round(X), round(Y)}),
            player_set_hand(State, none, 0, 0)
    end.


%-----------------------------------------------------------------------------------
% handle place object
player_place_object(St, Place,X,Y) -> player_place_object(St, Place, 0, X,Y).
player_place_object(St, none,_,_,_) -> St;
player_place_object(St, {PlaceType, ItemObjId, Action}, Mod, X,Y) ->
	{TX, TY} = if 
				   Mod == ?MOD_SHIFT -> 
					   map_server:tilify_center(X, Y);
				   true -> 
					   {X, Y}
			   end,
    ?DEBUG("player_place_object type=~p id=~p action=~p x=~p y=~p",[PlaceType, ItemObjId, Action, TX,TY]),
    #player_state{ coord = MyCoord, state = MoveState, send_pid = SendPid, grids = Grids } = St,
    case Action of
        spawn ->
            ?DEBUG("try to spawn"),
            case send_grid_msg_ack(Grids, {player_spawn_object, self(), PlaceType,TX,TY,
					10, % Q
					100, % hp
					none}
			) of
                {spawned_id, _} -> ok;
                {spawned_fail} -> ?DEBUG("spawn fail!"), ok
            end,
            player_set_place_obj(St, none);
        spawn_open ->
            % заспавнить и открыть объект (стройка без вещи)
            ?DEBUG("try to spawn"),
            case send_grid_msg_ack(Grids, {player_spawn_object, self(), PlaceType,TX,TY,
					10, % q
					100, % hp
					none}
			) of
                {spawned_id, NewObjID} ->
                    ?DEBUG("spawned_id ~p",[NewObjID]),
                    S1 = player_set_place_obj(St, none),
                    % link to build
                    % open build window
                    player_do_action(S1,
                                     right_click, NewObjID, 0, 0);
                {spawned_fail} ->
					?DEBUG("spawn fail!"),
					player_net:send_system_msg(SendPid, "build failed"),
                    player_set_place_obj(St, none)
            end;
        build ->
            SInit = player_set_place_obj(St, none),
            {MyX,MyY,_MyLv} = MyCoord,
            A = {build_place, 0,0,0, {PlaceType,ItemObjId,TX,TY}},
            if (MyX == TX) and (MyY == TY) ->
                                    SInit#player_state{ action = A, context = none };
            true ->
                                    St1 = case is_moving(MoveState) of
                                        true -> player_update_move(SInit);
                                        false -> SInit
                                    end,
                                    player_update_move(St1#player_state{
                                                            action = A,
                                                            context = none,
                                                            state =
                                                                {move_virtual_object,
                                                                  {get_tick_count(), TX,TY, build:get_build_type(PlaceType)}
                                                                }
                                                           })
            end
    end.

%-----------------------------------------------------------------------------------
% player set progress
player_set_progress(St, Progress) ->
    ?DEBUG("player_set_progress ~p", [Progress]),
    #player_state{ progress = Old, send_pid=SendPid } = St,
    if Progress =/= Old ->
           case Progress of
               none -> player_net:send_progress(SendPid, -1);
               _ -> player_net:send_progress(SendPid, Progress)
           end,
           St#player_state{ progress = Progress };
       true -> St
    end.

%-----------------------------------------------------------------------------------
% process action to object
player_do_action(St, ObjAction, ObjID, MX, MY) ->
    #player_state{ charid = CharID,
                   context=Context,
				   grids = Grids,
                   send_pid=SendPid   } = St,
%    {MoveState, _} = CurState,
	% TODO : player_do_action lock_fail
	% обработать lock_fail - может быть ошибка блокировки нужных гридов для получения всех данных
	% нужно попытатся несколько раз отправить мессагу
    case send_grid_msg_ack(Grids, {obj_action, ObjAction, self(), CharID, St, ObjID}) of
		{target} ->
			player_set_target(St, ObjID);

		{context, []} ->
									St#player_state{ context = none };
        {context, Items} ->
                                    ?DEBUG("PLAYER: grid return context: ~p",[Items]),
									% надо проверить может уже есть контекст меню
									case Context of
										none ->
                                    		player_net:send_context_menu(SendPid, MX,MY,Items),
                                    		St#player_state{ context = {ObjID, Items} };
										_ ->
											% !!! ADMIN контекстное меню уже открыто
											St
									end;
        {action, Name, Ticks, ActionObjID} ->
                                    ?DEBUG("PLAYER: grid return action: ~p",[Name]),
                                    player_recv_action(St, Name, Ticks, ActionObjID);
        {none} ->
                                    ?DEBUG("PLAYER: grid return none"),
                                    St;
		none -> 
                                    ?DEBUG("PLAYER: grid return none"),
                                    St
    end.

%-----------------------------------------------------------------------------------
% process action from session
player_recv_action(St, Name, Ticks, ActionObjID) ->
    ?DEBUG("PLAYER action: ~p, ticks=~p id=~p", [Name, Ticks, ActionObjID]),
    #player_state{ state = CurState, coord = MyCoord,
				   grids = Grids,
				   linked_objid = LinkedObjID,
				   charid = CharID } = St,
    {MyX, MyY, _} = MyCoord,
    % need move to object if not linked to it
    case ActionObjID of
		CharID ->
            ?DEBUG("its me!"),
			fly_text(St, MyX, MyY, 2, atom_to_list(get_action_name(Name))),
            St#player_state{ action = {Name, Ticks, Ticks,0, ActionObjID}, context = none, linked_objid = CharID };
%%         LinkedObjID ->
%%             ?DEBUG("already near with linked obj"),
%% 			fly_text(St, MyX, MyY, 2, atom_to_list(get_action_name(Name))),
%%             St#player_state{ action = {Name, Ticks, Ticks,0, ActionObjID}, context = none };
        {point, PX, PY} ->
			fly_text(St, PX, PY, 2, atom_to_list(get_action_name(Name))),
            if (PX == MyX) and (PY == MyY) ->
                ?DEBUG("already near with target point"),
                St#player_state{ action = {Name, Ticks, Ticks,0, ActionObjID}, context = none };
            true ->
                St#player_state{ context = none }
            end;
        _ ->
			% выведем действие отлетающим текстом
			% ищем объект чтобы узнать его координаты
			case send_grid_msg_ack(Grids, {find_object, self(), ActionObjID}) of
                {object_found, #object{coord={OX,OY,_Lv}}} ->
                    fly_text(St, OX,OY, 2, atom_to_list(get_action_name(Name)));
                none ->
					ok
            end,

            {PState, _} = CurState,
            St1 = case is_moving(PState) of
                true -> player_update_move(St);
                false -> St
            end,
			
			if
				% если уже прилинкованы к нужному объекту и ид объекта не нулевой
				(LinkedObjID == ActionObjID) andalso (ActionObjID =/= none) ->
					?DEBUG("already near with linked object"),
					St2 = St1#player_state{
	                        action = {Name, Ticks, Ticks, 0, ActionObjID},
							context = none
									},
					if 
						Ticks == 0 -> player_update_action(St2, 0);
						true -> St2
					end;
				true ->
	            	% move to point
	            	?DEBUG("move to object id=~p",[ActionObjID]),
		            % update moving
	    	        Time = get_tick_count(),
	        	    player_update_move(St1#player_state{
	                        action = {Name, Ticks, Ticks, 0, ActionObjID},
	                        context = none,
	                        state = {move_object, {Time, ActionObjID} }
	                                               })
			end
    end.


%-----------------------------------------------------------------------------------
% handle player context choose
player_context_action(St, Action) ->
    #player_state{ context = Context, grids = Grids, send_pid = SendPid,
                   inventory = InvList } = St,
    ?DEBUG("player_context_action: ~p context=~p", [Action,Context]),
    case Context of
        none ->
            ?DEBUG("WARNING player: no context menu ~p", [Action]),
            St;
		% инвентарь
        {inventory, ObjID,Items} ->
			S0 = St#player_state{context = none},
			case Action of
            [] -> % no select action
                % cancel context menu
                ?DEBUG("player: cancel context menu"),
                S0;
            _ -> % some selected
                case lists:member(Action, Items) of
                    false ->
                        ?DEBUG("WARNING player: no context action ~p", [Action]);
                    true ->
                        ?DEBUG("select item context action!"),
                        % find item in inventory
                        case inventory:take_item(InvList, ObjID) of
                            false ->
								player_net:send_system_msg(SendPid, "no item in root inventory"),
								?DEBUG("WARNING player: no such item in inventory"), S0;
                            {Item, _} -> case inventory:item_context(Item, Action, InvList, S0#player_state.charid, S0) of
                                none ->
                                    ?DEBUG("action: none"),
                                    S0#player_state{ context = none };
											 
                                {place, Place} ->
                                    ?DEBUG("placing ~p",[Place]),
                                    player_set_place_obj(S0, Place);
		 
								{state, SNew, NewInventory} ->
									player_update_inventory(SNew, NewInventory);
											 
								{state_func, F, NewInventory} ->
									F(player_update_inventory(S0, NewInventory));
								
								_ -> 
									player_net:send_system_msg(SendPid, "unknown item action"),
									S0#player_state{ context = none }
                            end
                        end
                end
        end;

		% контекстное меню вещи в инвентаре другого объекта грида
		{inventory_obj, ObjID, ItemObjID, Items} -> case Action of
            [] -> % no select action
                % cancel context menu
                ?DEBUG("player: cancel context menu"),
                St#player_state{ context = none };
            _ -> % some selected
                case lists:member(Action, Items) of
                    false ->
                        ?DEBUG("WARNING player: no context action ~p", [Action]);
                    true ->
						S0 = St#player_state{ context = none },
                        case send_grid_msg_ack(Grids, {action_completed, self(), St#player_state.charid, St, {context_item, Action, ItemObjID}, ObjID}) of
                            none ->
                                    ?DEBUG("action: none"),
                                    S0#player_state{ context = none };
                            {place, Place} ->
                                ?DEBUG("placing ~p",[Place]),
                                player_set_place_obj(S0, Place);
		 
							{state, SNew} -> SNew;
							{state_func, F} -> F(S0);
							
							_ -> 
								player_net:send_system_msg(SendPid, "unknown item action"),
								S0#player_state{ context = none }
                        end
                end
			end;
			

		% контекстное меню объекта
        {ObjID,Items} -> case Action of
            [] -> % no select action
                % cancel context menu
                ?DEBUG("player: cancel context menu"),
                St#player_state{ context = none };
            _ -> % some selected
                case lists:member(Action, Items) of
                    false ->
                        ?DEBUG("WARNING player: no context action ~p", [Action]);
                    true ->
                        case send_grid_msg_ack(Grids, {context_action, self(), St, ObjID, Action}) of
                            none ->
                                ?DEBUG("action: none"),
                                St#player_state{ context = none };
							{sysmsg, Text} ->
								player_net:send_system_msg(SendPid, Text),
								?DEBUG("action: sysmsg=~p",[Text]),
								St#player_state{ context = none };
							{build, BuildType, BuildItem, BX, BY} ->
								player_place_object(St, {BuildType, BuildItem, build}, BX, BY);
                            {action, Name, Ticks, ActionObjID} ->
                                player_recv_action(St, Name, Ticks, ActionObjID)
                        end
                end
        end
    end.

%-----------------------------------------------------------------------------------
player_eat(State, #item{type=Type} = Item) ->
	?DEBUG("eat ~p",[Item]),
	case food:is_food(Type) of
		true ->
			FoodFep = food:get_fep(Item),
			S1 = player:eat_fep(FoodFep, State),
			db_server:stat_update(S1#player_state.stats, S1#player_state.charid),
			player_net:send_stat(S1),
			S1;
		_ ->
			?WARNING_MSG("eat not food item! ~p",[Item]), State
	end.

%-----------------------------------------------------------------------------------
% закончилось действие направленное на себя
player_myaction_complete(#player_state{charid=CharID} = State, Name) ->
	?DEBUG("player_myaction_complete ~p",[Name]),
	case Name of
		{player, war_buff, _} ->
			% добавим бафф на себя
			B = #buff{duration=buff:get_duration(war_buff),
					  time=buff:get_duration(war_buff),
					  target_id=CharID,
					  type=war_buff},
			player_buff_add(State#player_state{action=none}, B);

		{player, god_buff, _} ->
			% добавим бафф на себя
			B = #buff{duration=buff:get_duration(god_buff),
					  time=buff:get_duration(god_buff),
					  target_id=CharID,
					  type=god_buff},
			player_buff_add(State#player_state{action=none}, B);
		_ ->
			?DEBUG("unknown action ~p",[Name]),
			State
	end.

%-----------------------------------------------------------------------------------
% update action. return new state
player_update_action(St, DT) ->
    #player_state{
        linked_objid = LinkedObjID,
        action = Action,
        send_pid=SendPid,
		%coord = MyCoord,
        inventory = Inventory,
		item_hand = Hand,
%        state = CurrentState,
        grids = Grids, charid = CharID,
		stamina_consume_timer = StaminaTimer,
		stamina = Stamina,				 
		coord = {_MyX, _MyY, MyLv} = _MyCoord
                  } = St,
	%?DEBUG("player update action ~p",[Action]),
    case Action of
        none -> St; % nothing to do
		%--------------------------------------------------------------------
		{lay_stone, TimeLeft, TimeFull, LastProgress, X, Y} ->
			case LinkedObjID of
				{point, X, Y} ->
					T = TimeLeft-DT,
        		    % if action not completed
            		if (T > 0) ->
        		        P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                		if (P > LastProgress) -> player_net:send_progress(SendPid, P);
		                    true -> ok end,
        		        St#player_state{ progress = P,
                		    action = {lay_stone, T, TimeFull, P, X, Y}
                        		       };
					% action completed
        		    true ->
						S1 = player_reset_action(St),
						case grid_get_tile(Grids, X, Y) of
							none ->
								S1;
							{get_tile_ack, #tile{type=TileType}} ->
								case map_server:can_lay_stone(TileType) of
									% не можем мостить тайл
									false -> player_net:send_system_msg(SendPid, "cant lay stone this tile"), S1;
									true ->
								% тайл можем мостить, проверим камень в инветаре
								case inventory:take_items(Inventory, stone_piece, 1, []) of
									false -> player_net:send_system_msg(SendPid, "havnt stone in inventory"), S1;
									{TakedItems, NewInv} ->
										% удалить камень из инвентаря
										S2 = player_gain_exp( 
											player_update_inventory(S1, NewInv),
											#exp{nature=2}),
										% уничтожаем вещи в бд
										inventory:destroy_items(TakedItems),
		
										% и изменить тайл на "мощеный"
		           						{Sg, Grid, Index} = map_server:get_sg_grid_index(X,Y,MyLv),
										send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=sett}}),
		
										player_set_reuse(S2,
											player:get_action_reuse(lay_stone)
														)
								end end
						end

					end;
				_ -> St
			end;
		%--------------------------------------------------------------------
		{lay_grass, TimeLeft, TimeFull, LastProgress, X, Y} ->
			case LinkedObjID of
				{point, X, Y} ->
					T = TimeLeft-DT,
        		    % if action not completed
            		if (T > 0) ->
        		        P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                		if (P > LastProgress) -> player_net:send_progress(SendPid, P);
		                    true -> ok end,
        		        St#player_state{ progress = P,
                		    action = {lay_grass, T, TimeFull, P, X, Y}
                        		       };
					% action completed
        		    true ->
						S1 = player_reset_action(St),
						case grid_get_tile(Grids, X, Y) of
							none ->
								S1;
							{get_tile_ack, #tile{type=TileType}} ->
								case map_server:can_lay_grass(TileType) of
									% не можем мостить тайл
									false -> player_net:send_system_msg(SendPid, "cant lay grass this tile"), S1;
									true ->
								% тайл можем мостить, проверим камень в инветаре
								case inventory:take_items(Inventory, slot_seed, 1, []) of
									false -> player_net:send_system_msg(SendPid, "havnt seed in inventory"), S1;
									{TakedItems, NewInv} ->
										% удалить камень из инвентаря
										S2 = player_gain_exp( 
											player_update_inventory(S1, NewInv),
											#exp{nature = 2}),
										% уничтожаем вещи в бд
										inventory:destroy_items(TakedItems),
		
										% и изменить тайл на "мощеный"
		           						{Sg, Grid, Index} = map_server:get_sg_grid_index(X,Y,MyLv),
										send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=grass}}),
		
										player_set_reuse(S2,
											player:get_action_reuse(lay_stone)
														)
								end end
						end

					end;
				_ -> St
			end;
		%--------------------------------------------------------------------
		{seed, X, Y, Continous} -> % посадить семечко. которое должны держать в руке
			case LinkedObjID of
            % нужна виртуальная коллизия
            virtual_collision ->
			%?DEBUG("seed get virt collision"),
			case Hand of
				none -> St;
				#item{id = HandObjID, type=HandType, q=HandQ} ->
				case farming:is_seed(HandType) of
					false -> St;
					true ->
					% берем фильтр на растения
					F = farming:get_plant_filter(),
					% область одного тайла
					{TX, TY} = map_server:tilify(X, Y),
					Rect = {TX, TY, TX+?TILE_SIZE, TY+?TILE_SIZE},
					% ищем в этом тайле другие растения
					case send_grid_msg_ack(Grids, {find_filter_objects, F, Rect, self()}) of
						none ->
							% в этом тайле нет растений
							PlantType = farming:get_plant_type(HandType),
							case PlantType of
								error -> St;
								_ ->
							case grid_get_tile(Grids, X, Y) of
								none ->
									St;
								{get_tile_ack, #tile{type=TileType}} -> case TileType of
								plowed -> 
									% надо посадить семечко
									?DEBUG("need to seed ~p",[Hand]),
									% спавним морковку в тайле
									{TX, TY} = map_server:tilify(X, Y),
									ObjState = farming:generate_state(St,Hand),
									case send_grid_msg_ack(Grids, {player_spawn_object,
											self(),
											PlantType,
											TX+(?TILE_SIZE div 2),
											TY+(?TILE_SIZE div 2),
											HandQ,
											objects_server:get_hp(HandType, HandQ, ObjState),
											ObjState
											}
									) of
                					{spawned_fail} ->
                    					?DEBUG("spawned_fail"),
                    					St;
					                {spawned_id, _NewObjID} ->
										% удаляем то что в руке
										db_server:inventory_remove(HandObjID),
										S1 = skills:skill_gain_ap( 
											player_gain_exp(St, #exp{nature=2}),
											farming, 1),
										% надо ли продолжить сеяние (зажат шифт)
										case Continous of
											false ->
												player_set_reuse(
													player_set_hand(S1#player_state{action=none}, none, 0, 0),
													player:get_action_reuse(seed));
											true ->
												player_set_reuse(
													player_take_item_to_hand(S1#player_state{action=none}, HandType),
													player:get_action_reuse(seed))
										end
									end
							end
							end
							end;
						{find_filter_objects_ack, _} ->
							% в тайле найдено растение
							St
					end
				end
			end;
			_ -> St
			end;
		%--------------------------------------------------------------------
        {build_place,_,_,_, Build} -> % set building
            case LinkedObjID of
                % для строительства нужна виртуальная коллизия
                virtual_collision -> % we come to place object
                    {BuildType,ItemObjID,ToX,ToY} = Build,
                    ?DEBUG("build ~p ~p",[BuildType,ItemObjID]),
                    case ItemObjID of
                        none ->
                            % строим без вещи
                            player_place_object(player_reset_action(St),
                                                {build:get_build_type(BuildType),none,spawn_open}, ToX,ToY);
                        _ ->
                            case lists:keyfind(ItemObjID, #item.id, Inventory) of
                                false ->
									player_reset_action(St);
                                Item ->
									player_set_reuse(
									player_build(
                                  			player_reset_action(St),
                                  			build:get_build_type(BuildType),Item,ToX,ToY
									),
									player:get_action_reuse(build)
									)
                            end
                    end;
               _ -> St
            end;
		% -------------------------------------------------------------
        {lift_down, _,_,_, ObjID, ToX, ToY} ->
            case LinkedObjID of
                % for lift need virtual collision
                virtual_collision -> % we come to place object
                    case send_grid_msg_ack(Grids, {action_completed, self(),CharID, St, {lift_down, ToX, ToY}, ObjID}) of
                            {action_end} ->
                                ?DEBUG("action_end"),
                                % reset progress
                                S1 = player_set_progress(St, none),
                                S1#player_state{ action = none }
                    end;
                _ -> St
            end;
		% -------------------------------------------------------------
		{plow, TimeLeft, TimeFull, LastProgress, X, Y} ->
			case LinkedObjID of
				{point, X, Y} ->
					T = TimeLeft-DT,
					MinStamina = player:get_action_min_stamina(plow),
        		    % if action not completed
            		if 
					(Stamina < MinStamina) ->
						player_set_progress(St#player_state{action=none}, none);
					(T > 0) ->
		                ?DEBUG("plow action not completed ticks=~p",[T]),
        		        P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                		if (P > LastProgress) -> player_net:send_progress(SendPid, P);
		                    true -> ok end,
						NewStaminaTimer = StaminaTimer + player:get_action_stamina(plow),
        		        St#player_state{stamina_consume_timer = NewStaminaTimer,  
										progress = P,
                		    action = {plow, T, TimeFull, P, X, Y}
                        		       };
					% action completed
        		    true ->
						?DEBUG("send plow tile"), 
						send_all_grids(Grids, {plow_tile, X, Y}),
						player_set_reuse(
						  	player_reset_action(St),
							player:get_action_reuse(plow)
						)
					end;
				_ -> St
			end;
		% -------------------------------------------------------------
		{dig_hole, TimeLeft, TimeFull, LastProgress, X, Y} ->
			case LinkedObjID of
				{point, X, Y} ->
					T = TimeLeft-DT,
					MinStamina = player:get_action_min_stamina(dig_hole),
        		    % if action not completed
            		if
					(Stamina < MinStamina) ->
						player_set_progress(St#player_state{action=none}, none);
					(T > 0) ->
		                ?DEBUG("dig hole action not completed ticks=~p",[T]),
        		        P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                		if (P > LastProgress) -> player_net:send_progress(SendPid, P); true -> ok end,
						NewStaminaTimer = StaminaTimer + player:get_action_stamina(dig_hole),
        		        St#player_state{stamina_consume_timer = NewStaminaTimer, 
										progress = P,
                		    action = {dig_hole, T, TimeFull, P, X, Y}
                        		       };
					% action completed
        		    true ->
						S1 = player_reset_action(St),
						case grid_get_tile(Grids, X, Y) of
							none ->
								S1;
							{get_tile_ack, #tile{type=TileType}} ->
								case map_server:can_dig_hole(TileType) of
									false -> player_net:send_system_msg(SendPid, "cant dig hole this tile"), 
											S1;
											 
									true ->							
				   						{Sg, Grid, Index} = map_server:get_sg_grid_index(X,Y,MyLv),
										send_all_grids(Grids, {update_tile, {Sg, Grid}, Index, #tile{type=hole}}),
										
										player_gain_exp(
										player_set_reuse(
											player_set_progress(S1, none),										 
											player:get_action_reuse(dig_hole)
														),
											#exp{industry=1})
										
								end
						end
					end;
				_ -> St
			end;
		% -------------------------------------------------------------
		{dig, TimeLeft, TimeFull, LastProgress, X, Y} ->
			case LinkedObjID of
				{point, X, Y} ->
					T = TimeLeft-DT,
					MinStamina = player:get_action_min_stamina(dig_hole),
        		    % if action not completed
            		if
					(Stamina < MinStamina) ->
						player_set_progress(St#player_state{action=none}, none);
					(T > 0) ->
		                ?DEBUG("dig action not completed ticks=~p",[T]),
        		        P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                		if (P > LastProgress) -> player_net:send_progress(SendPid, P); true -> ok end,
						NewStaminaTimer = StaminaTimer + player:get_action_stamina(dig_hole),
        		        St#player_state{stamina_consume_timer = NewStaminaTimer, 
										progress = P,
                		    action = {dig, T, TimeFull, P, X, Y}
                        		       };
					% action completed
        		    true ->
						S1 = St, %#player_state{action = none},
						case grid_get_tile(Grids, X, Y) of
							none ->
								player_set_reuse(
											player_set_progress(S1#player_state{action=none}, none),										 
											player:get_action_reuse(dig)
														);
							{get_tile_ack, #tile{type=TileType} = Tile} ->
								case map_server:can_dig(TileType) of
									false -> player_net:send_system_msg(SendPid, "cant dig this tile"), 
											player_set_reuse(
											player_reset_action(S1),										 
											player:get_action_reuse(dig)
														);											 
									true ->							
										SpawnItems = map_server:spawn_tile_items(Tile),
										?DEBUG("spawn tile items: ~p",[SpawnItems]),
										TT = player:get_action_time(dig),
										NewAction = {dig, TT, TT, 0, X, Y},
										S2 = player_gain_exp( 
											player_add_items(S1, SpawnItems),
											#exp{nature=1}),
				
										if 
											% в руке чтото держим возможно это кусок земли который не влез в инвентарь. прерываем действие.
											S2#player_state.item_hand =/= none ->
												player_set_reuse(
												  player_reset_action(S2),
												  player:get_action_reuse(dig)
																);
											true ->
												player_set_reuse(
													player_set_progress(S2#player_state{action=NewAction}, none),										 
													player:get_action_reuse(dig)
																)
										end
										
								end
						end
					end;
				_ -> St
			end;		
		% -------------------------------------------------------------
        {craft, CraftName, Click, TimeLeft, TimeFull, LastProgress} ->
            T = TimeLeft-DT,
            % if action not completed
            if (T > 0) ->
                ?DEBUG("craft action not completed ticks=~p",[T]),
                P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                if (P > LastProgress) -> player_net:send_progress(SendPid, P);
                    true -> ok end,
                St#player_state{ progress = P,
                    action = {craft, CraftName, Click, T, TimeFull, P}
                               };
			% action completed
            true ->
                ?DEBUG("craft action completed"),
                {FinalList, ReqList, _, _, NewTime} = craft:get_craft(CraftName),
				% проверяем условия
				case craft:check_craft(CraftName, St) of
					% крафт не возможен
					{false, A1, A2, A3} ->
						% выводим сообщения о причинах
			            if A1 == false ->
               				player_net:send_system_msg(SendPid, "need linked object"); true -> ok end,
            			if A2 == false ->
               				player_net:send_system_msg(SendPid, "havnt requied equip"); true -> ok end,
            			if A3 == false ->
               				player_net:send_system_msg(SendPid, "havnt required items in inventory"); true -> ok end,
						player_set_reuse(
            				player_reset_action(St),
							player:get_action_reuse(craft)
						);
					% крафт возможен
					true ->
                		% заполняем крафт лист
                		case craft:fill_craft_list(ReqList, Inventory, ReqList) of
							false -> error; % ADMIN!!!! т.к. уже проверили условия крафта. должно быть все ок
							{NewInventory, FilledList} ->
                				% крафтим вещь
								{NewItems, GainExp} = craft:craft(FilledList, FinalList, CraftName),
								% обновляем инвентарь.
                				St1 = player_gain_exp( 
									player_update_inventory(St, NewInventory),
									GainExp),
								% убираем старые вещи.
								DestroyList = craft:get_taked_list(FilledList, []),
								% уничтожаем вещи в бд
								inventory:destroy_items(DestroyList),

								?DEBUG("new inventory ~p", [NewInventory]),
								% кладем новые вещи в инвентарь
								St2 = player_add_items(St1, NewItems),
                				% заполняем новые данные
								NewAction = case Click of
									all ->
										% надо проверить возможен ли крафт с учетом изменений
										% проверяем условия
										case craft:check_craft(CraftName, St2) of
											% крафт не возможен
											{false, _A1, _A2, _A3} ->
												% выводим сообщения о причинах
%% 												if A1 == false ->
%% 													player_net:send_system_msg(SendPid, "need linked object"); true -> ok end,
%% 												if A2 == false ->
%% 													player_net:send_system_msg(SendPid, "havnt requied equip"); true -> ok end,
%% 												if A3 == false ->
%% 													player_net:send_system_msg(SendPid, "havnt required items in inventory"); true -> ok end,
												player_net:send_progress(SendPid, -1),
												none;
											true ->
												player_net:send_progress(SendPid, 0),
												{craft, CraftName, Click, NewTime, NewTime, 0}
										end;
									_ ->
										player_net:send_progress(SendPid, -1),
										none
									end,
								player_set_reuse(
									St2#player_state{ action = NewAction },
									player:get_action_reuse(craft)
								)
						end
				end
           	end;
		% -------------------------------------------------------------
        { Name, TimeLeft, TimeFull, LastProgress, ObjID } ->
            ?DEBUG("player_update_action action_name=~p time_left=~p linked=~p", [Name, TimeLeft, LinkedObjID]),
            case LinkedObjID of
                ObjID ->
                    T = TimeLeft-DT,
                    % if action not completed
                    if (T > 0) ->
                        ?DEBUG("action not completed"),
                        P = round( (1-(T / TimeFull)) * ?PROGRESS_STAGES),
                        if (P > LastProgress) -> player_net:send_progress(SendPid, P);
                        true -> ok end,

                        St#player_state{ progress = P,
                            action = {Name, T, TimeFull, P, ObjID}
                                       };
                    true -> % action completed
                        ?DEBUG("action completed"),
						case ObjID of
							CharID ->
								% действие направлено на себя
								S1 = player_set_progress(St, none),
								player_set_reuse(
									player_myaction_complete(S1, Name),
									player:get_action_reuse(Name)
								);
							_ ->
		                        AC = send_grid_msg_ack(Grids, {action_completed, self(),CharID, St, Name, ObjID}),
								player_process_session_action_completed(St, Name, ObjID, AC)
						end
                    end;
                _ ->
                    ?DEBUG("not action object"),
                    St
            end
    end.

player_process_session_action_completed(#player_state{
				send_pid=SendPid,
				inventory=Inv
				} = State, Name, ObjID, ActionAck) ->
	% действие на объект
    case ActionAck of
        {action_continue, NewName, NewTime} ->
	        player_net:send_progress(SendPid, 0),
    		State#player_state{ progress = 0,
                action = {NewName, NewTime, NewTime, 0, ObjID} };
		none ->
            ?DEBUG("action_end"),
	        % reset progress
    		S1 = player_set_progress(State, none),
			player_set_reuse(
            	S1#player_state{ action = none },
				player:get_action_reuse(Name)
			);
		
        {action_end} ->
            ?DEBUG("action_end"),
	        % reset progress
    		S1 = player_set_progress(State, none),
			player_set_reuse(
            	S1#player_state{ action = none },
				player:get_action_reuse(Name)
			);
		
		{new_state, SS} ->
            ?DEBUG("action_end"),
	        % reset progress
    		S1 = player_set_progress(SS, none),
			player_set_reuse(
            	S1#player_state{ action = none },
				player:get_action_reuse(Name)
			);
			
		% проверить наличие вещей в инвентаре, нужно для ремонта
		{check_items, SlotType, Count, GP} ->
			case inventory:take_items(Inv, SlotType, Count, []) of
				false -> GP ! {check_items, false};
				{_TakedItems, _NewInv} -> GP ! {check_items, true}
			end,
			receive
						{ack, Ack} -> player_process_session_action_completed(State, Name, ObjID, Ack)
			after 5000 ->
						player_process_session_action_completed(State, Name, ObjID, {action_end})
			end
	end.



%-----------------------------------------------------------------------------------
% update moving. return new state
player_update_move(St) ->
    %?DEBUG("player_update_move"),
    #player_state{
                  state = {MoveState, MoveParams},
                  linked_objid = OldLinked,
				  grids = Grids
                 } = St,
    SNew = case MoveState of
        move_point ->
            %?DEBUG("PLAYER: player_update_move: move_point"),
            {LastTime, ToX, ToY} = MoveParams,
            update_move(St, ToX, ToY, LastTime, none);
        move_object ->
            %?DEBUG("PLAYER: player_update_move: move_object"),
            {LastTime, ObjID} = MoveParams,
            case send_grid_msg_ack(Grids, {find_object, self(), ObjID}) of
                {object_found, #object{type=Type, coord={OX,OY,_Lv}}} ->
                    update_move(St, OX, OY, LastTime, Type);
                none ->
                    St#player_state{ state = {stay, none}, linked_objid = none }
            end;
        move_follow ->
            %?DEBUG("PLAYER: player_update_move: move_follow"),
            {LastTime, ObjID} = MoveParams,
            case send_grid_msg_ack(Grids, {find_object, self(), ObjID}) of
                {object_found, #object{coord={OX,OY,_Lv}}} ->
                    St2 = update_move(St, OX, OY, LastTime, none),
                    % continue moving anyway
                    St2#player_state{ state = {move_follow, {get_tick_count(), ObjID}}};
                none ->
                    St#player_state{ state = {stay, none}, linked_objid = none }
            end;
        move_virtual_object ->
            %?DEBUG("PLAYER: player_update_move: move_virtual_object"),
            {LastTime, ToX, ToY, VirtType} = MoveParams,
			%?DEBUG("have collision ~p bounds=~p", [HaveCollision, Bounds]),
            update_move(St, ToX, ToY, LastTime, VirtType);
        _ ->
            St
    end,
    #player_state{ linked_objid = NewLinked } = SNew,
    if OldLinked =/= NewLinked -> player_close_obj(SNew, OldLinked); true -> SNew end.

%-----------------------------------------------------------------------------------
% update moving. return new state. internal function
update_move(St, ToX, ToY, LastTime, Params) ->
    %?DEBUG("update_move"),
    #player_state{ %state = {_MoveState, MoveParams},
                   coord = {OldX, OldY, Lv} = CC,
                   charid = CharID,
                   grids = Grids,
				   stamina_consume_timer = StaminaTimer,
                   state = {MoveState, MoveParams} } = St,


%%	{LastTime, ToX, ToY} = MoveParams,

	Speed = player:get_current_speed(St),
	WG = get_work_grid(CC, Grids),

	NowTime = get_tick_count(),
	DT = (NowTime - LastTime) / 1000,
	Len = get_distance(OldX,OldY,ToX,ToY),
	DLen = Speed * DT,
	StaminaMod = if Speed > 40 -> 1.3;
					true -> 1
				 end,
	NewStaminaTimer = StaminaTimer + DLen * Speed * StaminaMod * 0.6,
	% тип виртуального объекта для коллизии. если ничего нет вернем none
	VType = case MoveState of
        move_virtual_object ->
            {_, _, _, VT } = MoveParams,
			VT;
		_ -> none
	end,

	{NewMoveState, LinkedObjID} =
    if
	(Speed == 0) ->
		% если скорость нулевая - то мы стоим на месте
	   	X = OldX, Y = OldY,
       	?DEBUG("speed 0 - stop"),
       	{{stay, none}, none};

	(Len == 0) ->
		% если расстояние нулевое - то мы стоим на месте
	   	X = OldX, Y = OldY,
       	?DEBUG("len 0 - stop"),
		case MoveState of
			move_object ->
				{_,MoveObjID} = MoveParams,
				{{stay, none}, MoveObjID};
			move_point ->
				{{stay, none}, {point,ToX,ToY}};
			move_virtual_object ->
				{{stay, none}, virtual_collision};
			_ ->
				{{stay, none}, none}
		end;

	(DLen > Len) ->
		TmpX = ToX, TmpY = ToY,
		% check collision with objects
		case try_request_move(WG, 
				{move_request, self(), 
				 	% в objid передаем все параметры движения
				 	{CharID, MoveState, MoveParams}, player, {VType, ToX, ToY}, OldX, OldY, TmpX,TmpY, pos, ?MOVE_LAND}, 
				?MOVE_TRY_COUNT) 
		of
			{collision_obj, CX, CY, _, CollisionObjID} ->
                ?DEBUG("collision! ~p ~p ~p",[CollisionObjID, CX, CY]),
				% we stopped
				X = CX, Y = CY,
				{{stay, none}, CollisionObjID};

			{collision_tile, CX, CY, _TileID} ->
				% we stopped
				X = CX, Y = CY,
				{{stay, none}, none};

			{collision_virtual, CX, CY} ->
				case MoveState of
					move_virtual_object ->
						% we stopped
                        X = CX, Y = CY,
                        ?DEBUG("virtual collision! in stopping"),
                        {{stay, none}, virtual_collision};
					_ ->
						X = CX, Y = CY,
						error(virtual_collision_when_move_state_not_virtual)
				end;

			{no_collision} ->
                ?DEBUG("no collision, we try to stop"),
				% we stopped
                case MoveState of
                    move_object ->
                        ?DEBUG("its move to object"),
                        X = TmpX, Y = TmpY,
                        {_,MoveObjID} = MoveParams,
                        ObjType = Params,
                        case cutils:get_object_collision(ObjType, player) of
                            true ->
                                % we havent collision. but obj must be collision.
                                {{stay, none}, none};
                            false ->
                                % obj havent collsision.
                                ?DEBUG("stay in move object"),
                                {{stay, none}, MoveObjID}
                        end;
                    move_virtual_object ->
                        {_, _, _, VirtualType } = MoveParams,
                        case cutils:get_object_collision(VirtualType, player) of
                            true ->
								% мы закончили движение в конечной точке
								% установлен тип виртуального объекта. который ДАЕТ коллизию
								% значит еще не дошли до конечной точки
								?WARNING_MSG("no_collision have virtual collision at end point"),
								X = TmpX, Y = TmpY,
								{{stay, none}, none};
                            false ->
								% если мы достигли конечной точки
								X = TmpX, Y = TmpY,
                              	{{stay, none}, virtual_collision}
                        end;
                    move_point ->
                        ?DEBUG("moved to point. stay"),
                        X = TmpX, Y = TmpY,
						{{stay, none}, {point,ToX,ToY}};
                    _ ->
                        X = TmpX, Y = TmpY,
                        {{stay, none}, none}
                end
		end;
	% in moving
	true ->
		K = DLen / Len,
		% calc new position
		TmpX = round(OldX + (ToX - OldX) * K),
		TmpY = round(OldY + (ToY - OldY) * K),
		% calc move vector if no collision
		{VX, VY} = if Len > ?PLAYER_MOVE_VECTOR_LENGTH ->
								  {TmpX+((ToX - OldX) / Len) * ?PLAYER_MOVE_VECTOR_LENGTH,
								   TmpY+((ToY - OldY) / Len) * ?PLAYER_MOVE_VECTOR_LENGTH};
							  true ->
								  {ToX, ToY}
						   end,
		% check collision with objects
		% need smooth moving on client
        SetParam = case MoveState of
        move_virtual_object ->
            %{_, _, _, VirtualType } = MoveParams,
			{line_move, {Speed,TmpX,TmpY, VX, VY}};
			% not_set ????????? почему????
%%             case cutils:get_object_collision(VirtualType, player) of
%%                 true -> not_set; ??????
%%                 false -> {line_move, {Speed,TmpX,TmpY, VX, VY}}
%%             end;
        _ -> {line_move, {Speed,TmpX,TmpY, VX, VY}}
        end,
		case try_request_move(WG, 
				{move_request, self(), 
				 	{CharID, MoveState, MoveParams}, 
				 	player, {VType, ToX, ToY}, OldX, OldY, TmpX,TmpY, SetParam, ?MOVE_LAND}, 
				?MOVE_TRY_COUNT) 
		of
			{collision_obj, CX, CY, _, CollisionObjID} ->
				% we stopped
                ?DEBUG("collision! id=~p x=~p y=~p",[CollisionObjID, CX, CY]),
				X = CX, Y = CY,
				{{stay, none}, CollisionObjID};

			{collision_tile, CX, CY, _TileID} ->
				X = CX, Y = CY,
				{{stay, none}, none};


			{collision_virtual, CX, CY} ->
				?DEBUG("virtual collision! in moving"),
				case MoveState of
					move_virtual_object ->
						% we stopped
                        X = CX, Y = CY,
                        ?DEBUG("virtual collision! in stopping"),
                        {{stay, none}, virtual_collision};
					_ ->
						X = CX, Y = CY,
						error(virtual_collision_when_move_state_not_virtual)
				end;

			{no_collision} ->
                case MoveState of
                    move_point ->
                        X = TmpX, Y = TmpY,
				        {{move_point, {NowTime, ToX,ToY} }, none};
                    move_object ->
                        X = TmpX, Y = TmpY,
                        {_,O} = MoveParams,
                        {{MoveState, {NowTime, O}}, none};
                    move_follow ->
                        X = TmpX, Y = TmpY,
                        {_,O} = MoveParams,
                        {{MoveState, {NowTime, O}}, none};
                    move_virtual_object ->
                        X = TmpX, Y = TmpY,
                        {{MoveState, {NowTime, ToX,ToY,VType}}, none}
                end
		end
    end,

%% 	?DEBUG("update move DT=~p Len=~p DLen=~p ToX=~p ToY=~p OldX=~p OldY=~p X=~p Y=~p",
%% 		[DT,Len,DLen,ToX,ToY,OldX,OldY,X,Y]),

	player_moved(St#player_state{stamina_consume_timer = NewStaminaTimer,
								 coord = {X,Y,Lv}, state = NewMoveState, linked_objid = LinkedObjID }).


%-----------------------------------------------------------------------------------
% обновить бафы, удалить если кончился
player_update_buffs(St, [],_,Acc) ->
	% в конце обновим бафы в стейте игрока
	St#player_state{buffs=Acc};
player_update_buffs(#player_state{send_pid=SendPid, charid=CharID} = St, [#buff{duration=Duration, type=Type}=B|T], DT, Acc) ->
	if Duration-DT =< 0 ->
		% бафф кончился
		% удаляем из бд
		db_server:buff_remove(CharID, Type),
		% пошлем пакет удаления баффа
		player_net:send_buff_delete(SendPid, CharID, Type),
		player_update_buffs(St, T, DT, Acc);
	true ->
		% бафф еще действует, обновляем его
		NewBuff = B#buff{duration=Duration-DT},
		% применим эффекты которые дает бафф на игрока...

		% обновим бафф в базе данных
		db_server:buff_update(NewBuff),
		player_update_buffs(St, T, DT, [NewBuff|Acc])
	end.


%-----------------------------------------------------------------------------------
% добавить баф игроку
player_buff_add(#player_state{buffs=Buffs, send_pid=SendPid} = State, B) ->
	case
		 case B#buff.type of
			% если добавляем вар баф- проверим наличие вар дебафа. если есть удалим его. если нет - добавим
			war_buff ->
				case lists:keytake(war_debuff, #buff.type, Buffs) of
					false -> need_add;
					{value, _, _} ->
						player_buff_delete(State, war_debuff)
				end;
			% проверим тип - если это вар дебаф - надо проверить наличе вар бафа - и удалить его. если его нет - добавить дебаф
			war_debuff ->
				case lists:keytake(war_buff, #buff.type, Buffs) of
					false -> need_add;
					{value, _, _} ->
						player_buff_delete(State, war_buff)
				end;
			_ -> need_add
		end
	of
	need_add ->
		% добавим бафф на себя
		case lists:keytake(B#buff.type, #buff.type, Buffs) of
			false ->
					% такого бафа нет. добавляем
					db_server:buff_add(B),
					player_net:send_buff_add(SendPid, B),
					State#player_state{buffs=[B|Buffs]};
			{value, _, Tail} ->
					% такой баф уже есть. заменяем
					db_server:buff_update(B),
					player_net:send_buff_add(SendPid, B),
					State#player_state{buffs=[B|Tail]}
		end;
	NewState -> NewState
	end.

%-----------------------------------------------------------------------------------
% удалить баф
player_buff_delete(#player_state{buffs=Buffs, charid=CharID, send_pid=SendPid} = State, Type) ->
	case lists:keytake(Type, #buff.type, Buffs) of
		false ->
			State;
		{value, _, Tail} ->
			db_server:buff_remove(CharID, Type),
			player_net:send_buff_delete(SendPid, CharID, Type),
			State#player_state{buffs=Tail}
	end.


%-----------------------------------------------------------------------------------
% build something
% Item - drawing of building. will be destroyed
player_build(State, BuildType, Item, X, Y) ->
    ?DEBUG("player_build ~p ~p",[BuildType, Item]),
    #player_state{ inventory = Inventory, 
                   grids=Grids, send_pid=SendPid } = State,
    #item{id=ItemID, q=Q} = Item,
    % remove item from inventory
    case inventory:find_inventory(Inventory, ItemID) of
        false -> State;
        _ ->
            % spawn build object
            case send_grid_msg_ack(Grids, {player_spawn_object, self(), BuildType,X,Y,
					Q,
					objects_server:get_hp(BuildType, Q, Item),
					Item}) of
                {spawned_fail} ->
                    ?DEBUG("spawned_fail"),
					player_net:send_system_msg(SendPid, "build failed"),
                    State;
                {spawned_id, NewObjID} ->
                    case inventory:take_item(Inventory, ItemID) of
                    false ->
                        % ignore if not in my inventory
                        player_net:send_system_msg(SendPid, "drawing_must_be_in_my_inventory"),
                        State;
					{_, NewList} ->
                        ?DEBUG("spawned_id ~p",[NewObjID]),
                        % remove item from bd
                        db_server:inventory_remove(ItemID),
                        S1 = player_update_inventory(State, NewList),
                        % link to build
                        % open build window
                        player_do_action(S1#player_state{ linked_objid = NewObjID }, right_click, NewObjID, 0, 0)
	                end
            end
    end.

%-----------------------------------------------------------------------------------
% login player into world
player_login(CharID, St) ->
    % get coords
    ?DEBUG("login..."),
    case db_server:player_load(CharID) of
        err ->
            ?DEBUG("fail load player"),
            lasterror({err, player_load});
        {AccountID,AccessLevel,Name,X,Y,Lv,Hp,SHp,HHp,Stamina,
		 ExpNature,ExpIndustry,ExpCombat,ReuseTime,ReuseLen,
		 StrLevel, StrFep, ConsLevel, ConsFep, PercLevel, PercFep,
		 AgiLevel, AgiFep, DexLevel, DexFep, IntLevel, IntFep,
		 WisLevel, WisFep, ChaLevel, ChaFep
		} ->
                ?INFO_MSG("player loaded ~p x=~p y=~p",[Name,X,Y]),
				case is_account_online(AccountID) of
					true -> error(account_already_online);
					false ->
						?MODULE ! {player_loaded, self(), AccountID},
						update_online(),
		                % линкуемся к сессии
		                case map_server:get_grid_coord(X, Y, Lv) of
		                    {coord_error} ->
								% ADMIN !!!!
		                        ?DEBUG("fail get coord x=~p y=~p lv=~p", [X,Y, Lv]),
		                        {err, get_coord_login};
		                    {{_,_},{_,_},{_,_},Sg, Grid, _} ->
		                        ?DEBUG("coord x=~p y=~p lv=~p, Sg=~p Grid=~p", [X,Y, Lv, Sg, Grid]),
		                        ?DEBUG("send logged.."),
		                        #player_state{ send_pid=SendPid } = St,
		                        send_packet(SendPid, ?GAMESERVER_LOGGED, [
												write_int(CharID),
												write_string(Name),
												write_int(time_server:get_time())
																		]),
		                        Skills = skills:load_knowledges(CharID),
								?DEBUG("skills ~p",[Skills]),
		                        % load my inventory
		                        Inventory = inventory:load_inventory(CharID),
								?DEBUG("inventory ~p",[Inventory]),
								Equip = equip:load_equip(CharID),
								?DEBUG("equip ~p",[Equip]),
		                        Hand = inventory:load_hand(CharID),
								Buffs = buff:load_buffs(CharID),
								?DEBUG("char buffs: ~p",[Buffs]),
								Stats = #player_stats{
		
													  	str= #stat{level=StrLevel,  fep=StrFep},
													  	cons=#stat{level=ConsLevel, fep=ConsFep},
													 	perc=#stat{level=PercLevel, fep=PercFep},
														agi= #stat{level=AgiLevel,  fep=AgiFep},
													 	dex= #stat{level=DexLevel,  fep=DexFep},
														int= #stat{level=IntLevel,  fep=IntFep},
														wis= #stat{level=WisLevel,  fep=WisFep},
														cha= #stat{level=ChaLevel,  fep=ChaFep}
													  },
								?DEBUG("char stats: ~p",[Stats]),
		
								Exp = #exp{combat=ExpCombat,industry=ExpIndustry,nature=ExpNature},
		
		                        %?DEBUG("player inventory: ~p", [Inventory]),
		                        NewState = St#player_state{
												inited = true,
		                                        state = {stay,none},
		                                        charid = CharID,
		                                        name = binary_to_list(Name),
		                                        accountid = AccountID,
		                                        coord = {X,Y,Lv},
												access_level = AccessLevel,
												exp=Exp,
												equip=Equip,
		                                        visible_last_coord = {0,0,none},
		                                        visible_buffer = [],
		                                        visible_zone = {1,1,0,0},
		                                        skills = Skills,
		                                        inventory = Inventory,
		                                        item_hand = none,
												buffs = Buffs,
												stats = Stats,
		                                        hp = Hp,
		                                        shp = SHp,
		                                        hhp = HHp,
												stamina = Stamina,
												reuse_time = {ReuseTime, ReuseLen}
		                                       },
		                        % add self to session
								PlayerObj = get_player_object(NewState),
								% пытаемся заспавнить игрока в мире, с учетом коллизий
								case player_spawn(PlayerObj, AccountID) of
									fail -> error(fail_spawn_player);
									% получаем фактические координаты куда заспавнились
									{#object{coord={NX,NY,NLv}}, Grids} ->
										% запросим данные у гридов
										lists:foreach(fun(#player_grid{pid=P}) ->
															  P ! {get_state, self()},
															  P ! {get_claims, self()}
													  end, Grids),
										NewState1 = NewState#player_state{coord={NX,NY,NLv}, grids=Grids},
				                        % send available actions
		        		                player_net:send_actions_list(SendPid, player:get_actions(Skills,AccessLevel)),
										player_net:send_buffs(SendPid, Buffs),
										player_net:send_current_speed(SendPid, NewState1#player_state.speed),
										player_net:send_player_param(SendPid, stamina, Stamina),
										player_net:send_stat(NewState1),
										player_net:send_inventory(SendPid, CharID, inventory:get_player_inventory_size(Equip), Inventory),
										player_net:send_equip(SendPid, Equip),
		                		        % send hand if need
		                        		S2 = case Hand of
											none ->
												NewState1;
											_ ->
		                        		    	{W,H} = inventory:item_inventory_size(inventory:get_item_type(Hand)),
		                            			player_set_hand(NewState1, Hand, W div 2, H div 2)
				                        end,
		        		                % change position
		                		        player_moved(S2)
								end
		                end
				end
    end.

% грузим 9 гридов вокруг игрока и активируем их. ждем когда все загрузятся
get_wait_grids(X,Y,Lv) ->
	% ждем полной загрузки гридов
	List = a1_utils:get_grids({X,Y,Lv}),
	Grids = lists:map(	fun (GC) ->
							   P = world_server:get_grid(GC),
							   P ! {activate, self()},
							   #player_grid{coord=GC, pid=P}
						end, List),
	wait_grids(List),
	Grids.

% заспавнится на место логина
% fail | {#object, [#player_grid]} - объект игрока и список загруженных гридов игрока, в центральном - находится игрок
player_spawn(#object{coord={X,Y,Lv}=C}=PlayerObj, AccountID) ->
	?DEBUG("try spawn player ~p ~p",[X,Y]),
	Grids = get_wait_grids(X, Y, Lv),
	P = get_work_grid(C, Grids),
	% пытаемся добавить с учетом коллизий
	case try_request_move(P, {player_add, PlayerObj, AccountID, self(), true}, ?MOVE_TRY_COUNT) of
		{ack_player_add_ok} -> {PlayerObj, Grids};
		{ack_player_add_fail} ->
			lists:foreach(fun (#player_grid{pid=PG}) ->
								   PG ! {deactivate, self()}
						  end, Grids),
			?DEBUG("fail spawn player ~p",[C]),
			player_spawn_near(X, Y, PlayerObj, AccountID, 10)
	end.

% заспавнится рядом с местом логина
player_spawn_near(_,_,PlayerObj,AccountID, 0) -> player_spawn_random(PlayerObj, AccountID, 10);
player_spawn_near(NX, NY, #object{coord={_,_,Lv}}=PlayerObj, AccountID, Count) ->
	X = NX-100+randoms:get(200),
	Y = NY-100+randoms:get(200),
	C = {X,Y,Lv},
	O = PlayerObj#object{coord={X,Y,Lv}},
	?DEBUG("try spawn player near ~p ~p",[X,Y]),

	Grids = get_wait_grids(X, Y, Lv),
	P = get_work_grid(C, Grids),
	% пытаемся добавить с учетом коллизий
	case try_request_move(P, {player_add, O, AccountID, self(), true}, ?MOVE_TRY_COUNT) of
		{ack_player_add_ok} -> {O, Grids};
		{ack_player_add_fail} ->
			lists:foreach(fun (#player_grid{pid=PG}) ->
								   PG ! {deactivate, self()}
						  end, Grids),
			?DEBUG("fail spawn player near ~p",[C]),
			player_spawn_near(NX,NY,O, AccountID, Count-1)
	end.


% заспавнится в случайном месте мира
player_spawn_random(_,_, 0) -> fail;
player_spawn_random(#object{coord={_,_,Lv}}=PlayerObj, AccountID, Count) ->
	X = ?MIN_X_COORD+randoms:get(?MAX_X_COORD-?MIN_X_COORD),
	Y = ?MIN_Y_COORD+randoms:get(?MAX_Y_COORD-?MIN_Y_COORD),
	C = {X,Y,Lv},
	O = PlayerObj#object{coord={X,Y,Lv}},
	?DEBUG("try spawn player random ~p ~p",[X,Y]),
	Grids = get_wait_grids(X, Y, Lv),
	P = get_work_grid(C, Grids),
	% пытаемся добавить с учетом коллизий
	case try_request_move(P, {player_add, O, AccountID, self(), true}, ?MOVE_TRY_COUNT) of
		{ack_player_add_ok} -> {O, Grids};
		{ack_player_add_fail} ->
			if Count > 1 ->
			% если сейчас еще не будет фейла - деактивируемся
			% если фейл - мы крашнемся. и гриды сами удалят меня по пиду
			lists:foreach(fun (#player_grid{pid=PG}) ->
								   PG ! {deactivate, self()}
						  end, Grids);
			true -> ok end,
			?DEBUG("fail spawn player random ~p",[C]),
			player_spawn_random(O, AccountID, Count-1)
	end.

%-----------------------------------------------------------------------------
% return object for player
get_player_object(State) ->
    #player_state{ charid = ObjID, coord = MyCoord, name = Name,
				   hp = HP
                    } = State,
    #object{id=ObjID, type=player, coord=MyCoord,
     params=[
			 {kin_info, Name}, 
			 {hp,HP}, 
			 {stealth, player:get_stealth(State)},
			 player:get_equip_param(State) 
			], state=none}.

%-----------------------------------------------------------------------------------
% handle moved event. update info in db
% update visible zone
% return new state with new map_grids
player_moved(State) ->
%%     ?DEBUG("player moved ~p", [State]),
    #player_state{ charid = CharID,
                   grids = OldGrids, coord = {X,Y,Lv},
                   visible_last_coord = VisibleCoord,
                   visible_buffer = VisibleObjects,
				   visible_client = VisibleClient,
%% 				   target_objid = TargetObjID,
%% 				   linked_objid = LinkedObjID,
                   send_pid=SendPid
                 } = State,
    %-------------------------------------------
    % save new coord in db
    db_server:player_update_coord(CharID, X, Y, Lv),
    %-------------------------------------------
    % update visible zone if need
    State1 = case check_visible_moved(VisibleCoord, {X,Y,Lv}) of
        true ->
            ?DEBUG("player moved change visible"),
            NewZone = get_visible_zone(X, Y),
            % get new visible objects, old removed from list
            NewVisibleObjects = clip_visible(none, NewZone, VisibleObjects),
			NewVisibleClient = clip_visible(SendPid, NewZone, VisibleClient),


			% request objects pos, when get pos - requesting info...
			lists:foreach(fun (#player_grid{pid=P}) ->
								   P ! {get_objects_in_rect, self(), NewZone}
						  end, OldGrids),
			% прилинкованный объект должен был остатся в области видимости
%% 			NewLinkedObjID = case lists:keymember(LinkedObjID, 1, NewVisibleObjects) of
%% 				true -> LinkedObjID;
%% 				false -> none
%% 			end,

%%             S1 = State#player_state{
			State#player_state{
%% 					linked_objid = NewLinkedObjID,
					visible_last_coord = {X,Y,Lv},
					visible_buffer = NewVisibleObjects,
					visible_client = NewVisibleClient,
					visible_zone = NewZone };

%% 			% обновим макрер цели - если больше не видим
%% 			?DEBUG("check target moved ~p",[TargetObjID]),
%% 			case TargetObjID of
%% 				none -> S1;
%% 				_ ->
%% 					case lists:keymember(TargetObjID, #visible_obj.id, NewVisibleObjects) of
%% 						true -> S1;
%% 						false -> player_set_target(S1, none)
%% 					end
%% 			end;
        false ->
%%             ?DEBUG("player false"),
            State
    end,
%%     ?DEBUG("player moved ~p", [State1]),
    %-------------------------------------------
    % refresh map data
    MyGrids = a1_utils:get_grids({round(X),round(Y),Lv}),
    % запросим новые гриды - если сдвинулись
    NewGrids = merge_map_grids(MyGrids, OldGrids, [], [], []),
	% деактивируем старые гриды
	lists:foreach(fun(#player_grid{coord=GC, pid=P}) ->
						  % ищем в новых гридах
						  case lists:keyfind(GC, #player_grid.coord, NewGrids) of
							  false ->
								  % если нет - деактивируем
								  P ! {deactivate, self()};
							  _ ->
								  % если есть - то все ок
								  ok
						  end
				  end, OldGrids),
    %-------------------------------------------
    State1#player_state{ grids = NewGrids }.

%-----------------------------------------------------------------------------
% set cursor in player state and send it to client
% return state
player_set_cursor(St, Cursor, SendName) ->
    #player_state{ send_pid=SendPid } = St,
    player_net:send_cursor(SendPid, SendName),
    St#player_state{ cursor = Cursor }.


player_update_inventory(State, NewInventory) ->
	?DEBUG("player_update_inventory ~p",[NewInventory]),
	#player_state{send_pid = SendPid, charid = CharID, equip = Equip} = State,
	player_net:send_inventory(SendPid, CharID, inventory:get_player_inventory_size(Equip), NewInventory),
	State#player_state{inventory = NewInventory}.

%-----------------------------------------------------------------------------
% close inventory by objid
player_close_obj(State, none) -> State;
player_close_obj(State, {point,_,_}) -> State;
player_close_obj(State, ObjID) ->
    #player_state{ grids = Grids, send_pid=SendPid} = State,
	?DEBUG("player_close_obj id=~p",[ObjID]),
    send_all_grids(Grids, {player_close_obj, self(), ObjID}),
	if is_integer(ObjID) -> player_net:send_close_object(SendPid, ObjID); true -> ok end,
	State.

%-----------------------------------------------------------------------------
% телепортировать игрока в указанные координаты
player_teleport(#player_state{accountid=AccountID, grids=Grids,
							  visible_client=VisibleClient, send_pid=SendPid} = State,
				X,Y,Lv) ->
	% удаляем из клиента все видимые объекты
	lists:foreach(fun (#visible_obj{id=ObjID}) ->
					   player_net:send_obj_remove(SendPid, ObjID)
			end, VisibleClient),
	% отключаемся от гридов
	lists:foreach(fun (#player_grid{pid=GP}) ->
						   GP ! {deactivate, self()}
				  end, Grids),

	% спавним заново гриды в новые координаты
	NewGrids = get_wait_grids(X,Y,Lv),
	P = get_work_grid(X,Y,Lv, NewGrids),
	O = get_player_object(State),
	% телепортимся без учета коллизий (временно)
	case try_request_move(P, {player_add, O#object{coord={X,Y,Lv}}, AccountID, self(), false}, 1) of
		{ack_player_add_ok} ->
			% запросим данные у гридов
			lists:foreach(fun(#player_grid{pid=PP}) ->
				  PP ! {get_state, self()},
				  PP ! {get_claims, self()}
			end, NewGrids);
		{ack_player_add_fail} -> exit(fail_teleport)
	end,

    ?DEBUG("grid pid ~p", [P]),

	NewState = State#player_state{
                                        state = {stay,none},
                                        coord = {X,Y,Lv},
                                        visible_last_coord = {0,0,none},
                                        visible_client = [],
										visible_buffer = [],
										visible_list = [],
										visible_timer = 0,
                                        visible_zone = {1,1,0,0},
										grids = NewGrids
                                       },
    player_moved(NewState).

%-----------------------------------------------------------------------------
% выполнить админ команду введенную в консоли
execute_command(#player_state{charid=CharID, name=Name, coord={_,_,Lv}=MyC, send_pid=SendPid} = State, Msg) ->
	[C|Params] = string:tokens(Msg, " "),
	Command = list_to_atom(C),
	ISpawn = fun() ->
					[SType, SQ] = Params,
					{Q,_} = string:to_integer(SQ),
					Type = list_to_atom(SType),
					?DEBUG("admin spawn item type=~p q=~p",[Type,Q]),
					player_spawn_new_item(State, Type, Q)
			 end,

	?DEBUG("command ~p params ~p",[Command, Params]),
	case Command of
		tp -> % телепорт в указанные координаты. параметры х_координата у_координата
			[CX,CY] = Params,
			{X,_} = string:to_integer(CX),
			{Y,_} = string:to_integer(CY),
			?DEBUG("admin teleport to ~p ~p",[X,Y]),
			player_teleport(State,X,Y,Lv);
		tp_char -> % телепорт указанного чара в указанные координаты
			[CName, CX, CY] = Params,
			{X,_} = string:to_integer(CX),
			{Y,_} = string:to_integer(CY),
			?DEBUG("admin teleport to ~p ~p",[X,Y]),
			?MODULE ! {get_players_list, self()},
			receive
				{get_players_list_ack, Players_List} ->
					lists:foreach(fun
									 (#player_server_rec{pid=PP}) -> PP ! {admin_teleport_to, CName, X, Y, Lv} 
								  end, Players_List)	
			after 500 ->
					error(timeout_get_players_list_ack)
			end,
			State;
		ispawn -> ISpawn();
		sitem -> ISpawn();
		sg ->
			{Sg, Grid} = map_server:get_sg_grid(MyC),
			player_net:send_system_say(SendPid, io_lib:format("sg=~p grid=~p",[Sg,Grid])),
			State;
		who -> % узнать кто онлайн и их координаты. параметров нет
			?MODULE ! {get_players_list, self()},
			receive 
				{get_players_list_ack, List} ->
					?DEBUG("get_players_list_ack ~p",[List]),
					% запрашиваем у всех игроков данные
					lists:foreach(fun
									 (#player_server_rec{pid=PP}) ->
										  PP ! {who, self()}
								  end, List),
					Num = receive_who_list(SendPid, 0),
					player_net:send_system_say(SendPid, io_lib:format("TOTAL: ~p",[Num]))
			after 1000 ->
					error(timeout_get_players_list)
			end,
			State;
		spawn ->
			[ObjType] = Params,
			player_set_place_obj(State, {list_to_atom(ObjType), 0,spawn});
		test_error ->
			get_work_grid(State) ! {test_error},
			State;
		
		_ ->
			?WARNING_MSG("char id=~p name=~p unknown admin command: ~p",[CharID,Name,Command]),
			State
	end.

%-----------------------------------------------------------------------------
receive_who_list(SendPid, Acc) ->
	receive 
		{who, _} -> receive_who_list(SendPid, Acc+1);
		{who_ack, #player_state{coord={XP, YP, LvP}, name=Name}} ->
			player_net:send_system_say(SendPid, io_lib:format("~p x=~p y=~p lv=~p", [Name, XP, YP, LvP])),
			receive_who_list(SendPid, Acc+1)
	after 300 -> Acc
	end.
			

%####################################################################################################
% UTILS #############################################################################################
%####################################################################################################

dialog_opened(#player_state{send_pid=SendPid, skills=Skills}, N) when N == "dlg_stat" ->
	player_net:send_all_skills(SendPid, Skills);
dialog_opened(_,_) -> ok.


%-----------------------------------------------------------------------------
% return new list of visible object
clip_visible(SendPid, NewVisibleZone, OldVisibleObjects) ->
    clip_visible(SendPid, NewVisibleZone, OldVisibleObjects, []).
clip_visible(_Socket, _NewVisibleZone, [], Acc) -> Acc;
clip_visible(SendPid, NewVisibleZone, OldVisibleObjects, Acc) ->
    [#visible_obj{x=X, y=Y, id=ObjID} = VO|T] = OldVisibleObjects,
%%     ?DEBUG("clip_visible list ~p rect ~p", [OldVisibleObjects,NewVisibleZone]),
    case get_in_rect({X,Y},NewVisibleZone) of
        true ->
%%             ?DEBUG("obj in visible zone ~p", [ObjID]),
            clip_visible(SendPid, NewVisibleZone, T, [VO|Acc]);
        false ->
%%             ?DEBUG("obj NOT in visible zone ~p", [ObjID]),
			case SendPid of
				none -> ok;
				_ ->
					player_net:send_obj_remove(SendPid, ObjID)
			end,
            clip_visible(SendPid,NewVisibleZone, T, Acc)
    end.

%-----------------------------------------------------------------------------
% запросить новые гриды, деактивировать старые из которых вышли
merge_map_grids([], _OldMapGrids, [], [], Acc) ->
    Acc;
merge_map_grids([], OldMapGrids, WaitAcc, PidAcc, Acc) ->
	wait_grids(WaitAcc),
	lists:foreach(fun(P) ->
						  P ! {get_state, self()},
						  P ! {get_claims, self()}
				  end, PidAcc),
	merge_map_grids([], OldMapGrids, [], [], Acc);
merge_map_grids([GC|T], OldMapGrids, WaitAcc, PidAcc, Acc) ->
    case case lists:keyfind(GC, #player_grid.coord, OldMapGrids) of
        false ->
			% такого грида в старых нет - запросим
			P = world_server:get_grid(GC),
			% активируем грид
			P ! {activate, self()},
			
            {#player_grid{coord=GC, pid=P}, GC, P};
		G ->
			% такой грид уже есть и загружен просто его вернем
			{G, none, none}
    end of
		{Elem, none, none} -> merge_map_grids(T, OldMapGrids, WaitAcc, PidAcc, [Elem|Acc]);
		{Elem, GCC, Pid} -> merge_map_grids(T, OldMapGrids, [GCC|WaitAcc], [Pid|PidAcc], [Elem|Acc])
	end.

%-----------------------------------------------------------------------------------
% need update visible?
check_visible_moved({X1,Y1,Lv1}, {X2,Y2,Lv2}) ->
%%     ?DEBUG("check_visible_moved ~p ~p ~p   ~p ~p ~p", [X1,Y1,Lv1, X2,Y2,Lv2]),
    if Lv1 =/= Lv2 -> true;
    true ->
            Distance = math:sqrt( (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1) ),
            if Distance > ?PLAYER_VISIBLE_UPDATE -> true; true -> false
            end
    end.

%-----------------------------------------------------------------------------------
% get socket from player state
get_send_pid(State) ->
    #player_state{ send_pid=SendPid } = State,
    SendPid.

%---------------------------------------------------------------------------

is_moving(MoveState) when (MoveState == move_point) or (MoveState == move_object) or (MoveState == move_follow)
         or (MoveState == move_virtual_object) -> true;
is_moving(_) -> false.

get_action_name(Action) when is_atom(Action) -> Action;
get_action_name(Action) ->
	case Action of
		{player, Name, _} -> Name;
		_ -> 
			if 
				is_tuple(Action) -> element(1, Action);
				true -> unknown
			end
	end.

% return visible zone
get_visible_zone(X,Y) ->
    {X-?PLAYER_VISIBLE_DISTANCE_RECT,
     Y-?PLAYER_VISIBLE_DISTANCE_RECT,
     X+?PLAYER_VISIBLE_DISTANCE_RECT,
     Y+?PLAYER_VISIBLE_DISTANCE_RECT}.

% return classic msecs now time
get_tick_count() ->
    {_,S,M} = now(),
    (S*1000)+(M div 1000).

% get point distance
get_distance(X1,Y1,X2,Y2) ->
    math:sqrt( (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1) ).

get_in_rect({X,Y}, {L,T,R,B}) when (X >= L) and (X =< R) and (Y >= T) and (Y =< B) -> true;
get_in_rect(_, _) -> false.

get_stealth_params(Params) ->
	case lists:keyfind(stealth, 1, Params) of
		false -> 0;
		{_, S} -> S
	end.

% ждем загрузки всех гридов.
wait_grids([]) -> ok;
wait_grids(Grids) ->
	?DEBUG("wait_session_grids ~p",[Grids]),
	receive
		{activated, GC} ->
			?DEBUG("grid activated, ~p -----------------------------------------------------",[GC]),
			wait_grids(lists:delete(GC, Grids))
	after 5000 -> exit(timeout_wait_grids)
	end.


send_all_grids([], _) -> ok;
send_all_grids([#player_grid{pid=P}|T], M) -> P ! M, send_all_grids(T, M).

% отправить гридам сообщение и дождатся ответа #player_grid
send_grid_msg_ack(Grids, M) ->
	List = lists:map(fun (#player_grid{pid=P, coord=G}) -> P ! M, G end, Grids),
	wait_grids_ack(List).

% ждем ответа от грида на посланное сообщение
wait_grids_ack([]) ->
	?DEBUG("wait_grids_ack : none"),
	none;
wait_grids_ack(Grids) ->
	%?DEBUG("wait_grids_ack ~p",[Grids]),
	receive
		% ответ получен, объект найден
		{ack, M} -> M;
		% нет ответа, или не найден
		{none, GC} -> wait_grids_ack(lists:delete(GC, Grids))
	after 5000 -> exit(timeout_wait_grids_ack)
	end.

% получить центральный грид
get_grid(#player_state{grids=Grids, coord={_,_,Lv}}, X, Y) ->
	get_work_grid(X,Y,Lv, Grids).
% получить рабочий грид в котором находится игрок
get_work_grid(X,Y,Lv, List) -> get_work_grid({X,Y,Lv}, List).
get_work_grid({X,Y,Lv}, List) ->
	GC = map_server:get_sg_grid(X, Y, Lv),
	case lists:keyfind(GC, #player_grid.coord, List) of
		false -> error(get_work_grid);
		#player_grid{pid=P} -> P
	end;
get_work_grid({Sg, Grid}, List) ->
	case lists:keyfind({Sg, Grid}, #player_grid.coord, List) of
		false -> error(get_work_grid);
		#player_grid{pid=P} -> P
	end.
get_work_grid(#player_state{grids=Grids, coord=CC}) ->
	get_work_grid(CC, Grids).


% попытка обсчитать движение
try_request_move(_P, _Msg, 0) -> error(try_request_move_fail);
try_request_move(P, Msg, Count) ->
	P ! Msg,
	receive
		{fail_retry} ->
			sleep(randoms:get(100+10)),
			try_request_move(P, Msg, Count-1);

		{no_collision} -> {no_collision};
		{collision_tile, XX, YY, T} -> {collision_tile, XX, YY, T};
 	    {collision_virtual, XX, YY} -> {collision_virtual, XX, YY};
 	    {collision_obj, XX, YY, O, OI} -> {collision_obj, XX, YY, O, OI};


		{ack_player_add_ok} -> {ack_player_add_ok};
		{ack_player_add_fail} -> {ack_player_add_fail}
	after 2000 -> error(timeout_try_request_move)
	end.

grid_get_tile(Grids, X, Y) ->
	send_grid_msg_ack(Grids, {get_tile, self(), X, Y}).

rect_from_bound(X,Y,{L,T,R,B}) ->
    {X+L,Y+T,X+R,Y+B}.

% пересекаются ли прямоугольники
is_rect_intersect({L1,T1,R1,B1},{L2,T2,R2,B2}) when
  (((L1 > L2) and (L1 =< R2)) or ((R1 > L2) and (R1 =< R2)) or
  ((L2 > L1)  and (L2 =< R1)) or ((R2 > L1) and (R2 =< R1))) and
  (((T1 > T2) and (T1 =< B2)) or ((B1 > T2) and (B1 =< B2)) or
  ((T2 > T1)  and (T2 =< B1)) or ((B2 > T1) and (B2 =< B1))) ->
	true;
is_rect_intersect(_,_) -> false.


lasterror(R) ->
    ?CRITICAL_MSG("die player process ~p", [R]),
    exit({lasterror, R}).