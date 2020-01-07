%% Author: arksu
%% Created: 25.06.2011
%% Description: TODO: Add description to build
-module(build).

-export([]).
-compile(export_all).
-include("defines.hrl").
-include("base_io.hrl").
-include("map.hrl").
-include("types.hrl").
-include("objects.hrl").

-define(BUILD_SLOT_HEIGHT, 40).

%% --------------------------------------------------------------------
% build slots:
%   {Type, Current, Max, ItemsList} - slot tuple
%   object state: [Slot] - list of slots

%% --------------------------------------------------------------------
% MAIN PARAMS

get_def() ->
	[
	 %													context time	required
	 {box, 					build_box, 					false, 	5000, 	[{drawing_box, item}, {billet, 5}] },
	 
	 {corner_home_wood, 	build1_corner_home_wood, 	true, 	3000, 	[{board, 2}, {billet, 4}] },
	 {wally_home_wood, 		build1_wally_home_wood, 	true, 	3000, 	[{board, 1}, {billet, 4}] },
	 {wallx_home_wood, 		build1_wallx_home_wood, 	true, 	3000, 	[{board, 1}, {billet, 4}] },
	 {wallwindowy_home_wood,build1_wallwindowy_home_wood,true, 	3000, 	[{board, 1}, {billet, 4}] },
	 {wallwindowx_home_wood,build1_wallwindowx_home_wood,true, 	3000, 	[{board, 1}, {billet, 4}] },
	 {walldoory_home_wood,	build1_walldoory_home_wood,	true, 	3000, 	[{board, 1}, {billet, 4}] },
	 {walldoorx_home_wood,	build1_walldoorx_home_wood,	true, 	3000, 	[{board, 1}, {billet, 4}] },

	 {corner_fence, 		build_corner_fence, 		true, 	3000, 	[{billet, 1}] },
	 {wallx_fence, 			build_wallx_fence, 			true, 	3000, 	[{billet, 1}] },
	 {wally_fence, 			build_wally_fence, 			true, 	3000, 	[{billet, 1}] },
	 {gatex_fence, 			build_gatex_fence, 			false, 	3000, 	[{billet, 1}] },
	 {gatey_fence, 			build_gatey_fence, 			false, 	3000, 	[{billet, 1}] },

	 {chair, 				build_chair, 				false, 	5000, 	[{board, 1}, {billet, 2}, {branch, 2}] },
	 {claim, 				build_claim, 				false, 	1000, 	[{rabbit_catched, 1}, {stone_piece, 10}, {billet, 2}]   },
	 {bonfire, 				build_bonfire, 				false, 	3000, 	[{stone_piece, 8}] },
	 {jar, 					build_jar, 					false, 	2000, 	[{clay, 15}] }
	 ].

% это стройка (строящийся объект)
is_build(Type) ->
	case lists:keyfind(Type, 2, get_def()) of
		false -> false;
		_ -> true
	end.


% это строение (уже построеный объект)
is_building(Type) ->
	case get_build_type(Type) of
		unknown -> false;
		_ -> true
	end.

is_context_have(Type) ->
	case lists:keyfind(Type, 1, get_def()) of
		false -> false;
		Tuple -> element(3, Tuple)
	end.

% generate object params for spawn
generate_state(Type, Item) ->
case lists:keyfind(Type, 2, get_def()) of
		false -> [];
		Tuple -> 
			List = element(5, Tuple),
			generate_slots(Item, List, [])
	end.    
generate_slots(_, [], Acc) -> Acc;
generate_slots(Item, [H|T], Acc) ->
	case H of
		{Type, item} -> generate_slots(Item, T, [create_slot(Type, 1, [Item]) 	| Acc]);
		{Type, Count} -> generate_slots(Item, T, [create_slot(Type, Count) 		| Acc])
	end.

% сколько времени строится одна вещь из слотов
get_ticks(Type) ->
 	case lists:keyfind(Type, 2, get_def()) of
		false -> 20000;
		Tuple -> element(4, Tuple)
	end.

% что получится из стройки
get_final_type(Type) ->
 	case lists:keyfind(Type, 2, get_def()) of
		false -> unknown;
		Tuple -> element(1, Tuple)
	end.

%% --------------------------------------------------------------------
% get object type for building
get_build_type(PlaceType) ->
 	case lists:keyfind(PlaceType, 1, get_def()) of
		false -> unknown;
		Tuple -> element(2, Tuple)
	end.


%% --------------------------------------------------------------------
% получить контекстное меню для построенного объекта (стены)
get_context(State, #object{type=Type, coord={X, Y, _}} = O) ->
	?DEBUG("get context ~p", [O]),
	case Type of
		corner_home_wood ->
			% проверим возможные направления строительства
			% выдадим только то куда можно строить
			% build1_corner_home_wood - проверяем этим типом возможные направления стройки т.к. он всегда дает коллизию,
			% ведь нам просто надо узнать дает ли коллизию объект в указанном направлении
			{E,S,W,N} = get_build_dirs(State, build1_corner_home_wood, X, Y),
			L1 = if E==true -> ["wall_e"]; 		true -> [] end,
			L2 = if S==true -> ["wall_s"]; 		true -> [] end,
			L3 = if W==true -> ["wall_w"]; 		true -> [] end,
			L4 = if N==true -> ["wall_n"]; 		true -> [] end,

			lists:append([L1, L2, L3, L4]);

		wally_home_wood ->
			[S,N] = get_build_dirs(State, build1_corner_home_wood, X, Y, [south, north], []),
			?DEBUG("wally_home_wood ~p ~p", [S,N]),
			L1 = if S==true -> ["wallwindow_s", "walldoor_s", "wall_s", "corner_s"]; 	true -> [] end,
			L2 = if N==true -> ["wallwindow_n", "walldoor_n", "wall_n", "corner_n"]; 	true -> [] end,
			lists:append([L1, L2]);
		wallwindowy_home_wood ->
			[S,N] = get_build_dirs(State, build1_corner_home_wood, X, Y, [south, north], []),
			?DEBUG("wally_home_wood ~p ~p", [S,N]),
			L1 = if S==true -> ["wall_s"]; 	true -> [] end,
			L2 = if N==true -> ["wall_n"]; 	true -> [] end,
			lists:append([L1, L2]);
		walldoory_home_wood ->
			[S,N] = get_build_dirs(State, build1_corner_home_wood, X, Y, [south, north], []),
			?DEBUG("wally_home_wood ~p ~p", [S,N]),
			L1 = if S==true -> ["wall_s"]; 	true -> [] end,
			L2 = if N==true -> ["wall_n"]; 	true -> [] end,
			lists:append([L1, L2]);

		wallx_home_wood ->
			[E,W] = get_build_dirs(State, build1_corner_home_wood, X, Y, [east, west], []),
			?DEBUG("wallx_home_wood ~p ~p", [W,W]),
			L1 = if E==true -> ["wallwindow_e", "walldoor_e", "wall_e", "corner_e"]; 	true -> [] end,
			L2 = if W==true -> ["wallwindow_w", "walldoor_w", "wall_w", "corner_w"]; 	true -> [] end,
			lists:append([L1, L2]);
		wallwindowx_home_wood ->
			[E,W] = get_build_dirs(State, build1_corner_home_wood, X, Y, [east, west], []),
			?DEBUG("wallx_home_wood ~p ~p", [W,W]),
			L1 = if E==true -> ["wall_e"]; 	true -> [] end,
			L2 = if W==true -> ["wall_w"]; 	true -> [] end,
			lists:append([L1, L2]);
		walldoorx_home_wood ->
			[E,W] = get_build_dirs(State, build1_corner_home_wood, X, Y, [east, west], []),
			?DEBUG("wallx_home_wood ~p ~p", [W,W]),
			L1 = if E==true -> ["wall_e"]; 	true -> [] end,
			L2 = if W==true -> ["wall_w"]; 	true -> [] end,
			lists:append([L1, L2]);
		
		%$$$$$$$$$$$$$$$$$$$$$$$$
		corner_fence ->
			{E,S,W,N} = get_build_dirs(State, build_corner_fence, X, Y),
			L1 = if E==true -> ["wall_e"]; 	true -> [] end,
			L2 = if S==true -> ["wall_s"]; 	true -> [] end,
			L3 = if W==true -> ["wall_w"]; 	true -> [] end,
			L4 = if N==true -> ["wall_n"];	true -> [] end,
			[GE,GW] = get_build_dirs(State, build_gatex_fence, X, Y, {17, 0, 17, 0}, [east, west], []),
			LG1 = if GE==true -> ["gate_e"]; 	true -> [] end,
			LG2 = if GW==true -> ["gate_w"]; 	true -> [] end,
			[GS,GN] = get_build_dirs(State, build_gatey_fence, X, Y, {0, 17, 0, 17}, [south, north], []),
			LG3 = if GS==true -> ["gate_s"]; 	true -> [] end,
			LG4 = if GN==true -> ["gate_n"]; 	true -> [] end,
			lists:append([L1, L2, L3, L4,     LG1, LG2, LG3, LG4]);
		wallx_fence ->
			[E,W] = get_build_dirs(State, build_corner_fence, X, Y, [east, west], []),
			L1 = if E==true -> ["wall_e", "corner_e"]; 	true -> [] end,
			L2 = if W==true -> ["wall_w", "corner_w"]; 	true -> [] end,
			[GE,GW] = get_build_dirs(State, build_gatex_fence, X, Y, {17, 0, 17, 0}, [east, west], []),
			LG1 = if GE==true -> ["gate_e"]; 	true -> [] end,
			LG2 = if GW==true -> ["gate_w"]; 	true -> [] end,
			lists:append([L1, L2,     LG1, LG2]);
		wally_fence ->
			[S,N] = get_build_dirs(State, build_corner_fence, X, Y, [south, north], []),
			L1 = if S==true -> ["wall_s", "corner_s"]; 	true -> [] end,
			L2 = if N==true -> ["wall_n", "corner_n"]; 	true -> [] end,
			[GS,GN] = get_build_dirs(State, build_gatey_fence, X, Y, {0, 17, 0, 17}, [south, north], []),
			LG3 = if GS==true -> ["gate_s"]; 	true -> [] end,
			LG4 = if GN==true -> ["gate_n"]; 	true -> [] end,
			lists:append([L1, L2,       LG3, LG4]);
		

		_ -> []
	end.

%% --------------------------------------------------------------------
% обработать выбор игрока в контекстном меню
context_action(_, #object{type=Type, coord={X,Y,_}}, Action) ->
	CA =
	case Type of
		corner_home_wood -> case Action of
			"wall_e" -> 			{wallx_home_wood, 1, 0};
			"wall_s" -> 		{wally_home_wood, 0, 1};
			"wall_w" -> 			{wallx_home_wood, -1, 0};
			"wall_n" -> 		{wally_home_wood, 0, -1};
			_ -> none
			end;

		wally_home_wood -> case Action of
			"wall_s" -> 		{wally_home_wood,  		0, 1};
			"wall_n" -> 		{wally_home_wood, 		0, -1};
			"wallwindow_s" -> 	{wallwindowy_home_wood, 0, 1};
			"wallwindow_n" -> 	{wallwindowy_home_wood, 0, -1};
			"walldoor_s" -> 	{walldoory_home_wood, 	0, 1};
			"walldoor_n" -> 	{walldoory_home_wood, 	0, -1};
			"corner_s" -> 		{corner_home_wood, 		0, 1};
			"corner_n" -> 		{corner_home_wood, 		0, -1};
			_ -> none
			end;
		wallwindowy_home_wood -> case Action of
			"wall_s" -> 		{wally_home_wood, 0, 1};
			"wall_n" -> 		{wally_home_wood, 0, -1};
%% 			"wallwindow_s" -> 	{build, wallwindowy_home_wood, none, X, Y+?TILE_SIZE};
%% 			"wallwindow_n" -> 	{build, wallwindowy_home_wood, none, X, Y-?TILE_SIZE};
%% 			"walldoor_s" -> 	{build, walldoory_home_wood, none, X, Y+?TILE_SIZE};
%% 			"walldoor_n" -> 	{build, walldoory_home_wood, none, X, Y-?TILE_SIZE};
			_ -> none
			end;
		walldoory_home_wood -> case Action of
			"wall_s" -> 		{wally_home_wood, 0, 1};
			"wall_n" -> 		{wally_home_wood, 0, -1};
%% 			"wallwindow_s" -> 	{build, wallwindowy_home_wood, none, X, Y+?TILE_SIZE};
%% 			"wallwindow_n" -> 	{build, wallwindowy_home_wood, none, X, Y-?TILE_SIZE};
%% 			"walldoor_s" -> 	{build, walldoory_home_wood, none, X, Y+?TILE_SIZE};
%% 			"walldoor_n" -> 	{build, walldoory_home_wood, none, X, Y-?TILE_SIZE};
			_ -> none
			end;

		wallx_home_wood -> case Action of
			"wall_e" -> 			{wallx_home_wood, 		1, 0};
			"wall_w" -> 			{wallx_home_wood, 		-1, 0};
			"wallwindow_e" -> 	{wallwindowx_home_wood, 1, 0};
			"wallwindow_w" -> 	{wallwindowx_home_wood, -1, 0};
			"walldoor_e" -> 		{walldoorx_home_wood, 	1, 0};
			"walldoor_w" -> 		{walldoorx_home_wood, 	-1, 0};
			"corner_e" -> 		{corner_home_wood, 		1, 0};
			"corner_w" -> 		{corner_home_wood, 		-1, 0};
			_ -> none
			end;
		wallwindowx_home_wood -> case Action of
			"wall_e" -> 			{wallx_home_wood, 1, 0};
			"wall_w" -> 			{wallx_home_wood, -1, 0};
			_ -> none
			end;
		walldoorx_home_wood -> case Action of
			"wall_e" -> 			{wallx_home_wood, 1, 0};
			"wall_w" -> 			{wallx_home_wood, -1, 0};
			_ -> none
			end;

		%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
		corner_fence -> case Action of
			"wall_e" -> 			{wallx_fence, 1, 0};
			"wall_s" -> 			{wally_fence, 0, 1};
			"wall_w" -> 			{wallx_fence, -1, 0};
			"wall_n" -> 			{wally_fence, 0, -1};
			"gate_e" -> 			{gatex_fence, ex, 16, 0};
			"gate_s" -> 			{gatey_fence, ex, 0, 16};
			"gate_w" -> 			{gatex_fence, ex, -16, 0};
			"gate_n" -> 			{gatey_fence, ex, 0, -16};
			_ -> none
			end;
		wallx_fence -> case Action of
			"wall_e" -> 			{wallx_fence, 1, 0};
			"wall_w" -> 			{wallx_fence, -1, 0};
			"corner_e" -> 			{corner_fence, 1, 0};
			"corner_w" -> 			{corner_fence, -1, 0};
			"gate_e" -> 			{gatex_fence, ex, 16, 0};
			"gate_w" -> 			{gatex_fence, ex, -16, 0};
			_ -> none
			end;
		wally_fence -> case Action of
			"wall_s" -> 			{wally_fence, 0, 1};
			"wall_n" -> 			{wally_fence, 0, -1};
			"corner_s" -> 			{corner_fence, 0, 1};
			"corner_n" -> 			{corner_fence, 0, -1};
			"gate_s" -> 			{gatey_fence, ex, 0, 16};
			"gate_n" -> 			{gatey_fence, ex, 0, -16};
			_ -> none
			end;

		_ -> none
	end,
	case CA of
		{CAType, CX, CY} ->
			{build, CAType, none, X+?TILE_SIZE*CX, Y+?TILE_SIZE*CY};
		{CAType, ex, CX, CY} ->
			{build, CAType, none, X+CX, Y+CY};
		_ -> none
	end.


%##############################################################################################################
% SYSTEM
%##############################################################################################################

%% --------------------------------------------------------------------
% получить направления для стройки
get_build_dirs(State, Type, X, Y) ->
	get_build_dirs(State, Type, X, Y, {?TILE_SIZE, ?TILE_SIZE, ?TILE_SIZE, ?TILE_SIZE}).
get_build_dirs(State, Type, X, Y, Bounds) ->
	E = check_build_direction(State, Type, X, Y, Bounds, east),
	S = check_build_direction(State, Type, X, Y, Bounds, south),
	W = check_build_direction(State, Type, X, Y, Bounds, west),
	N = check_build_direction(State, Type, X, Y, Bounds, north),
	{E,S,W,N}.

get_build_dirs(State, Type, X, Y, L, Acc) ->
	get_build_dirs(State, Type, X, Y, {?TILE_SIZE, ?TILE_SIZE, ?TILE_SIZE, ?TILE_SIZE}, L, Acc).
get_build_dirs(_State, _Type, _X, _Y, _Bounds, [], Acc) -> lists:reverse(Acc);
get_build_dirs(State, Type, X, Y, Bounds, [H|T], Acc) ->
	get_build_dirs(State, Type, X, Y, Bounds, T,
				   [check_build_direction(State, Type, X, Y, Bounds, H) | Acc]
				  ).


%% --------------------------------------------------------------------
% проверить направление для стройки
check_build_direction(State, Type, X, Y, {TE, TS, TW, TN}, Dir) ->
	{AddX, AddY} = case Dir of
		east -> {TE, 0};
		south -> {0, TS};
		west -> {-TW, 0};
		north -> {0, -TN}
	end,
	
	CX = X+AddX, CY = Y + AddY,

	case grid:check_collision(State, Type, CX, CY, ?MOVE_LAND) of
				{no_collision} -> true;
				% любая коллизия - нельзя строить
				_ -> false
	end.


%% --------------------------------------------------------------------
% if build object empty - return true, else = false
% используется при закрытии объекта. если пустой - уничтожится
is_empty(#object{state=Slots, type=Type}) ->
	case Type of
%% 		build1_wally_home_wood -> false;
%% 		build1_wallx_home_wood -> false;
		_ -> is_empty_slots(Slots)
	end.

is_empty_slots([]) -> true;
is_empty_slots([H|T]) ->
	{_, _, _, ItemsList} = H,
	if ItemsList == [] -> is_empty_slots(T);
	   true -> false
	end.

%% -------------------------------------------------------------------------------------------------------------
% build object. if succees need change object type
% send msg to player, return new grid state
% call from GRID
building_object(State, PlayerPid, #player_state{charid=CharID}, Object) ->
    % get slots list
    #object{id=ObjID, type=ObjType, coord=Coord, state=Slots } = Object,
    % process build
    {NewSlots, Result} = building_slots(Slots),
	?DEBUG("BUILD result=~p newslots=~p",[Result,NewSlots]),
    case Result of
        success ->
            ?DEBUG("BUILD: building success! ~p",[ObjType]),
            PlayerPid ! {ack, {action_end}},
			PlayerPid ! {gain_exp, #exp{industry=15,nature=2}},
            % change build to final object
			% создаем объект
            O = #object{id=ObjID, type=get_final_type(ObjType), coord=Coord},
			% заполняем его состояние
			O1 = case ObjType of
				build_claim -> objects_server:object_fill(O#object{state=#obj_claim{owner_id=CharID}});
				_ -> objects_server:object_fill(O)
			end,
            % изменяем объект стройки на вновь созданный
            S1 = grid:object_changed(State, ObjID, Coord, O1),
			% удаляем параметр "открыт"
            grid:object_remove_param_notify(S1, ObjID, opened);
        continue ->
            PlayerPid ! {ack, {action_continue, build, get_ticks(ObjType)}},
			PlayerPid ! {gain_exp, #exp{industry=10,nature=2}},
            NewObject = Object#object{state=NewSlots },
            % change object
%% 			grid:send_all_notify(State#grid_state.notify, {obj_visual_state, NewObject}),
            grid:object_changed(State, ObjID, Coord, NewObject);
        end_material ->
            PlayerPid ! {ack, {action_end}},
            NewObject = Object#object{state=NewSlots },
            % change object
%% 			grid:send_all_notify(State#grid_state.notify, {obj_visual_state, NewObject}),
            grid:object_changed(State, ObjID, Coord, NewObject);

        false ->
            PlayerPid ! {ack, {action_end}},
            State
    end.

%% -------------------------------------------------------------------------------------------------------------
can_build(Slots) ->
    case get_work_slot(Slots, 1) of
        none -> false;
        _ -> true
    end.

%% -------------------------------------------------------------------------------------------------------------
% calc 1 build action
% return {NewSlots, Result}
building_slots(Slots) ->
    case get_work_slot(Slots, 1) of
        none -> {Slots, false}; % ADMIN!!
        {Slot, _Num} ->
            {Type, Current, Max, ItemsList} = Slot,
            NewSlots = replace_slot(Type, {Type, Current+1, Max, ItemsList}, Slots, []),
%%                 lists:keyreplace(Type, 1, Slots, {Type, Current+1, Max, ItemsList}),
            All = all_builded(NewSlots),
            if (All == true) ->
                {NewSlots, success};
            true ->
                case get_work_slot(NewSlots, 1) of
                    none -> {NewSlots, end_material};
                    _ -> {NewSlots, continue}
                end
            end
    end.

replace_slot(_Type, _NewSlot, [], Acc) -> lists:reverse(Acc);
replace_slot(Type, NewSlot, Slots, Acc) ->
    [Slot|T] = Slots,
    {SlotType, _Current, _Max, _ItemsList} = Slot,
    if (SlotType == Type) ->
        replace_slot(Type, NewSlot, T, [NewSlot|Acc]);
    true ->
        replace_slot(Type, NewSlot, T, [Slot|Acc])
    end.

all_builded([]) -> true;
all_builded(Slots) ->
    [H|T] = Slots,
    {_, Current, Max, _Items} = H,
    if (Current >= Max) -> all_builded(T);
       true -> false
    end.

% return none | {Slot, Num}
get_work_slot([],_) -> none;
get_work_slot(Slots, Num) ->
    [H|T] = Slots,
    {_, Current, Max, Items} = H,
    if (Current < Max) andalso (Current < length(Items)) ->
        {H, Num};
    true ->
        get_work_slot(T, Num+1)
    end.

slot_click(State, N, Object, PlayerPid, PlayerState) ->
	PlayerPid ! {ack, {action_end}},
	#player_state{item_hand=Hand} = PlayerState,
	% get slots list
    #object{id=ObjID, coord=Coord, state=Slots } = Object,
    % get slot type
	case lists:sublist(Slots, N, 1) of
		[] ->
			?DEBUG("slot_click : no slot by order"),
			State;
		[{SlotType, Current, Max, ItemsList}] ->
			?DEBUG("slot_click : ~p",[SlotType]),
            % can put item hand to slot
            case objects_server:check_slot_item(SlotType, Hand) of
            true ->
                if (length(ItemsList) < Max) ->
                    #item{id=ItemObjID} = Hand,
                    % clear player hands
                    db_server:inventory_remove(ItemObjID),
                    PlayerPid ! {set_hand, none, 10, 10},
                    % add to items
                    NewItems = [Hand|ItemsList],
                    % refresh state
                    NewObjState = replace_slot(SlotType, {SlotType, Current, Max, NewItems}, Slots, []),
                    NewObject = Object#object{state=NewObjState },

%% 					grid:send_all_notify(State#grid_state.notify, {obj_visual_state, NewObject}),
							
					grid:object_changed(State, ObjID, Coord, NewObject);
                true ->
                    State
                end;
            false -> State
            end
	end.

%% -------------------------------------------------------------------------------------------------------------
% по слоту покрутили колесиком. надо положить подходящие материалы
slot_wheel(State, N, Object, PlayerPid, PlayerState) ->
	
	#player_state{inventory=Inventory} = PlayerState,
	% get slots list
    #object{id=ObjID, coord=Coord, state=Slots } = Object,
    % get slot type
	case lists:sublist(Slots, N, 1) of
		[] ->
			?DEBUG("slot_wheel : no slot by order"),
			PlayerPid ! {ack, {action_end}},
			State;
		[{SlotType, Current, Max, ItemsList}] ->
			?DEBUG("slot_wheel : ~p",[SlotType]),
			% если еще нужен материал в слот
			if (length(ItemsList) < Max) ->
				case inventory:take_items(Inventory, SlotType, 1, []) of
					false ->
						PlayerPid ! {ack, {action_end}},
						State;
					{[Taked], NewInventory} ->
						% удаляем из базы что взяли
						inventory:destroy_items(Taked),
						% вернем игроку новый стейт с обновленным инвентарем, откуда взяли в слот 1 вещь
						PlayerPid ! {ack, {new_state, player_server:player_update_inventory(PlayerState, NewInventory)} },
						
						% add to items
	                    NewItems = [Taked|ItemsList],
	                    % refresh state
	                    NewObjState = replace_slot(SlotType, {SlotType, Current, Max, NewItems}, Slots, []),
	                    NewObject = Object#object{state=NewObjState },
						
						grid:object_changed(State, ObjID, Coord, NewObject)
				end;
			true ->
				PlayerPid ! {ack, {action_end}},
				State
			end
	end.

%% -------------------------------------------------------------------------------------------------------------
% fill slot
create_slot(Type, Max) -> {Type, 0, Max, []}.
create_slot(Type, Max, Items) -> {Type, 0, Max, Items}.

make_slot_pkt([], Acc) -> Acc;
make_slot_pkt([H|T], Acc) -> make_slot_pkt(T, [make_slot_pkt(H)|Acc]).
make_slot_pkt({Type, Count, Max, Items}) ->
	[
	 write_string(objects_server:slot_image_name(Type)),
	 write_word(Max - length(Items)),
	 write_word(length(Items) - Count),
	 write_word(Count)
	].
	

%% -------------------------------------------------------------------------------------------------------------
save_db_data(Slots) ->
    A = [ 
         write_int(length(Slots)),
         save_db_slots(Slots,[])
        ],
    list_to_binary(A).

save_db_slots([],Acc) -> Acc;
save_db_slots(Slots, Acc) ->
    [H|T] = Slots,
    save_db_slots(T,[save_slot(H)|Acc]).
save_slot({Type, Current, Max, ItemsList}) ->
    [
     write_int(objects_server:get_typeid(Type)),
     write_int(Current),
     write_int(Max),
     write_int(length(ItemsList)),
     inventory:save_db_items(ItemsList,[])
    ].

%% -------------------------------------------------------------------------------------------------------------
load_db_data(<<>>) -> [];
load_db_data(Data) ->
    %?DEBUG("BUILD: load_db_data ~p",[Data]),
    {Count, B1} = read_int(Data),
    {Slots, _BinOffset} = load_slots(B1, Count, []),
    Slots.
load_slots(Bin, 0, Acc) -> {Acc, Bin};
load_slots(Bin, Count, Acc) ->
    {Slot, Bin2} = load_slot(Bin),
    load_slots(Bin2, Count-1, [Slot|Acc]).
load_slot(Data) ->
    {TypeID, B1} = read_int(Data),
    {Current, B2} = read_int(B1),
    {Max, B3} = read_int(B2),
    {Len, B4} = read_int(B3),
    {Items, BinOffset} = inventory:load_db_items(B4, Len, []),
    {{objects_server:get_type(TypeID), Current, Max, Items},BinOffset}.
