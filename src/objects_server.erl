%%% -------------------------------------------------------------------
%%% Author  : arksu
%%% Description :
%%%
%%% Created : 18.02.2011
%%% -------------------------------------------------------------------
-module(objects_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("types.hrl").
-include("objects.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
		 is_liftable/1,
		 get_hp/3,
         get_typeid/1,
         get_type/1,
         get_drawable/1,
         get_time_type/1,
		 get_stealth/1,
		 get_always_show/1,
		 can_destroy/1,
		 get_repair/1,
		 get_direction/1,
		 set_direction/2,
		 can_lift_down_on_tile/2,
         save_object_data/1,
         load_object_data/2,
		 parse_row_obj/2,
         get_new_params/1,
		 open_object/2,
		 can_spawn_on_tile/2,
         object_fill/1,
		 is_object_save_db/1,
         slot_image_name/1,
         slot_hint/1,
         check_slot_item/2,
		 visual_state_packet/1,
		 visual_state_have/1,
		 update_tick_object/3]).


-include("defines.hrl").
-include("map.hrl").
-include("base_io.hrl").

% #########################################################################################################
% DEFINES !!!!!
% #########################################################################################################

is_liftable(box) -> true;
is_liftable(runestone) -> true;
is_liftable(jar) -> true;
is_liftable(chair) -> true;
is_liftable(log) -> true;
is_liftable(logy) -> true;
is_liftable(_) -> false.

%% --------------------------------------------------------------------
% static, tick or dyn type of object.
get_time_type(Type) ->
	TypeID = cutils:get_object_typeid(Type),
	if (TypeID >= ?MIN_TICK_ID) andalso (TypeID < ?MAX_TICK_ID) -> tick;
	true ->
    case Type of
		player -> dyn;
        box -> tick;
		rabbit -> dyn;
		bonfire -> tick;

        _ -> static
	end
    end.

%% --------------------------------------------------------------------
% действие для "уничтожить" - сколько времени надо на действие
% false - нельзя уничтожить
can_destroy(#object{type=Type}) ->
	case Type of
		player -> false;
		stone -> false;
		claim -> false;
		
		_ -> 2000
	end.

%% --------------------------------------------------------------------
% данные для ремонта предмета
get_repair(#object{type=log, hp={Current,Max}}) when Current<Max -> {true, 2000, billet, 1, 20};  % time, slot_name, count, hp
get_repair(_) -> false.


%% --------------------------------------------------------------------
% получить хп объекта (заполняется при создании)
get_hp(Type, Q, _State) ->
	case Type of
		_ -> Q * 10 % по умолчанию хп = ку * 10
	end.

%% --------------------------------------------------------------------
% получить скрытность (срабатывает в момент загрузки объекта из бд и в момент создания)
get_stealth(#object{type=Type}) ->
	case Type of
%%  		plant_carrot -> 1;
%% 		box -> 1;
		plant_wild -> 1;
		string -> 5;
		string_wild -> 5;
		_ -> none
	end.

%% --------------------------------------------------------------------
% всегда показывать в клиенте (не скрывать рядом с игроком)
get_always_show(Type) ->
	case inventory:is_item(Type) of 
		true -> true;
		_ ->
	case Type of
		log -> true;
		logy -> true;
		chair -> true;
		
		_ -> false
	end end.

%% --------------------------------------------------------------------
% get draw name for object
get_drawable(Object) ->
    #object{type=Type, state=State} = Object,
	StrType = atom_to_list(Type),
	IsSeed = string:left(StrType, 5),
	
	D = 
	if IsSeed == "seed_" -> "seed";
	   
	true ->	
	case farming:is_plant(Type) of true ->
			#obj_plant{stage=Stage} = State,
			% "type_stage"
			atom_to_list(Type)++"_"++integer_to_list(Stage);
		
	false ->
    case Type of
        player -> "player";
		string_wild -> "string";
		
		build_corner_fence -> "build_sign";
		build_wallx_fence -> "build_sign";
		build_wally_fence -> "build_sign";
		build_gatex_fence -> "build_sign";
		build_gatey_fence -> "build_sign";
		build_chair -> "build_sign";
		build_jar -> "build_sign";
		
		gatex_fence ->
			#obj_gate{opened=IsOpened} = State,
			case IsOpened of true -> "open_gatex_fence"; false -> "close_gatex_fence" end;
		gatey_fence -> 
			#obj_gate{opened=IsOpened} = State,
			case IsOpened of true -> "open_gatey_fence"; false -> "close_gatey_fence" end;
		bonfire ->
			#obj_bonfire{fire=Fire, fuel=Fuel} = State,
			case Fire of true -> "bonfire_fire"; 
				false ->
					case game_bonfire:get_fuel_list(Fuel) of
						[] -> "bonfire";
						_ -> "bonfire_fuel"
					end
			end;
			
		
        _ -> atom_to_list(Type)
    end end end,
	{drawable, D}.

%% --------------------------------------------------------------------
% get params for spawned object
get_new_params(_Type) -> [].
%%     case Type of
%%         _ -> []
%% 	end.

%% --------------------------------------------------------------------
% может ли быть заспавнен на указанном типе тайла (стройка)
can_spawn_on_tile(Type, #tile{type=TileType}) ->
	case TileType of
		water_low -> false;
		water_deep -> false;
		hole -> false;
		_ ->
	
	case Type of
		% стройку стены можно спавнить только на замощенном тайле
		build1_corner_home_wood -> if TileType == sett -> true; true -> false end;
		corner_home_wood -> if TileType == sett -> true; true -> false end;

		build1_wallx_home_wood -> if TileType == sett -> true; true -> false end;
		wallx_home_wood -> if TileType == sett -> true; true -> false end;
		build1_wallwindowx_home_wood -> if TileType == sett -> true; true -> false end;
		wallwindowx_home_wood -> if TileType == sett -> true; true -> false end;
		build1_walldoorx_home_wood -> if TileType == sett -> true; true -> false end;
		walldoorx_home_wood -> if TileType == sett -> true; true -> false end;

		build1_wally_home_wood -> if TileType == sett -> true; true -> false end;
		wally_home_wood -> if TileType == sett -> true; true -> false end;
		build1_wallwindowy_home_wood -> if TileType == sett -> true; true -> false end;
		wallwindowy_home_wood -> if TileType == sett -> true; true -> false end;
		build1_walldoory_home_wood -> if TileType == sett -> true; true -> false end;
		walldoory_home_wood -> if TileType == sett -> true; true -> false end;

		_ -> true
	end
	end.

%% --------------------------------------------------------------------
% можно ли поставить на этот тайл объект который несли в руках
can_lift_down_on_tile(Type, #tile{type=TileType}) ->
	case Type of
		boat -> true; % лодку можно поставить везде
		_ ->
			% остальные объекты только не на воде
			case TileType of
				water_low -> false;
				water_deep -> false;
				hole -> false;
				_ -> true
			end
	end.
				

%% --------------------------------------------------------------------
% SLOTS (для билда или крафта)

slot_image_name(SlotType) -> atom_to_list(SlotType).
slot_hint(SlotType) -> atom_to_list(SlotType).


%% --------------------------------------------------------------------
% проверить подходит ли вещь Item для слота SlotType
check_slot_item(_, none) -> false;
check_slot_item(SlotType, Item) ->
    #item{type=ItemType} = Item,
    case SlotType of
		% кусок тайла.
		tile_piece -> lists:member(ItemType,[soil,clay]);

		slot_seed -> farming:is_seed(ItemType);

		% по умолчанию проверяем совпадение на тип объекта
        _ ->  lists:member(ItemType,[SlotType])
    end.
 
%% --------------------------------------------------------------------
% создать стейт для объектов
generate_state(Type, OriginalState) ->
	case Type of
		gatex_fence -> 	#obj_gate{opened = false};
		gatey_fence -> 	#obj_gate{opened = false};
		chair -> 		#obj_chair{};
		bonfire ->		#obj_bonfire{};
			
		_ -> OriginalState
	end.

%% --------------------------------------------------------------------
% получить если есть направление объекта (которое сохраняется)
get_direction(#object{type=Type, state=S}) ->
	case Type of
		chair -> 
			#obj_chair{direction=D}=S, D;
		_ -> 0
	end.
%% --------------------------------------------------------------------
% установить направление объекту - если возможно
set_direction(#object{type=Type} = Object, Dir) ->
	case Type of
		chair -> Object#object{state=#obj_chair{direction=Dir}};
		_ -> false
	end.
  
%% --------------------------------------------------------------------
% сохраняется ли объект в бд (или чисто спавница рандомно)
is_object_save_db(Type) ->
	case Type of 
		player -> false;
		rabbit -> false;
		_ -> true
	end.

%% --------------------------------------------------------------------
% попытаться открыть объект учитывая что он уже может быть открыт 
open_object(#object{id=ObjID}, _FromObjID) -> {action, open, 0, ObjID}.
	% сначала надо проверить - может уже открыт мной
%% 	case lists:keyfind(opened, 1, Params) of
%%     	false ->
%% 		{action, open, 0, ObjID}; % 0 ticks. immediately
%% 	{opened, List} -> {action, open, 0, ObjID}
%% 		?DEBUG("box already opened objid=~p list=~p", [FromObjID, List]),
%%     	% find id in list
%%     	case lists:member(FromObjID, List) of
%%     		% add id to list
%% 			true -> ?DEBUG("player in opened list"), {none};
%%         	false -> ?DEBUG("player not open box"),
%% 				 {action, open, 0, ObjID}
%% 		end
%% 	end.


%% --------------------------------------------------------------------
% имеет ли объект визуальное отображение на клиента (при изменении объекта будет высылатся стейт всем)
visual_state_have(Type) ->
	case build:is_build(Type)of
		true -> true;
		false ->
	case inventory:obj_have_inventory(Type) of
		true -> true;
		false ->
	case Type of
		bonfire -> true;
		claim -> true;
		runestone -> true;
		_ -> false
	end
	end
	end.

%% --------------------------------------------------------------------
% конвертировать стейт объекта для его отображения на клиенте
visual_state_packet(#object{id=ObjID, type=Type, state=State} = Object) ->
	%--------------------------------------
	case inventory:obj_have_inventory(Type) of
		true -> 
			% создаем пакет для инвентаря
			Inv = inventory:get_object_inventory(Object, main),
			{W, H} = inventory:obj_inventory_size(Type),
			L = if is_list(Inv) -> length(Inv); true -> 0 end,
			{inventory, [write_int(ObjID), write_byte(W), write_byte(H), write_word(L), player_net:make_inventory_pkt(Inv, [])]};
		
		
	false ->
	%--------------------------------------
	case build:is_build(Type) of
		true ->
			?DEBUG("build state: ~p",[State]),
			{build, [write_byte(length(State)), build:make_slot_pkt(lists:reverse(State), [])]};
		
		
	false -> case Type of
	%--------------------------------------
		runestone -> case State of 
						 	none -> {Type, write_string("")};
						 	_ -> {Type, write_string(unicode:characters_to_binary(State))}
					 end;
	%--------------------------------------
		claim -> case State of
					 		none -> {Type, write_int(0)};
					 		#obj_claim{owner_id=OID} -> {Type, write_int(OID)}
				 end;
	%--------------------------------------
		bonfire -> 
			#obj_bonfire{fire=Fire, fuel=Fuel, inv=Inv} = State,
			L = if is_list(Inv) -> length(Inv); true -> 0 end,
			L2 = if is_list(Fuel) -> length(Fuel); true -> 0 end,
			{Type, [
						write_byte(if Fire == true -> 1; true -> 0 end),
						
						write_byte(4), write_byte(2),
						write_word(L), player_net:make_inventory_pkt(Inv, []),
						
						write_byte(4), write_byte(1),
						write_word(L2), player_net:make_inventory_pkt(Fuel, [])
				   ]};
									  
		_ -> {Type, write_term(State)}
			 
	end
	end
	end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%################################################################################################################
%################################################################################################################
%################################################################################################################

parse_row_obj(Sg, Row) ->
    %?DEBUG("obj: send obj ~p",[Row]),
    [ObjID,Grid,X,Y,TypeID,_Hp,_SHp,Data,Time,Q,HP,SHP] = Row,
    {_,_,Lv} = map_server:get_map_coord(Sg, Grid),
%%     ?DEBUG("obj: send obj to session"),
    Type = get_type(TypeID),
    % if obj have invetory - need load it
    Params = case inventory:obj_have_inventory(Type) of
        false -> [];
        true -> [{inventory, inventory:load_inventory(ObjID)}]
    end,
    O = #object{id=ObjID, type=Type, coord={X,Y,Lv}, params=Params, time=Time, q=Q, hp={HP,SHP} },
    O1 = load_object_data(O, Data),
	% добавляем скрытность если надо
	case get_stealth(O1) of
		none -> O1;
		S -> O1#object{params=[{stealth,S}|Params]}
	end.

%% --------------------------------------------------------------------
% load object data from database and fill params in object. Data - is bianry from db
load_object_data(Object, Data) ->
	case Data of
		<<>> -> Object#object{state = none };
    	_ -> Object#object{ state=binary_to_term(Data) }
	end.

%% --------------------------------------------------------------------
% return binary for save in bd field `data`
save_object_data(Object) ->
    #object{type=_Type, state=ObjectState } = Object,
	case ObjectState of
		none -> <<>>;
		_ -> term_to_binary (ObjectState)
	end.

%% ---------------------------------------------------------------------------------------------------------------
% заполнить состояние вновь создаваемого объекта
object_fill(Object) ->
    #object{type=Type, params=Params, state=OriginalState} = Object,
    % generate state
    ObjectState = case build:is_build(Type) of
            true -> build:generate_state(Type, OriginalState);
            false -> generate_state(Type, OriginalState)
    end,
    % if obj have inventory - add it to params
    Params1 = case inventory:obj_have_inventory(Type) of
                    true -> [{inventory, []}|Params];
                    false -> Params
    end,
	Params2 = case get_stealth(Object) of
		none -> [];
		S -> [{stealth,S}]
	end,

    Object#object{params=lists:append([Params1,get_new_params(Type), Params2]), state=ObjectState, time = time_server:get_time()}.

%% --------------------------------------------------------------------------------------------------------------------------
% обновить объект. и вернуть изменившийся. (сессия, сервер объектов) жизнь тик объектов.
% может изменится. остался как прежде. умер.
% return {Result, NewObject}   Result = true(changed) | false | die | to_static (объект стал статичным. закончил свою работу)
update_tick_object(_DT, Time, #object{type=bonfire} = Object) ->
	game_bonfire:update_tick(Time, Object);
update_tick_object(_DT, Time, #object{type=Type, id=_ObjID, time=ObjTime} = Object) ->
	case inventory:is_item(Type) of
		% если это вещь
		true ->
			% ищем время жизни
			% вещи брошенные на землю - просто умирают. не обновляясь
			LTime = inventory:get_lifetime(Type),
			if (Time - ObjTime) >= LTime ->
							% сдохли
							{die, Object};
			   true ->
						   	% если еще не сдохли - уменьшаем время жизни, и говорим что объект не изменился
						   	{false, Object}
			end;
		false ->
			{false,Object}
	end.


%% --------------------------------------------------------------------
% get id from type
get_typeid(Type) ->
	cutils:get_object_typeid(Type).

%% --------------------------------------------------------------------
% get type from id
get_type(TypeID) ->
	cutils:get_object_type(TypeID).


% defines.h types.c UPDATE!!!!!!  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<