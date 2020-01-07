%% Author: arksu
%% Created: 04.06.2011
%% Description: TODO: Add description to inventory
-module(inventory).

-export([]).
-compile(export_all).

-include("defines.hrl").
-include("base_io.hrl").
-include("types.hrl").
-include("objects.hrl").

%% --------------------------------------------------------------------

% is type - inventory item. need load from db
is_item(Type) ->
	% UPDATE types.c !!!!!!!!
	cutils:is_object_item(Type).

%% --------------------------------------------------------------------
% размер вещи в инвентаре
item_inventory_size(Type) ->
    case Type of
        stone_axe -> {1,2};
        billet -> {1,2};
		fabric_bag -> {2,2};
		board -> {4, 1};
		rabbit_catched -> {2,2};
		rabbit_dead -> {1,2};
		
        _ -> {1,1}
    end.

%% --------------------------------------------------------------------
% время жизни вещи будучи брошенной на землю в секундах
get_lifetime(Type) ->
	case Type of
		billet -> 120;
		branch_leaf -> 30;
		stone_piece -> 120;
		board -> 120;
		seed_wild -> 60;
		brick_clay -> 360;
		straw -> 360;
		_ -> 50
	end.

%% --------------------------------------------------------------------
% get object invetory size, not item! example for box his inventory size will be 4x4
obj_inventory_size(Type) ->
    case Type of
		% хак для определения размера инвентаря игрока
		{player, Equip} -> get_player_inventory_size(Equip);
        box -> {4, 4};
		fabric_bag -> {5, 5};
		jar -> {2, 4};
		
		{bonfire, main} -> {4,2};
		{bonfire, fuel} -> {4, 1};

        _ -> ?DEBUG("WARNING: item_inventory_size non inv object!"), {1,1}
    end.

%% --------------------------------------------------------------------
% получить размер инвентаря игрока
get_player_inventory_size(_Equip) ->
    {5, 5}.

%% --------------------------------------------------------------------
% is object have inventory in it
obj_have_inventory(Type) ->
    case Type of
		player -> true; % ))))))))))) как ни парадоксально но факт)
        box -> true; % TEST!!!!!!
        fabric_bag -> true;
		jar -> true;

        % usually item havnt inventory
        _ -> false
    end.

%% --------------------------------------------------------------------
% can put to object?
% return true | false
can_put_to_object(RecvType, PutType) ->
	case RecvType of
        % игрок в себя может положить что угодно
	    {player, _} -> true;

        % в мешок нельзя класть другой мешок
 	    fabric_bag -> if PutType == fabric_bag -> false; true -> true end;
		
		% костер
		{bonfire, main} -> if PutType == clay -> true; true -> false end;
		{bonfire, fuel} -> if PutType == branch -> true; PutType == board -> true; true -> false end;

        % по дефолту можно все
	    _ -> true
	end.

%% --------------------------------------------------------------------
% можем ли взять вещь из инвентаря
can_take_from_object(_, none) -> true;
can_take_from_object(InvType, Taked) ->
	case InvType of
		{bonfire, fuel} ->
			if 
				Taked#item.ticks > 0 -> false;
				true -> true
			end;
		_ -> true
	end.
%% --------------------------------------------------------------------
% get icon name and hint for item
item_get_icon_hint(Item) ->
    #item{type=Type} = Item,
	case is_item(Type) of
		true -> {atom_to_list(Type), atom_to_list(Type)};
		false -> {"unknown", "unknown"}
	end.

%% --------------------------------------------------------------------
% rmb on item
item_right_click(Item) ->
    #item{id=ObjID, type=Type} = Item,
	case food:is_food(Type) of
		true ->	 			{context, ObjID, ["eat"]};
		false ->
	case Type of
        drawing_box -> 		{context, ObjID, ["build"]};
		billet -> 			{context, ObjID, ["split_branches"]};
		rabbit_catched -> 	{context, ObjID, ["wreck_neck"]};
		apple_stub -> 		{context, ObjID, ["extract_seed"]};
        _ -> none
	end
    end.

%% --------------------------------------------------------------------
% выбор контекстного меню для вещи
item_context(Item, Action, Inventory, OwnerID, PlayerState) ->
    #item{id=ObjID, type=Type, q=Q} = Item,
	case food:is_food(Type) of
		true -> if 
					Action == "eat" -> 
						case food:get_result(Item) of
							false ->
								case inventory:take_item(Inventory, ObjID) of
									false -> none;
									{EatItem, NewInv} ->
										db_server:inventory_remove(ObjID),
										S1 = player_server:player_eat(PlayerState, EatItem),
										{state, S1, NewInv}
								end;
							ResultType ->
								% превращаем вещь в новую
								NewItem = Item#item{type=ResultType},
								case replace_item(NewItem, ObjID, OwnerID, Inventory) of
									false -> none;
									NewInv ->
										S1 = player_server:player_eat(PlayerState, Item),
										{state, S1, NewInv}
								end
						end;
								
				   	true -> none end;
		_ ->
    case Type of
        % place building of box
        drawing_box ->
			if Action == "build" ->
            	{place, {box,ObjID,build}};
            true -> none end;
		
		billet -> if 
			(Action == "split_branches") andalso (is_record(PlayerState, player_state)) ->
				% удаляем основу
				case inventory:take_item(Inventory, ObjID) of
					false -> none;
					{_, NewInv} ->
						db_server:inventory_remove(ObjID),
    					
						% спавним ветки
						F = fun (S) ->
							NewItems = spawn_items(4, branch, Q, 1),
							player_server:player_gain_exp(
								player_server:player_add_items(S, NewItems),
								#exp{nature=3}
							)
						end,
						{state_func, F, NewInv}
				end;
			true -> none end;
		
		rabbit_catched -> if 
			(Action == "wreck_neck") andalso (is_record(PlayerState, player_state)) ->
				% превращаем вещь в новую
				NewItem = Item#item{type=rabbit_dead},
				case replace_item(NewItem, ObjID, OwnerID, Inventory) of
					false -> none;
					NewInv ->
						S1 = player_server:player_gain_exp(PlayerState, #exp{nature=3, industry=3}),
						{state, S1, NewInv}
				end;
			true -> none end;
								
		apple_stub -> if 
			(Action == "extract_seed") andalso (is_record(PlayerState, player_state)) ->
				% превращаем вещь в новую
				NewItem = Item#item{type=seed_apple},
				case replace_item(NewItem, ObjID, OwnerID, Inventory) of
					false -> none;
					NewInv ->
						S1 = player_server:player_gain_exp(PlayerState, #exp{nature=3}),
						{state, S1, NewInv}
				end;
			true -> none end;		
		
        _ -> none
    end end.

%% --------------------------------------------------------------------
% во что превращается вещь брошенная на землю
get_type_for_spawn(ItemType) ->
	case ItemType of
		rabbit_catched -> rabbit;
		_ -> ItemType
	end.

%##########################################################################################
% SYSTEMS

%% --------------------------------------------------------------------
% обработать клик в инвентаре
% take, put, item_act, context, drop
item_click2(OwnerInventoryType, InvList, Hand, 
			InvObjID, ObjID, IX, IY, Btn, Mod) ->
	case Hand of
		% в руке ничего не держим
		none -> case take_item(InvList, ObjID) of
			false -> none; % попали в вещь которой нет в инвентаре
			{#item{} = FoundItem, _} -> case Btn of
				?MB_LEFT -> case Mod of
					?MOD_CONTROL -> 
						% берем вещь 
						case take_item(InvList, ObjID) of
	                    	false -> none;
	                    	{TakedItem, NewList} ->
								case can_take_from_object(OwnerInventoryType, TakedItem) of
									true -> {drop, NewList, TakedItem};
									false -> none
								end
	                	end;
					_ ->
						% берем вещь в руку
						case take_item(InvList, ObjID) of
	                    	false -> none;
	                    	{TakedItem, NewList} ->
								case can_take_from_object(OwnerInventoryType, TakedItem) of
									true -> {true, NewList, true, set_hand(TakedItem)};
									false -> none
								end
	                	end
					end;
				?MB_RIGHT ->
					% контекстное меню, или использовать вещь
					item_right_click(FoundItem);
				?MB_DOUBLE ->none;
				?MB_MIDDLE -> none
				end
			end;
		% HAND ----------------------------			
		#item{type=HandType} -> case Btn of
				?MB_LEFT ->
					% в какой именно инвентарь попали
					InventoryType = case take_item(InvList, InvObjID) of
									false -> OwnerInventoryType;
									{#item{type=IT},_} -> IT
					end,
					% положим вещь в руки в инвентарь
					case can_put_to_object(InventoryType, HandType) of
	                    true ->
							?DEBUG("try put object ~p ~p",[InventoryType, HandType]),
		                    % проверяем свободное место в инвентаре
		                    case put_to_inventory(InvList, obj_inventory_size(InventoryType), Hand, IX, IY, InvObjID, OwnerInventoryType) of
			                    false -> none;
			                    {NewList, NewHand} ->
									{true, NewList, true, NewHand}
		                    end;
	                    false -> none
	                end;
				?MB_RIGHT -> none;
				?MB_DOUBLE ->none;
				?MB_MIDDLE -> none
			end
	end.

%% --------------------------------------------------------------------
% deprecated
% получить список вещей которые исчезли из нового списка относительно старого. не рекурсивно.
get_deleted_items([], _NewList,Acc) -> Acc;
get_deleted_items(OldList, NewList, Acc) ->
	[H|T] = OldList,
	#item{id=ObjID} = H,
	case lists:keymember(ObjID, 2, NewList) of
		false -> get_deleted_items(T, NewList, [H|Acc]);
		true ->  get_deleted_items(T, NewList, Acc)
	end.

%% --------------------------------------------------------------------
% load item in hand of char
% return Item | none
load_hand(CharID) ->
    Rows = db_server:inventory_load_hand(CharID),
    case Rows of
        [] -> none;
        _ ->
            [Item|_] = parse_inventory(Rows, []),
            Item
    end.

%% --------------------------------------------------------------------
% load inventory from bd and parse it
% return array of items
load_inventory(ObjID) ->
    Rows = db_server:inventory_load(ObjID),
    parse_inventory(Rows, []).

% parse rows from db
parse_inventory([], Acc) -> Acc;
parse_inventory([  [ObjID,TypeID,Q, Amount, TicksLeft, Ticks, X,Y,Num]  |Tail], Acc) ->
    Type = objects_server:get_type(TypeID),
    % if item have inv - must load it recursive
    Items = case obj_have_inventory(Type) of
                true -> load_inventory(ObjID);
                false -> []
            end,
    Item = #item{id=ObjID, type=Type, q=Q, amount=Amount, ticks_left=TicksLeft, ticks=Ticks, x=X,y=Y,num=Num, items=Items},
    parse_inventory(Tail, [Item|Acc]).

%% --------------------------------------------------------------------
% get inventory list from object
% return Inventory
get_object_inventory(#object{type=Type, params=Params, state=State}, Tag) ->
	case Type of
		bonfire ->
			#obj_bonfire{inv=Inv, fuel=Fuel} = State,
			case Tag of		   
				main -> Inv;
				fuel -> Fuel
			end;
		_ ->
    case lists:keyfind(inventory, 1, Params) of
        false -> none;
        {_,InvList} -> InvList
	end
    end.

% return new object
set_object_inventory(#object{type=Type, params=Params, state=State}=O, NewInv, Tag) ->
	case Type of
		bonfire ->
			case Tag of
				main -> O#object{state=State#obj_bonfire{inv=NewInv}};
				fuel -> O#object{state=State#obj_bonfire{fuel=NewInv}}
			end;
		_ ->
    case lists:keytake(inventory, 1, Params) of
        false -> O#object{params=[ {inventory, NewInv} | Params ]};
        {value, _, Tail} -> 
			O#object{params=[ {inventory, NewInv} | Tail ]}
	end
    end.

get_object_inv_type(Type, Tag) ->
	case Type of
		bonfire -> {Type, Tag};
		_ -> Type
	end.

%% --------------------------------------------------------------------
% InventoryID - в какой инвентарь кладем. они могут быть вложенными.
put_to_inventory(InvList, {W,H}, Hand, X, Y, InventoryID, OwnerInventoryType) ->
	% пытаемся найти указанный инвентарь в списке вещей
	case take_item(InvList, InventoryID) of
		% его там нет. пытаемся положить в корень. InventoryID - укажет на ид объекта владельца всего инвентаря
		false -> put_to_inventory_direct(InvList, {W,H}, Hand, X, Y, InventoryID, OwnerInventoryType);
		% нашли указанный вложенный инвентарь
		{#item{items=List, type=Type} = Item, _} -> 
			% пытаемся положить в него
			case put_to_inventory_direct(List, obj_inventory_size(Type), Hand, X, Y, InventoryID, OwnerInventoryType) of
				false -> false;
				{NewList, NewHand} ->
					% положили. надо обновить вещь - владельца
					NewItem = Item#item{items=NewList},
					% заменим вещь владельца в исходном инвентаре		
					% ид для апдейта в базе не указываем. т.к. исходная вещь не изменилась. надо было только положить в нее.
					% это было сделано вызововм put_to_inventory_direct
					{replace_item(NewItem, InventoryID, none, InvList), NewHand}
			end
	end.

%% --------------------------------------------------------------------
% put item to inventory
% return false | {NewList, NewHand}
put_to_inventory_direct(InvList, {W,H}, Hand, X, Y, InventoryID, OwnerInventoryType) -> %{[Hand|InvList], none}.
    #item{type=Type} = Hand,
    {IW,IH} = item_inventory_size(Type),
    % check size inventory
    if (X < 0) or (Y < 0) or ((X+IW)>W) or ((Y+IH)>H) -> false;
    true ->
    % get intersections
    IntersectList = get_intersect(X,Y,X+IW,Y+IH, InvList, []),
    L = length(IntersectList),
    if L > 1 -> false;
    true ->
        % if 1 - take to hand
        {TakedItem, NewList} = if L == 1 ->
                         [#item{id=TakeObjID}] = IntersectList,
                         case take_item(InvList, TakeObjID) of
                             % cant find item o_O
                             false ->
                                 ?DEBUG("INVENTORY: put_to_inventory_direct WARNING len=1 no take"),
                                 {none, InvList};
                             Res -> Res
                         end;
                        % L = 0. no intersect. dont need to take
                        true -> {none, InvList}
        end,
		case can_take_from_object(OwnerInventoryType, TakedItem) of
			true -> 
		        % check free space without taked item from inventory
		        IntersectList2 = get_intersect(X,Y,X+IW,Y+IH, NewList, []),
		        % if have intersect - no free space
		        if length(IntersectList2) > 0 -> false;
		        % have free space. put hand to inventory, return taked item
		        true ->
		            UpdatedItem = Hand#item{x=X,y=Y},
		            db_server:inventory_update(UpdatedItem, InventoryID),
		            {[UpdatedItem|NewList], set_hand(TakedItem)}
		        end;
			false -> false
		end
    end
    end.

% return false | {ok, PosX, PosY}
find_free_space(InvList,W,H, Item) ->
    find_free_spaceY(InvList,W,H, Item, 0).

find_free_spaceY(_InvList,_W,H, _Item, Y) when Y >= H -> false;
find_free_spaceY(InvList,W,H, Item, Y) ->
    case find_free_spaceX(InvList,W,H, Item, 0, Y) of
        false -> find_free_spaceY(InvList,W,H, Item, Y+1);
        X -> {ok, X, Y}
    end.
find_free_spaceX(_InvList,W,_H, _Item, X, _Y) when X >= W -> false;
find_free_spaceX(InvList,W,H, Item, X, Y) ->
    #item{type=Type} = Item,
    {IW,IH} = item_inventory_size(Type),
    if (X < 0) or (Y < 0) or ((X+IW)>W) or ((Y+IH)>H) -> false;
    true ->
    IntersectList = get_intersect(X,Y,X+IW,Y+IH, InvList, []),
    if length(IntersectList) > 0 -> find_free_spaceX(InvList,W,H, Item, X+1, Y);
    true -> X
    end
    end.

% set item to hand (x=255, y=0)
set_hand(none) -> none;
set_hand(Item) ->
    Item#item{x=255,y=0, ticks=0, ticks_left=0}.

set_coord(none,_,_) -> none;
set_coord(Item,X,Y) ->
    Item#item{x=X,y=Y}.


get_item_type(Item) ->
    #item{type=Type} = Item,
    Type.

get_intersect(_,_,_,_,[],Acc) -> Acc;
get_intersect(L,T,R,B,List,Acc) ->
    [I|Tail] = List,
    #item{type=Type, x=X,y=Y} = I,
    {W,H} = item_inventory_size(Type),
    case is_rect_intersect({L,T,R,B},{X,Y,X+W,Y+H}) of
        true -> get_intersect(L,T,R,B,Tail,[I|Acc]);
        false -> get_intersect(L,T,R,B,Tail,Acc)
    end.

%% --------------------------------------------------------------------
% find item in inventory by type
% return Item | false
find_item_type([], _Type) -> false;
find_item_type(InvList, AType) ->
    [Item|Tail] = InvList,
    #item{type=Type} = Item,
    if AType == Type -> Item;
       true -> find_item_type(Tail, AType)
    end.

%% --------------------------------------------------------------------
% find item in inventory by coords
% return Item | false
find_item([], _,_) -> false;
find_item(InvList, AX,AY) ->
    [Item|Tail] = InvList,
    #item{type=Type,x=X,y=Y} = Item,
    {W,H} = item_inventory_size(Type),
    %?DEBUG("test ~p ~p = ~p ~p ~p ~p",[AX,AY,X,Y,W,H]),
    if (AX >= X) and (AY >= Y) and (AX<(X+W)) and (AY<(Y+H)) -> Item;
       true -> find_item(Tail, AX,AY)
    end.

%% --------------------------------------------------------------------
% ищем инвентарь вещи в списке рекурсивно
% return Item | false
find_inventory([], _) -> false;
find_inventory([H|T], ItemID) ->
	#item{id = ID, items = Items, type=Type} = H,
	if ID == ItemID -> H;
	true -> case obj_have_inventory(Type) of
			true -> case find_inventory(Items, ItemID) of
				false -> find_inventory(T, ItemID);
				Found -> Found
				end;
			false -> find_inventory(T, ItemID)
			end
	end.

%% --------------------------------------------------------------------
% получить вещь контейнер содержащую вещь с указанным ид
find_item_container([], _, _) -> false;
find_item_container([H|T], ItemID, ParentID) ->
	#item{id = ID, items = Items, type=Type} = H,
	if ID == ItemID -> ParentID;
	true -> case obj_have_inventory(Type) of
			true ->
				case find_item_container(Items, ItemID, ID) of
				false -> find_item_container(T, ItemID, ParentID);
				Found -> Found
				end;
			false -> find_item_container(T, ItemID, ParentID)
			end
	end.


%% --------------------------------------------------------------------
% return new list | false
replace_item(_, _, _, []) -> false;
replace_item(NewItem, ObjID, InventoryID, InvList) ->
    case lists:keytake(ObjID, #item.id, InvList) of
        false ->
			replace_item_from_child(NewItem, ObjID, InventoryID, InvList, []);
        {value, _, NewList} ->
			case InventoryID of
				none -> ok;
				_ -> db_server:inventory_update(NewItem, InventoryID)
			end,
			[NewItem|NewList]
			
    end.
replace_item_from_child(_, _, _, [], _) -> false;
replace_item_from_child(NewItem, ObjID, InventoryID, [H|T], Acc) ->
	#item{items=Childs, id=ID} = H,
	case replace_item(NewItem, ObjID, 
					  % только если надо обновлять в базе передадим ид инвентаря
					  if InventoryID == none -> none; true -> ID end, 
					  Childs) of
		false ->
			replace_item_from_child(NewItem, ObjID, InventoryID, T, [H|Acc]);
		NewInv ->
			?DEBUG("child item replaced! ~p",[NewInv]),
			lists:append([H#item{items=NewInv}|Acc], T)
	end.


%% --------------------------------------------------------------------
% take item from invetory (extract from list)
% return {Item, NewList} | false
take_item([], _) -> false;
take_item(InvList, ObjID) ->
    case lists:keytake(ObjID, #item.id, InvList) of
        false ->
			take_item_from_child(InvList, ObjID, []);
        {value, Item, NewList} -> {Item, NewList}
    end.

take_item_from_child([], _, _) -> false;
take_item_from_child([H|T], ObjID, Acc) ->
	#item{items=Childs} = H,
	case take_item(Childs, ObjID) of
		false ->
			take_item_from_child(T, ObjID, [H|Acc]);
		{TakedItem, NewInv} ->
			?DEBUG("child item taked! ~p ~p",[TakedItem, NewInv]),
			{TakedItem, lists:append([H#item{items=NewInv}|Acc], T)}
	end.

% взять вещи из списка. указанного типа нужного количества
% return {TakedItems, NewInv} | false
take_items(InvList, _, Count, Acc) when Count =< 0 -> {Acc,InvList};
take_items(InvList, SlotType, Count, Acc) ->
	?DEBUG("take_items count=~p acc=~p",[Count,Acc]),
    case take_slot_item(SlotType, InvList, []) of
        false -> case take_items_from_child(InvList, [], SlotType, []) of
					 false -> false;
					 {TakedItems, NewInv} ->
						 ?DEBUG("from child taked! ~p ~p",[TakedItems, NewInv]),
						 take_items(NewInv, SlotType, Count-1, lists:append(TakedItems,Acc))
				 end;
        {value, Item, NewList} -> take_items(NewList,SlotType,Count-1,[Item|Acc])
    end.


%-----------------------------------------------------------------------------
% взять вещь указанного типа слоат из списка (lists:keytake)
% false | {value, Item, NewList}
take_slot_item(SlotType, [Item|T], L) ->
	case objects_server:check_slot_item(SlotType, Item) of
		true -> {value, Item, lists:reverse(L, T)};
		false -> take_slot_item(SlotType, T, [Item|L])
	end;
take_slot_item(_SlotType, [], _L) -> false.

%-----------------------------------------------------------------------------
% берем вещи из детей
take_items_from_child([], _AccList, _Type, _Acc) -> false;
take_items_from_child(List, AccList, Type, Acc) ->
	?DEBUG("take_items_from_child acc=~p",[Acc]),
	[H|T] = List,
	#item{items=Childs} = H,
	case take_items(Childs, Type, 1, Acc) of
		false ->
			take_items_from_child(T, [H|AccList], Type, Acc);
		{TakedItems, NewInv} ->
			?DEBUG("child items taked! ~p ~p",[TakedItems, NewInv]),
			{TakedItems, lists:append([H#item{items=NewInv}|AccList], T)}
	end.


%-----------------------------------------------------------------------------
% получить список типов из инвентаря
get_items_types([], Acc) -> Acc;
get_items_types(List, Acc) ->
    [H|T] = List, 
    #item{type=Type} = H, 
	case lists:member(Type, Acc) of
		false ->
			% если в списке нет - добавляем и идем дальше
    		get_items_types(T, [Type|Acc]);
		_ ->
			get_items_types(T, Acc)
	end.

%-----------------------------------------------------------------------------
% make packet for inventory items
get_inventory_packet([], Acc) -> Acc;
get_inventory_packet(InvList, Acc) ->
    [I|T] = InvList,
	A = case I of
		none ->
			[write_int(0)];
		#item{id=ObjID, type=Type, q=Q,ticks_left=TicksLeft, ticks=Ticks, x=X,y=Y} ->
    		{W,H} = item_inventory_size(Type),
    		{Icon,Hint} = item_get_icon_hint(I),

    		[write_int(ObjID), write_int(X), write_int(Y),
         	write_int(W), write_int(H),
         	write_int(Q),
         	write_string(Icon), write_string(Hint),
         	write_int(Ticks-TicksLeft),
         	write_int(Ticks),
         	write_int(0)]
	end,
    get_inventory_packet(T, [A|Acc]).

%-----------------------------------------------------------------------------
% уничтожить все вещи из списка. удаление из бд
destroy_items([]) -> ok;
destroy_items(List) when is_list(List)-> 
	[H|T] = List,
	destroy_items(H),
	destroy_items(T);
destroy_items(#item{id=ID}) ->
	db_server:inventory_remove(ID).
%-----------------------------------------------------------------------------
% заспавнить новую вещь
spawn_item(Type, Q, Amount) ->
	#item{id=world_server:get_next_id(), q=Q, amount=Amount, type=Type}.

%-----------------------------------------------------------------------------
% заспавнить сразу несколько вещей
spawn_items(Count, Type, Q, Amount) -> spawn_items(Count, Type, Q, Amount, []).
spawn_items(0, _Type, _Q, _Amount, Acc) -> Acc;
spawn_items(Count, Type, Q, Amount, Acc) ->
	spawn_items(Count-1, Type, Q, Amount, [spawn_item(Type, Q, Amount)|Acc]).

%-----------------------------------------------------------------------------
% get data for item to save db in object
% return binary
save_db_data(Item) ->
    #item{id=ObjID, type=Type, q=Q, amount=Amount, ticks_left=TicksLeft, ticks=Ticks, x=X,y=Y,num=Num, items=Items} = Item,
    A = [
         write_int(ObjID), write_int(objects_server:get_typeid(Type)),
         write_int(Q),write_int(Amount),
         write_int(TicksLeft),
         write_int(Ticks),
         write_int(X), write_int(Y),
         write_int(Num),
         write_int(length(Items)),
         save_db_items(Items,[])
         ],
    list_to_binary(A).

save_db_items([],Acc) -> Acc;
save_db_items(Items, Acc) ->
    [I|T] = Items,
    save_db_items(T,[save_db_data(I)|Acc]).

%-----------------------------------------------------------------------------
% return item
load_db_data(Data) ->
    {ObjID, B1} = read_int(Data),
    {TypeID, B2} = read_int(B1),
    {Q, B3} = read_int(B2),
    {Amount, B4} = read_int(B3),
    {TicksLeft, B5} = read_int(B4),
    {Ticks, B6} = read_int(B5),
    {X, B7} = read_int(B6),
    {Y, B8} = read_int(B7),
    {Num, B9} = read_int(B8),
    {Len, B10} = read_int(B9),
    {Items, BinOffset} = load_db_items(B10, Len, []),
    {#item{id=ObjID, type=objects_server:get_type(TypeID), q=Q, amount=Amount, ticks_left=TicksLeft, ticks=Ticks, x=X,y=Y,num=Num, items=Items}, BinOffset}.

load_db_items(Bin, 0, Acc) -> {Acc, Bin};
load_db_items(Bin, Count, Acc) ->
    {Item, Bin2} = load_db_data(Bin),
    load_db_items(Bin2, Count-1, [Item|Acc]).

is_rect_intersect({L1,T1,R1,B1},{L2,T2,R2,B2}) when
  (((L1 >= L2) and (L1 < R2)) or ((R1 > L2) and (R1 < R2)) or
  ((L2 >= L1) and (L2 < R1)) or ((R2 > L1) and (R2 < R1))) and
  (((T1 >= T2) and (T1 < B2)) or ((B1 > T2) and (B1 < B2)) or
  ((T2 >= T1) and (T2 < B1)) or ((B2 > T1) and (B2 < B1))) -> true;
is_rect_intersect(_,_) -> false.
