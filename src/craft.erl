%% Author: arksu
%% Created: 01.07.2011
%% Description: TODO: Add description to craft
-module(craft).

-export([]).
-compile(export_all).

-include("defines.hrl").
-include("base_io.hrl").
-include("net.hrl").
-include("types.hrl").
%% --------------------------------------------------------------------
% описание крафта
%   {[FinalList], [RequiredList], RequiredObject, [RequiredEquip], Ticks}

% список того что получется в результате крафта
%   FinalList = {Type, Count} - тип и количество

% список требуемых вещей для крафта
%   RequiredList = {SlotType, Max, ItemsList}
%       SlotType - тип слота требуемых вещей
%       Max - сколько надо вещей
%       ItemsList - список вещей для крафта, сюда добавляются вещи непосредственно перед крафтом

% RequiredObject - тип объекта который должен быть прилинкован. или none - если не нужен
% RequiredEquip - список типов итемов которые должны быть в эквипе.

get_craft(CraftName) ->
    case CraftName of
        basket -> 			{ [{basket,1}],    		[{billet,3,[]}], none, none, 5000}; % TEST !!!!!!
		stone_axe -> 		{ [{stone_axe,1}],  	[{branch,1,[]}, {stone_piece,1,[]}], none, none, 5000};
%% 		brick_clay_raw ->	{ [{brick_clay_raw,1}], [{clay,1,[]}, {straw,1,[]}], none, none, 5000};
		wood_shoes -> 		{ [{wood_shoes,1}],  	[ {billet,3,[]} ], none, none, 6000};
        _ -> {[],[], none, [], 1}
    end.


%####################################################################################

% скрафтить вещь. с учетом формулы качества
% return [NewItem]
craft(_ReqList, _FinalList, Name) ->
	case Name of
		basket ->
			{ [inventory:spawn_item(basket, 10, 1)], #exp{nature=5} };
		stone_axe ->
			{ [inventory:spawn_item(stone_axe, 10, 1)], #exp{nature=10, industry=3} };
		brick_clay_raw ->
			{ [inventory:spawn_item(brick_clay_raw, 10, 1)], #exp{nature=10, industry=10} };
		wood_shoes ->
			{ [inventory:spawn_item(wood_shoes, 10, 1)], #exp{nature=10, industry=5} };
		_ -> []
	end.

%-----------------------------------------------------------------------------
% заполнить список вещами из инвентаря. удалить взятые из инвентаря
% return {Inventory, ReqAcc} | false
fill_craft_list([], Inventory, ReqAcc) -> {Inventory, ReqAcc};
fill_craft_list(ReqList, Inventory, ReqAcc) ->
    [H|T] = ReqList,
    {ReqType,Count,_} = H,
    case inventory:take_items(Inventory, ReqType, Count, []) of
        false -> false;
        {TakedItems, NewInv} ->
            fill_craft_list(T, NewInv, [{ReqType,Count,TakedItems}|ReqAcc])
    end.

%-----------------------------------------------------------------------------
% создать список из взятых вещей
get_taked_list([], Acc) -> Acc;
get_taked_list(Taked, Acc) ->
	[H|T] = Taked,
	{_ReqType,_Count,TakedItems} = H,
	get_taked_list(T, TakedItems ++ Acc).

%-----------------------------------------------------------------------------
% возможен ли крафт. удовлетворяют все условия
% return true | {false, Allow1, Allow2, Allow3}
check_craft(CraftName, PlayerState) ->
    #player_state{grids=Grids, 
                  inventory = Inventory,
                  equip = Equip,
                  linked_objid = LinkedObjID
                  } = PlayerState,
    Craft = craft:get_craft(CraftName),
    % проверяем условия
    {_,ReqItems,ReqObj, ReqEquip, _} = Craft,

    % проверяем прилинкованный объект если надо
    Allow1 = if (ReqObj =/= none) ->
					
		case player_server:send_grid_msg_ack(Grids, {find_object, self(), LinkedObjID}) of
				{object_found, Obj} ->
	                #object{type=Type} = Obj,
	                if Type == ReqObj ->
						   % TODO: can craft object?
							% спросить у объекта может ли он в данный момент сделать крафт
						   true;
					true -> false end;
                none ->
					false
        end;				
					
        
    true -> true
    end,

    % проверяем эквип
    Allow2 = if ReqEquip =/= none ->
                % преобразовать эквип в список типов
                L1 = inventory:get_items_types(Equip, []),
                % проверить наличие требуемого эквипа
                a1_utils:list_exist(ReqEquip, L1);
            true -> true
    end,

    % проверяем наличие требуемых вещей в инвентаре
    Allow3 = case craft:fill_craft_list(ReqItems, Inventory, []) of
        false ->
            false;
        _ -> true
    end,

    if (Allow1 == true) andalso (Allow2 == true) andalso (Allow3 == true) ->
        true;
    true -> {false, Allow1, Allow2, Allow3}
    end.
%-----------------------------------------------------------------------------
% отправить пакет на клиент
send_craft(Socket, CraftName) ->
    {FinalList, RequiredList, _, _, _} = get_craft(CraftName),
    ?DEBUG("send_craft ~p", [CraftName]),
    send_packet(Socket, ?GAMESERVER_CRAFT_LIST,
                [write_string(atom_to_list(CraftName)),
                 write_int(length(FinalList)),
                 send_final_list(FinalList, []),
                 write_int(length(RequiredList)),
                 send_req_list(RequiredList, [])]
               ).

send_final_list([], Acc) -> Acc;
send_final_list(List, Acc) ->
    [H|T] = List,
    {Type, Count} = H,
    send_final_list(T, [[write_string(objects_server:slot_image_name(Type)),
                         write_string(objects_server:slot_hint(Type)),
                         write_int(Count)
                                      ]|Acc]).

send_req_list([], Acc) -> Acc;
send_req_list(List, Acc) ->
    [H|T] = List,
    {SlotType, Max, _ItemsList} = H,
    send_req_list(T, [[write_string(objects_server:slot_image_name(SlotType)),
                         write_string(objects_server:slot_hint(SlotType)),
                         write_int(Max)
                                      ]|Acc]).