%% Author: arksu
%% Created: 27.08.2011
%% Description: TODO: Add description to equip
-module(equip).

-export([]).
-compile(export_all).
-include("defines.hrl").
-include("types.hrl").

type_in_hands(Type, #player_state{equip=Equip}) ->
	#equip{lhand=LHand, rhand=RHand}=Equip,
	LType = case LHand of none -> none; _ -> #item{type=A}=LHand, A	end,
	RType = case RHand of none -> none; _ -> #item{type=B}=RHand, B	end,
	if (Type==LType) orelse (Type==RType) -> true; true -> false end.

%% -------------------------------------------------------------------------------------------------------------
% return none | {NewHand, NewEquip}
handle_click(State, Slot, Btn) ->
	#player_state{equip=Equip, item_hand=Hand, charid=CharID} = State,
	case Btn of
		?MB_LEFT ->
		case Hand of
			% в руках ничего нет. пытаемся взять из слота вещь
			none ->
				case can_take_slot(State, Slot, Equip) of
					true ->
						update_equip(CharID, Slot, none),
						take_slot(Slot, Equip);
					false -> none
				end;
			% чтото держим в руках. кладем вещь в слот
			_ ->
				{TakedItem, _} = take_slot(Slot, Equip),
				case TakedItem of
					none ->
						case can_put_to_slot(State, Slot, Hand) of
							true ->
								update_equip(CharID, Slot, Hand),
								{none, set_slot(Slot, Hand, Equip)};
							false -> none
				 		end;
					_ -> none
				end
		end;
		?MB_RIGHT -> none; % TODO : mb right
		?MB_MIDDLE -> none;
		?MB_DOUBLE -> none
	end.
%% -------------------------------------------------------------------------------------------------------------
% можем ли взять вещь из слота?
can_take_slot(_State, _Slot, _Equip) ->
	true.
% можем ли положить указанную вещь в слот
can_put_to_slot(_State, Slot, Item) ->
	#item{type=Type} = Item,
	case Slot of
		lhand -> case Type of
					 stone_axe -> true;
					 billet -> true;
					 _ -> false
				 end;
		rhand -> case Type of
					 stone_axe -> true;
					 billet -> true;
					 _ -> false
				 end;
		foots -> case Type of
					wood_shoes -> true;
					_ -> false
				end;
		legs -> case Type of
					pants_skin -> true;
					_ -> false
				end;
		head -> case Type of
					fric_kepka -> true;
					_ -> false
				end;
		_ -> false
	end.

%% -------------------------------------------------------------------------------------------------------------
load_equip(CharID) ->
	Rows = db_server:inventory_load_equip(CharID),
    List = inventory:parse_inventory(Rows, []),
	set_equip(List, #equip{}).
%% -------------------------------------------------------------------------------------------------------------
set_equip([], Equip) -> Equip;
set_equip(List, Equip) ->
	[H|T] = List,
	#item{x=X} = H,
	Slot = get_slot(X),
	set_equip(T, set_slot(Slot, H#item{x=0, y=0}, Equip)).
%% -------------------------------------------------------------------------------------------------------------
update_equip(CharID, Slot, Item) ->
	X = get_slot_id(Slot),
	case Item of
	none ->
		ok;
		% вещь не уничтожается. а переходит в руку. как правило.
		%db_server:inventory_remove(CharID, X);
	_ ->
		db_server:inventory_update(Item#item{x=X,y=0}, CharID)
	end.
%% -------------------------------------------------------------------------------------------------------------
get_slot_id(Slot) ->
	case Slot of
		head 	-> 201;
		body 	-> 202;
		lhand 	-> 203;
		rhand 	-> 204;
		legs 	-> 205;
		foots 	-> 206
	end.
%% -------------------------------------------------------------------------------------------------------------
get_slot(ID) ->
	case ID of
		201 -> head;
		202 -> body;
		203 -> lhand;
		204 -> rhand;
		205 -> legs;
		206 -> foots
	end.
%% -------------------------------------------------------------------------------------------------------------
set_slot(Slot, Item, Equip) ->
	case Slot of
		head -> Equip#equip{head=Item};
		body -> Equip#equip{body=Item};
		lhand -> Equip#equip{lhand=Item};
		rhand -> Equip#equip{rhand=Item};
		legs -> Equip#equip{legs=Item};
		foots -> Equip#equip{foots=Item};
		_ -> Equip
	end.
%% -------------------------------------------------------------------------------------------------------------
% return {TakedItem, NewEquip}
take_slot(Slot, Equip) ->
	case Slot of
		head -> #equip{head=Item}=Equip, {Item, Equip#equip{head=none}};
		body -> #equip{body=Item}=Equip, {Item, Equip#equip{body=none}};
		lhand -> #equip{lhand=Item}=Equip, {Item, Equip#equip{lhand=none}};
		rhand -> #equip{rhand=Item}=Equip, {Item, Equip#equip{rhand=none}};
		legs -> #equip{legs=Item}=Equip, {Item, Equip#equip{legs=none}};
		foots -> #equip{foots=Item}=Equip, {Item, Equip#equip{foots=none}};
		_ -> {none, Equip}
	end.
