%% Author: arksu
%% Created: 18.12.2011
%% Description: TODO: Add description to trees
-module(trees).

-include("types.hrl").

-export([
		 spawn_branch_leaf/11,
		 action_complete/6
		]).
-include("defines.hrl").

% сколько бревен дает дерево
get_logs_count(Type) ->
	case Type of
		oak_tree -> 4;
		_ -> 2
	end.

% сколько веток заспавнить после рубки дерева
get_brach_chop(Type) ->
	case Type of
		oak_tree -> {branch_leaf, 	20, 	40, 120};
		_ -> {branch_leaf, 			10,		40, 60}
	end.

%##################################################################################################################
%##################################################################################################################


%% --------------------------------------------------------------------
% срубить дерево
action_complete(State, ActionName, #object{id=ObjID, type=Type, coord={X,Y,Lv}, q=Q }, From, FromState, FromObjID) ->
	?DEBUG("obj_action_completed ~p", [ActionName]),
    case ActionName of
	take_branch ->
		From ! {ack, {action_end}},
		From ! {gain_exp, #exp{industry=0,nature=5}},
		From ! {player_add_items, [inventory:spawn_item(branch, Q, 1)]},
		State;
	take_apple ->
		From ! {ack, {action_end}},
		if Type == apple_tree -> 
		From ! {gain_exp, #exp{industry=0,nature=5}},
		From ! {player_add_items, [inventory:spawn_item(apple, Q, 1)]},
		State;			   
		true -> State end; 
	chop ->
		case equip:type_in_hands(stone_axe, FromState) of
			true ->
				% tell action is end
				From ! {ack, {action_end}},
				From ! {gain_exp, #exp{industry=2,nature=12}},


				St2 = grid:object_changed(
						State, ObjID, {X,Y,Lv},
						#object{id=ObjID, type=stump, coord={X,Y,Lv}}
										 ),

				% ищем объект который срубил дерево
				% TODO : object can be in other grid!
				case grid:find_object_neighbors(State, FromObjID) of 
					none -> St2;
					#object{coord={FX,FY,_}} ->
						A = math:atan2(FY-Y, FX-X),
						A1 = if A<0 -> 2*math:pi()+A; true -> A end,
						A2 = round((2*A1) / math:pi()) rem 4,
						?DEBUG("dir angle ~p",[A2]),
						{DX,DY, LogName} = case A2 of
							2 -> {1,0, log};
							0 -> {-1, 0, log};
							3 -> {0,1,logy};
							1 -> {0, -1, logy};
							_ -> {1,0, log}
						end,

						FS = spawn_logs(St2, X, Y, Lv, DX, DY, Q, LogName, get_logs_count(Type)),
						{Branch, BranchCount, BW, BH} = get_brach_chop(Type),
						spawn_branch_leaf(FS, BW, BH, Branch, X,Y,Lv,DX, DY, Q, BranchCount)
				end;
			false -> From ! {ack, {action_end}}, State
		end;
	_ ->
		From ! {ack, {action_end}}, State
	end.

%% --------------------------------------------------------------------
% заспавнить бревна от дерева
spawn_logs(State, _X, _Y, _Lv, _DX, _DY, _Q, _Type, 0) -> State;
spawn_logs(State, X, Y, Lv, DX, DY, Q, Type, Count) ->
	?DEBUG("spawn logs ~p ~p",[Type, Count]),
	HP = objects_server:get_hp(Type, Q, none),
	RX = X+20*DX*Count,
	RY = Y+20*DY*Count,
	Obj = #object{id=none, type=Type, coord={RX,RY,Lv}, q=Q, hp={HP,HP} },
	spawn_logs(
					grid:object_spawn(State, Obj),
					X, Y, Lv, DX, DY, Q, Type, Count-1
					  ).

%% --------------------------------------------------------------------
% заспавнить ветки листьев в указанном разбросе возле дерева
spawn_branch_leaf(St, _W, _H, _Type, _X,_Y,_Lv,_DX,_DY, _Q, 0) -> St;
spawn_branch_leaf(St, W, H, Type, X,Y,Lv,DX, DY, Q, Count) ->
	spawn_branch_leaf(
	  	spawn_branch_leaf(St, W, H, Type, X,Y,Lv,DX, DY, Q),
	    W, H, Type, X,Y,Lv,DX, DY, Q, Count-1).

%% --------------------------------------------------------------------
% заспавнить одну ветку
spawn_branch_leaf(St, W, H, Type, X,Y,Lv,DX, DY, Q) ->
	{AX,AY} = if (DY == 0) ->
		{(random:uniform(H) ) * DX,
		(random:uniform(W) - (W div 2)) * DX};
	true ->
		{(random:uniform(W) - (W div 2)) * DY,
		(random:uniform(H) ) * DY}
	end,

	%?DEBUG("ax=~p ay=~p",[AX,AY]),


	ID1 = world_server:get_next_id(),
    I1 = #item{id=ID1, type=Type, q=10, amount=1},
	HP = objects_server:get_hp(Type, Q, I1),
    Obj1 = #object{id=ID1, type=Type, coord={X+AX,Y+AY,Lv}, state=I1, q=Q, hp={HP,HP} },
    grid:object_spawn(St, Obj1).