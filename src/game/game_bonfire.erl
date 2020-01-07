%% Author: arksu
%% Created: 05.09.2012
%% Description: TODO: Add description to bonfire
-module(game_bonfire).

-include("types.hrl").
-include("objects.hrl").

-export([
		 	obj_action/5,
			obj_action_completed/5,
			update_tick/2,
			get_fuel_list/1
		]).

-include("defines.hrl").

%%----------------------------------------------------------------------------------------------------
obj_action(_State, Action, _FromState, _From, Object) ->
	case Action of
    	right_click -> {action, open, 0, Object#object.id};
		fire -> {action, Action, 2000, Object#object.id};
		{inv_click, _} -> {action, Action, 0, Object#object.id};
		{fuel_click, _} -> {action, Action, 0, Object#object.id};
    	_ -> {none}
   	end.	

%%----------------------------------------------------------------------------------------------------
obj_action_completed(State, ActionName, From, FromState, #object{coord=C, state=ObjState} = Object) ->
	case ActionName of
	    open ->
	        From ! {ack, {action_end}},
			From ! {obj_visual_state, Object},
			State;
		fire ->
			From ! {ack, {action_end}},
			% проверим есть ли топливо
			#obj_bonfire{fuel=F} = ObjState,
			FL = get_fuel_list(F),
			case FL of
				[] -> State;
				_ -> grid:object_changed(
								State, Object#object.id, C,
								Object#object{state=ObjState#obj_bonfire{fire=true}}
										 )
			end;
		{inv_click, Click} ->
			From ! {ack, {action_end}},
			grid:obj_inventory_click(State, FromState, From, Object, main, Click);
		{fuel_click, Click} ->
			From ! {ack, {action_end}},
			grid:obj_inventory_click(State, FromState, From, Object, fuel, Click);
		{context_item, Action, ItemObjID} ->
			grid:obj_inventory_context(State, FromState, From, Object, main, Action, ItemObjID);
		_ -> From ! {ack, {action_end}}, State
	end.

%%----------------------------------------------------------------------------------------------------
update_tick(_Time, #object{state = #obj_bonfire{fire=Fire, inv=Inv, fuel=Fuel} = S } = Object) -> 
	%?DEBUG("bonfire upd: ~p",[Object]),
	if 
	% если костер не зажжен - ничего не делаем
	Fire == false ->
		?DEBUG("no fire ~p",[Object]),
		{false, Object};
	true ->
		% если зажжен. проверим топливо. сделаем 1 тик текущему топливу
		case make_fuel_tick(Fuel) of
			false ->
				?DEBUG("fuel tick false ~p",[Object]),
				{true, Object#object{state=S#obj_bonfire{fire=false}}};
			{IsEnd, NewFuel} ->
				?DEBUG("fuel tick is end: ~p",[IsEnd]),
				NewInv = case IsEnd of 
					true -> 
						% в инвентаре меняем состоиние на отработавший 1 тик
						make_inv_tick(Inv);
					false -> 
						make_inv_tick(Inv)
				end,
		
				{true, Object#object{state=S#obj_bonfire{fuel=NewFuel, inv=NewInv}}}
		end
	end.

%%----------------------------------------------------------------------------------------------------
% сделать 1 тик в инвентарь. объекты на которые подействовал костер должны изменить свое состояние
% вернет список
make_inv_tick([]) -> [];
make_inv_tick(L) -> make_inv_tick(L, []).
make_inv_tick([], Acc) -> Acc;
make_inv_tick([H|T], Acc) ->
	case is_effect(H#item.type) of
		true -> make_inv_tick(T, [make_effect(H)|Acc]);
		false -> make_inv_tick(T, [H|Acc])
	end.

%%----------------------------------------------------------------------------------------------------
% сделать 1 тик топливу. вернет false если топливо закончилось, {isEnd, NewFuel} если сделали успешный тик 
make_fuel_tick([]) -> false; % топлива нет вообще
make_fuel_tick(L) ->
	% отсортируем топливо по порядку следования в координатах вещей
	FL = get_fuel_list(L),
	case FL of
		% топлива нет 
		[] ->
			?DEBUG("no fuel"),
			false;
		_ ->
			
			?DEBUG("make_fuel_tick: ~p ~p",[L, FL]),
			% ищем первую вещь с ненулевым тиком
			F1 = fun(I) ->
						 if 
							 I#item.ticks == 0 -> true;
							 true -> false 
						 end 
				 end,
			% удаляем все вещи которые не содержат тиков
			L1 = lists:dropwhile(F1, FL),
			case L1 of
				[] ->
					% нет топлива с начавшимся тиком
					?DEBUG("no ticks fuel"),
					% возьмем первую попавшуюся вещь из списка топлива
					[CF|_] = FL,
					Ticks = get_fuel_ticks(CF#item.type),
					NF = CF#item{ticks_left=Ticks-1, ticks=Ticks},
					{false, lists:keyreplace(CF#item.id, #item.id, L, NF)};
				[#item{ticks_left=ITicksLeft, id=ID} = CF|_] ->
					% берем первую вещь с тиком
					if 
						% если вещь сгорела полностью
						ITicksLeft =< 1 -> 
							% меняем эту вещь на золу
							NF = CF#item{type=ash, ticks=0, ticks_left=0, amount=1, items=[]},
							{true, lists:keyreplace(ID, #item.id, L, NF)};
						true ->
							NF = CF#item{ticks_left=ITicksLeft-1},
							{false, lists:keyreplace(ID, #item.id, L, NF)}
					end
			end
	end.
	
get_fuel_list(L) -> lists:sort(fuel_sort_func(), lists:filter(fun(FI) -> is_fuel(FI) end, L)).

%%----------------------------------------------------------------------------------------------------
is_fuel(#item{type=branch}) -> true;
is_fuel(#item{type=board}) -> true;
is_fuel(_) -> false.

%%----------------------------------------------------------------------------------------------------
get_fuel_ticks(branch) -> 4;
get_fuel_ticks(board) -> 16;
get_fuel_ticks(_) -> error(not_fuel_ticks).



%%----------------------------------------------------------------------------------------------------
is_effect(clay) -> true;
is_effect(_) -> false.

get_inv_ticks(clay) -> 8;
get_inv_ticks(_) -> error(not_effect_item).

%%----------------------------------------------------------------------------------------------------
transorm_item(#item{type=clay} = I) -> I#item{type=brick_clay};
transorm_item(_) -> error(transorm_item).

%%----------------------------------------------------------------------------------------------------
% произвести воздействие от сжигания в костре
make_effect(#item{type=Type, ticks=Ticks, ticks_left=TicksLeft} = I) ->
	if 
		% только начали давать эффект на вещь
		Ticks == 0 -> I#item{ticks=get_inv_ticks(Type), ticks_left=get_inv_ticks(Type)-1};
		% тики закончились. вещь надо преобразовать
		TicksLeft =< 1 -> TI = transorm_item(I), TI#item{ticks=0, ticks_left=0};
		% тики еще не закончились
		true -> I#item{ticks_left=TicksLeft-1}
	end;
make_effect(_) -> error(wrong_effect_item).

%%----------------------------------------------------------------------------------------------------
fuel_sort_func() ->
	fun (#item{x=X1, y=Y1}, #item{x=X2, y=Y2}) ->
		if 
			Y2 > Y1 -> false;
			true ->
				if
					X2 > X1 -> false;
					true -> true
				end
		end
	end.