%% Author: arksu
%% Created: 12.04.2011
%% Description: TODO: Add description to object
-module(object).


-export([]).
-compile(export_all).

-include("types.hrl").
-include("defines.hrl").
-include("objects.hrl").

-record(rabbit_state, { self = none, % собственный объект
                        grid = none, % пид грида в котором находимся
						mode = idle, % текущий режим
						coord = error, % мои координаты
						enemy = none, % координаты от кого убегаем
						to_pos = none, % единичный вектор направления смены позиции
						mode_timer = 0 % таймер простоя
                      }).
%--------------------------------------------------------------------
% spawn new simple object process
spawn_dynamic_process(GridPid, #object{type=Type} = Object) ->
    ?DEBUG("spawn object process!!!"), 
	case Type of
	   	rabbit -> spawn_link(?MODULE, rabbit_start, [#rabbit_state{grid = GridPid, self=Object}]);
		
	   	_ -> error({unknown_dyn_object, Type})
   	end.

%--------------------------------------------------------------------
rabbit_start(#rabbit_state{self=#object{coord=MC}} = State) ->
	rabbit_loop(mode_idle(State#rabbit_state{coord=MC})).

%--------------------------------------------------------------------
% RABBIT LOGIC
rabbit_loop(#rabbit_state{
		grid=GridPid, mode=Mode, mode_timer=ModeTimer, coord=MC
						 } = State) ->
	case receive
		{tick, DT, _GT} -> 
			case Mode of
				inactive -> ok;
				idle -> 
					NewIdleTimer = ModeTimer + DT,
%% 					?DEBUG("rabbit idle : ~p",[NewIdleTimer]),
					if 
						NewIdleTimer >= 500000 ->
							mode_change_position(State);
						true ->
							State#rabbit_state{mode_timer=NewIdleTimer}
					end;
				change_pos ->
					NewTimer = ModeTimer + DT,
%% 					?DEBUG("rabbit change pos : ~p",[NewTimer]),
					if 
						NewTimer >= 3000 ->
							mode_idle(update_move(State#rabbit_state{mode_timer=NewTimer}, DT, stop));
						true -> 
							update_move(State#rabbit_state{mode_timer=NewTimer}, DT, move)
					end;
				escape ->
					NewTimer = ModeTimer + DT,
					if 
						NewTimer >= 3000 ->
							mode_idle(update_move(State#rabbit_state{mode_timer=NewTimer}, DT, stop));
						true -> 
							update_move(State#rabbit_state{mode_timer=NewTimer}, DT, move)
					end;
				escape_obstacle ->
					NewTimer = ModeTimer + DT,
					if 
						NewTimer >= 2000 ->
							mode_idle(update_move(State#rabbit_state{mode_timer=NewTimer}, DT, stop));
						true -> 
							update_move(State#rabbit_state{mode_timer=NewTimer}, DT, move)
					end
			end;
			 
		% какой то объект движется
		{dyn_object_pos, #object{type=player, coord=PC}} ->
			D = coord:get_distance(MC, PC),
			if 
				D < 200 ->
					case Mode of
						escape_obstacle -> ok;
						_ ->
							?DEBUG("dyn_object_pos ~p",[PC]),
							mode_escape(State, PC)
					end;
				true ->
					ok
			end;

		{dyn_object_pos, _} -> ok;
			 
		% наш грид деактивирован
		{deactivate_grid, GridPid} ->
			% переходим в неактивный режим
			mode_inactive(State);
		% чужой грид - ничего не делаем
		{deactivate_grid, _} -> ok;
			 
		% активация грида
		{activate_grid, GridPid} -> mode_idle(State);
		{activate_grid, _} -> ok;
			 
		{obj_remove, _} -> ok;			 
			 
		{finish} -> done;
			 
		M -> 
			?WARNING_MSG("rabbit: unhandled msg ~p",[M]),
			ok
		 
	end of
		done -> ?DEBUG("rabbit: exit"), ok;
		ok -> rabbit_loop(State);
		ONew -> rabbit_loop(ONew)
	end.


%--------------------------------------------------------------------
mode_idle(State) -> State#rabbit_state{mode=idle, mode_timer=0, enemy=none}.
	
mode_inactive(State) -> State#rabbit_state{mode=inactive}.

mode_change_position(State) ->
	TX = randoms:get(100)-50,
	TY = randoms:get(100)-50,
	Len = coord:get_distance(0, 0, TX, TY),
	VX = TX/Len, VY = TY/Len,
	update_move(State#rabbit_state{mode = change_pos, to_pos={VX, VY}, mode_timer=0},
								  0, move).

mode_escape(#rabbit_state{coord={Mx,My,_}}= State, {Ex,Ey,_} = Enemy) -> 
	TX = Mx - Ex,
	TY = My - Ey,
	Len = coord:get_distance(0, 0, TX, TY),
	if Len ==0 -> VX = 1, VY = 0;
		   true -> VX = TX/Len, VY = TY/Len
	end,
	update_move(State#rabbit_state{mode = escape, to_pos={VX, VY}, mode_timer=0, enemy=Enemy},
								  0, move).

mode_escape_obstacle(State) -> 
	TX = randoms:get(100)-50,
	TY = randoms:get(100)-50,
	Len = coord:get_distance(0, 0, TX, TY),
	VX = TX/Len, VY = TY/Len,
	update_move(State#rabbit_state{to_pos={VX,VY}, mode=escape_obstacle, mode_timer=0},
			0, move).

update_move(#rabbit_state{
			mode=Mode, coord={OldX, OldY, Lv}, to_pos={VX, VY}, grid=WG, self=#object{id=ObjID}} = State, 
			DT, Continue) ->
	?DEBUG("rabbit : update_move ~p ~p",[OldX, OldY]),
	Speed = case Mode of
				change_pos -> 20;
				_ -> 60
			end,
	DLen = Speed * (DT / 1000),

	% calc new position
	TmpX = round(OldX + VX * DLen),
	TmpY = round(OldY + VY * DLen),
	
	GP = world_server:get_grid(TmpX, TmpY, Lv),
	GP ! {is_active, self()},
	receive
		{is_active_ack, IsActive} ->
			case IsActive of
			false ->
				case Mode of 
					change_pos -> mode_idle(State);
					escape -> mode_escape_obstacle(State);
					escape_obstacle -> mode_idle(State);
					_ -> State
				end;
			true ->
					% вектор движения для клиента
					MX = round(TmpX + VX * 100), MY = round(TmpY + VY * 100),
					
					SetParam = case Continue of 
								   stop -> pos;
								   move -> {line_move, {Speed,TmpX,TmpY, MX, MY}}
							   end,
					
					{NX, NY, NMode} = case player_server:try_request_move(WG, 
								{move_request, self(), 
								 	ObjID, 
								 	rabbit, none, OldX, OldY, TmpX,TmpY, SetParam, ?MOVE_LAND}, 
								?MOVE_TRY_COUNT) 
					of
						{no_collision} -> 
							{TmpX, TmpY, move};
						{collision_obj, CX, CY, _, _} ->
							% we stopped
				            ?DEBUG("collision! x=~p y=~p",[CX, CY]),
							{CX, CY, stay};
				
						{collision_tile, CX, CY, _TileID} ->
							{CX, CY, stay};
				
						{collision_virtual, _CX, _CY} ->
							error(rabbit_virtual)
					end,
				
					
					S1 = State#rabbit_state{grid=world_server:get_grid(NX, NY, Lv)},
					
					case NMode of
						stay -> case Mode of 
									change_pos -> mode_idle(S1#rabbit_state{coord={NX, NY, Lv}});
									escape -> mode_escape_obstacle(S1#rabbit_state{coord={NX, NY, Lv}});
									escape_obstacle -> mode_idle(S1#rabbit_state{coord={NX, NY, Lv}});
									_ -> S1#rabbit_state{coord={NX, NY, Lv}}
								end;
						move ->
							S1#rabbit_state{coord={NX, NY, Lv}}
					end
			end
	after 500 ->
			mode_idle(State)
	end.


	
	
	
	
	
	
	
	
	