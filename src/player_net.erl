%% Author: arksu
%% Created: 05.08.2012
%% Description: 
-module(player_net).

-compile(export_all).

-include("defines.hrl").
-include("base_io.hrl").
-include("net.hrl").
-include("types.hrl").


%-----------------------------------------------------------------------------------
send_obj_set_param(SendPid, ObjID, {ParamName, Data}) ->
    %?DEBUG("send_obj_set_param ~p ~p", [ObjID, ParamName]),
    case ParamName of
%%         follow ->
%%             send_follow(SendPid, ObjID, Data);
        kin_info ->
            send_kin_info(SendPid, ObjID, Data);
        drawable ->
            send_drawable(SendPid, ObjID, Data);
        line_move ->
            {Speed,X,Y, VX, VY} = Data,
            send_line_move(SendPid, ObjID, Speed, X, Y, VX, VY);
        opened ->
            send_opened(SendPid, ObjID, Data);
        inventory -> ok;
		_ -> send_param(SendPid, ObjID, ParamName, Data)

    end.

send_param(SendPid, ObjID, ParamName, Data) ->
	%?DEBUG("send erlang param ~p ~p", [ObjID, ParamName]),
	send_packet(SendPid, ?GAMESERVER_SET_PARAM,
		[write_int(ObjID), write_string(atom_to_list(ParamName)), write_term(Data)]
	).

send_player_param(SendPid, ParamName, Data) ->
	?DEBUG("send erlang player param ~p", [ParamName]),
	send_packet(SendPid, ?GAMESERVER_SET_PLAYER_PARAM,
		[write_string(atom_to_list(ParamName)), write_term(Data)]
	).

send_opened(SendPid, ObjID, List) ->
    ?DEBUG("send_opened ~p ~p", [ObjID, List]),
    if length(List) > 0 ->
        send_packet(SendPid, ?GAMESERVER_SET_OPENED,
                [write_int(ObjID)]
               );
       true -> send_obj_remove_param(SendPid, ObjID, opened)
    end.

send_follow(SendPid, ObjID, {ToObjID,OX,OY,AddZ}) ->
    ?DEBUG("send_follow ~p ~p ~p ~p ~p", [ObjID, ToObjID,OX,OY,AddZ]),
    send_packet(SendPid, ?GAMESERVER_SET_FOLLOW,
                [write_int(ObjID),
                 write_int(ToObjID),
                 write_int(OX),
                 write_int(OY),
                 write_int(AddZ)
                ]
               ).
send_kin_info(SendPid, ObjID, Name) ->
    %?DEBUG("send_kin_info ~p ~p", [ObjID, Name]),
    send_packet(SendPid, ?GAMESERVER_SET_KIN_INFO,
                [write_int(ObjID), write_string(Name)]
               ).
send_drawable(SendPid, ObjID, Name) ->
    %?DEBUG("send_drawable ~p ~p", [ObjID, Name]),
    send_packet(SendPid, ?GAMESERVER_SET_DRAWABLE,
                [write_int(ObjID), write_string(Name)]
               ).

%-----------------------------------------------------------------------------------
send_place_object(SendPid, Name) ->
    %?DEBUG("send_place_object ~p", [Name]),
    send_packet(SendPid, ?GAMESERVER_PLACE_OBJECT,
                [write_string(Name)]
               ).

%---------------------------------------------------------------------------
send_obj_remove_param(SendPid, ObjID, ParamName) ->
    %?DEBUG("send_obj_remove_param ~p ~p", [ObjID, ParamName]),
    P = case ParamName of
            kin_info -> 1;
            follow -> 2;
            light -> 3;
            opened -> 4;
			links -> 5;
            inventory -> none
        end,
    if P =/= none ->
    send_packet(SendPid, ?GAMESERVER_OBJ_DELETE_PARAM,
                [write_int(ObjID),
                 write_int(P)]
               )
    end.
%---------------------------------------------------------------------------
send_obj_clear_params(SendPid, ObjID) ->
    send_packet(SendPid, ?GAMESERVER_OBJ_CLEAR_PARAMS,
                [write_int(ObjID)]
               ).
%---------------------------------------------------------------------------
send_obj_move(SendPid, ObjID, X, Y) ->
    %?DEBUG("send_obj_move id=~p x=~p y=~p", [ObjID, X, Y]),
    send_packet(SendPid, ?GAMESERVER_OBJ_MOVE,
                [write_int(ObjID),
                 write_int(round(X)),
                 write_int(round(Y))]
               ).
%---------------------------------------------------------------------------
send_obj_type(SendPid, ObjID, Type) ->
    %?DEBUG("send_obj_move id=~p x=~p y=~p", [ObjID, X, Y]),
    send_packet(SendPid, ?GAMESERVER_OBJ_TYPE,
                [write_int(ObjID),
                 write_string(Type)]
               ).
%---------------------------------------------------------------------------
send_line_move(SendPid, ObjID, Speed, X, Y, VX, VY) ->
    %?DEBUG("send_line_move ~p ~p ~p ~p ~p ~p", [ObjID, Speed, X, Y, VX, VY]),
    send_packet(SendPid, ?GAMESERVER_OBJ_LINE_MOVE,
                [write_int(ObjID),
                 write_int(round(Speed)),
                 write_int(round(X)),
                 write_int(round(Y)),
                 write_int(round(VX)),
                 write_int(round(VY))]
               ).
%---------------------------------------------------------------------------
send_obj_remove(SendPid, ObjID) ->
    %?DEBUG("send_obj_remove ~p", [ObjID]),

	send_packet(SendPid, ?GAMESERVER_OBJ_DELETE,
                [write_int(ObjID)]
               ).
%---------------------------------------------------------------------------
send_context_menu(SendPid, X,Y, Items) ->
    ?DEBUG("send_context_menu ~p ~p ~p", [Items, X,Y]),
    send_packet(SendPid, ?GAMESERVER_CONTEXT_MENU,
                [write_int(round(X)),
                 write_int(round(Y)),
                 write_int(length(Items)),
                 list_string_to_binary(Items)]
               ).
%---------------------------------------------------------------------------
send_progress(SendPid, P) ->
    %?DEBUG("send_progress ~p", [P]),
    send_packet(SendPid, ?GAMESERVER_PROGRESS,
                [write_int(P)]
               ).
%---------------------------------------------------------------------------
send_cursor(SendPid, Cursor) ->
    ?DEBUG("send_cursor ~p", [Cursor]),
    send_packet(SendPid, ?GAMESERVER_CURSOR,
                [write_string(Cursor)]
               ).
%---------------------------------------------------------------------------
send_area_say(SendPid, ObjID, Msg) ->
    ?DEBUG("send_area_say ~p ~p", [ObjID, Msg]),
    send_packet(SendPid, ?GAMESERVER_SERVER_SAY,
                [write_int(0), % channnel
                 write_int(ObjID),
                 write_string(Msg)
                 ]
               ).
%---------------------------------------------------------------------------
send_system_say(SendPid, Msg) ->
    ?DEBUG("send_system_say ~p", [Msg]),
    send_packet(SendPid, ?GAMESERVER_SERVER_SAY,
                [write_int(1), % channnel
                 write_string(Msg)
                 ]
               ).
%---------------------------------------------------------------------------
send_system_msg(SendPid, Msg) ->
    ?DEBUG("send_system_msg ~p", [Msg]),
    send_packet(SendPid, ?GAMESERVER_SYSTEM_MSG,
                [write_string(Msg)
                 ]
               ).
%---------------------------------------------------------------------------
send_fly_text(SendPid, X,Y,Code,Msg) ->
    ?DEBUG("send_fly_text ~p ~p ~p ~p", [X,Y,Code,Msg]),
    send_packet(SendPid, ?GAMESERVER_FLY_TEXT,
                [write_int(round(X)),write_int(round(Y)), write_int(Code), write_string(Msg)
                 ]
               ).
%---------------------------------------------------------------------------
send_actions_list(_, []) -> ok;
send_actions_list(SendPid, Actions) ->
    {Data, Count} = prepare_send_actions_list(Actions,[],0),
    %?DEBUG("send_actions_list ~p ~p", [Data, Count]),
    send_packet(SendPid, ?GAMESERVER_ACTIONS_LIST,
                [write_int(Count),
                 Data]
               ).
prepare_send_actions_list([],Acc,Count) -> {Acc,Count};
prepare_send_actions_list(Actions,Acc,Count) ->
   [{Parent, Item}|T] = Actions,
   prepare_send_actions_list(T,[write_string(Parent),write_string(Item)|Acc], Count+1).
%---------------------------------------------------------------------------
send_gain_exp(SendPid, ObjID, E1, E2, E3) ->
    ?DEBUG("send_gain_exp ~p ~p ~p ~p", [ObjID, E1, E2, E3]),
    send_packet(SendPid, ?GAMESERVER_GAIN_EXP,
                [
                 write_int(ObjID),
                 write_int(E1),
				 write_int(E2),
				 write_int(E3)
                 ]
               ).
%---------------------------------------------------------------------------
send_map_data(SendPid, MyGrids, X,Y, Data) ->
    % ?DEBUG("send_map_data ~p ~p ~p", [X,Y,size(Data)]),
    GridCoords = fill_grids_coords(MyGrids, []),
	
	GZ = zlib:gzip(Data),
	
    send_packet(SendPid, ?GAMESERVER_MAP_DATA,
                [write_int(X), write_int(Y), write_int(length(MyGrids)), GridCoords,
				 write_int(size(GZ)), 
				 GZ
				]
               ).
%---------------------------------------------------------------------------
send_buffs(_,[]) -> ok;
send_buffs(SendPid, [H|T]) ->
	send_buff_add(SendPid, H),
	send_buffs(SendPid, T).
%---------------------------------------------------------------------------
send_buff_add(SendPid, Buff) ->
    ?DEBUG("send_buff_add ~p", [Buff]),
    send_packet(SendPid, ?GAMESERVER_BUFF_ADD,
                [write_int(Buff#buff.duration),
				 write_int(Buff#buff.time),
				 write_int(Buff#buff.target_id),
				 write_string(Buff#buff.type),
				 write_term(Buff#buff.state) ]
               ).
%---------------------------------------------------------------------------
send_buff_delete(SendPid, TargetID, Type) ->
    ?DEBUG("send_buff_delete ~p", [Type]),
    send_packet(SendPid, ?GAMESERVER_BUFF_DELETE,
                [write_int(TargetID),
				 write_string(Type) ]
               ).
%---------------------------------------------------------------------------
send_target(SendPid, none) ->
    ?DEBUG("send_target ~p", [none]),
    send_packet(SendPid, ?GAMESERVER_TARGET,
                [write_int(0)]
               );
%---------------------------------------------------------------------------
send_target(SendPid, Object) ->
    ?DEBUG("send_target ~p", [Object]),
    send_packet(SendPid, ?GAMESERVER_TARGET,
                [write_int(Object#object.id) ]
               ).
%---------------------------------------------------------------------------
send_reuse(SendPid, {Timeout, Len}) ->
    ?DEBUG("send_reuse ~p ~p", [Timeout, Len]),
    send_packet(SendPid, ?GAMESERVER_REUSE_TIME,
                [write_int(Timeout), write_int(Len) ]
               ).
%---------------------------------------------------------------------------
send_current_speed(SendPid, Speed) ->
    ?DEBUG("send_current_speed ~p", [Speed]),
    send_packet(SendPid, ?GAMESERVER_CLIENT_SPEED,
                [write_int(Speed) ]
               ).
%---------------------------------------------------------------------------
send_all_skills(_, []) -> ok;
send_all_skills(SendPid, [K|T]) ->
 	send_knw(SendPid, K),
	send_all_skills(SendPid, T).

send_knw(SendPid, #knowledge{base=B, level=L, type=T, skills=S}) ->
    send_packet(SendPid, ?GAMESERVER_KNOWLEDGE, [
							write_string(T),
							write_string(B),
							write_int(L),
							write_int(length(S)),
							make_skills(S, [])
												]).
make_skills([], Acc) -> Acc;
make_skills([#skill{type=T, level=L, bar=B, max_bar=MB, req_exp=RE}|Tail], Acc) ->
	make_skills(Tail, [
			write_string(T),
			write_int(L),
			write_int(B),
			write_int(MB),
			write_int(RE)
					   | Acc
				   ]).
	
%-----------------------------------------------------------------------------
send_stat(#player_state{send_pid=SendPid, stats=Stats, exp=Exp} = State) ->
	player_net:send_player_param(SendPid, stat, {Exp, Stats, player:get_max_fep(State)}).
%-----------------------------------------------------------------------------
send_close_object(SendPid, ObjID) ->
    send_packet(SendPid, ?GAMESERVER_OBJECT_CLOSE, [
							write_int(ObjID)
												]).
	
%-----------------------------------------------------------------------------
send_object_visual_state(SendPid, #object{id=ObjID, type=Type} = O) ->
	{T, Pkt} = objects_server:visual_state_packet(O),
	P = if is_list(Pkt) -> list_to_binary(Pkt); true -> Pkt end,
	%?DEBUG("send_object_visual_state ~p",[P]),
    send_packet(SendPid, ?GAMESERVER_OBJECT_VISUAL_STATE, [
							write_int(ObjID),
							write_string(Type),
							write_string(T),
							write_int(size(P)), P
												]).


%---------------------------------------------------------------------------
fill_grids_coords([],Acc) ->
    Acc;
fill_grids_coords(MyGrids,Acc) ->
    [{Sg, Grid}|T] = MyGrids,
    {X,Y,_Lv} = map_server:get_map_coord(Sg, Grid),
    fill_grids_coords(T, [[write_int(X),write_int(Y)]|Acc]).

%---------------------------------------------------------------------------
send_claim_remove(SendPid, OwnerID) ->
    ?DEBUG("send_claim_remove ~p", [OwnerID]),
    send_packet(SendPid, ?GAMESERVER_CLAIM_REMOVE,
                [write_int(OwnerID) ]
               ).
send_claim_change(SendPid, #claim_personal{owner_id=OwnerID, object_id=ObjectID, rect={L,T,R,B}} = C) ->
    ?DEBUG("send_claim_change ~p", [C]),
    send_packet(SendPid, ?GAMESERVER_CLAIM_CHANGE,
                [write_int(OwnerID), write_int(ObjectID),
				 write_int(L),
				 write_int(T),
				 write_int(R),
				 write_int(B)
				]
               ).

%---------------------------------------------------------------------------
send_inventory(SendPid, OwnerID, {W, H}, List) ->
	Pkt = make_inventory_pkt(List, []),
	L = if is_list(List) -> length(List); true -> 0 end,
%%     ?DEBUG("send_inventory ~p ~p ~p list: ~p pkt: ~p", [OwnerID, W, H, List, Pkt]),
    send_packet(SendPid, ?GAMESERVER_INVENTORY,
                [write_int(OwnerID), 
				 write_byte(W),
				 write_byte(H),
				 write_word(L),
				 Pkt
				]
               ).
make_inventory_pkt([], Acc) -> Acc;
make_inventory_pkt([Item|T], Acc) ->
	P = case Item of
		none -> [];
		[] -> [];
		_ -> make_item_pkt(Item)
	end,
	make_inventory_pkt(T, [P|Acc]).

make_item_pkt(#item{id=Id, q=Q, amount=Amount, items=List, num=Num, ticks=Ticks, ticks_left=TicksLeft, type=Type, x=X, y=Y} = Item) ->
	{W,H} = inventory:item_inventory_size(Type),
	{Icon,Hint} = inventory:item_get_icon_hint(Item),
	[
	 write_int(Id),
	 write_int(cutils:get_object_typeid(Type)),
	 write_word(Q),
	 write_byte(X),
	 write_byte(Y),
	 write_byte(W),
	 write_byte(H),
	 write_string(Icon),
	 write_string(Hint),
	 write_int(Amount),
	 write_int(Num),
	 write_int(Ticks),
	 write_int(Ticks-TicksLeft),
	 case inventory:obj_have_inventory(Type) of 
	 true -> 
		{IW, IH} = inventory:obj_inventory_size(Type),
		if 
			is_list(List) -> 
				{IW, IH} = inventory:obj_inventory_size(Type),
				[<<1>>, write_byte(IW), write_byte(IH), write_word(length(List)), make_inventory_pkt(List, [])];	
			true -> 
				[<<1>>, write_byte(IW), write_byte(IH), write_word(0)]
		end; 
	 false -> <<0>> 
	 end].
	
%---------------------------------------------------------------------------
send_set_hand(SendPid, ObjID, W, H, OX, OY, Img) ->
    send_packet(SendPid, ?GAMESERVER_SET_HAND,
                [write_int(ObjID), 
				 write_byte(W),
				 write_byte(H),
				 write_byte(OX),
				 write_byte(OY),
				 write_string(Img)
				]
               ).	
send_set_hand(SendPid, none) ->
    send_packet(SendPid, ?GAMESERVER_SET_HAND,
                [write_int(0) 
				]
               ).	
	
%---------------------------------------------------------------------------
send_equip(SendPid, #equip{body=Body, foots=Foots, head=Head, legs=Legs, lhand=LHand, rhand=RHand}) -> 
    send_packet(SendPid, ?GAMESERVER_EQUIP, [
			make_equip_slot_pkt(Head),
			make_equip_slot_pkt(Body),
			make_equip_slot_pkt(LHand),
			make_equip_slot_pkt(RHand),
			make_equip_slot_pkt(Legs),
			make_equip_slot_pkt(Foots)
											]).	

make_equip_slot_pkt(Slot) when Slot == none -> [write_int(0)];
make_equip_slot_pkt(Slot) -> make_item_pkt(Slot).
	