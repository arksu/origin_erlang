%% Author: arksu
%% Created: 12.11.2011
%% Description: buffs for chars
-module(buff).

-include("types.hrl").

-export([get_typeid/1, get_type/1, load_buffs/1, get_duration/1, get_cast_time/1, get_buff/2
		 ]).

-include("defines.hrl").

%%---------------------------------------------------------------------------------------
% получить длительность бафа
get_duration(Type) ->
	case Type of
		war_buff -> 25000;
		war_debuff -> 25000;
		god_buff -> 1000 * 60 * 60; % 1 hour
		_ ->
			?ERROR_MSG("wrong_type_buff"),
			exit(wrong_type_buff)
	end.

%%---------------------------------------------------------------------------------------
% получить время на кастование бафа
get_cast_time(Type) ->
	case Type of
		war_buff -> 200;
		war_debuff -> 200;
		god_buff -> 1000;
		_ ->
			?ERROR_MSG("wrong_type_buff"),
			exit(wrong_type_buff)
	end.



%%======================================================================================
% 		СИСТЕМЩИНА
%%======================================================================================
%%---------------------------------------------------------------------------------------
% найти баф указаного типа
get_buff(#player_state{buffs = List}, BuffType) ->
	case lists:keyfind(BuffType, #buff.type, List) of
		false -> none;
		B -> B
	end.

%%---------------------------------------------------------------------------------------
% загрузить список баффов из базы
load_buffs(TargetID) ->
	Rows = db_server:buff_load(TargetID),
	load_buffs_rows(Rows,[]).

load_buffs_rows([], Acc) -> Acc;
load_buffs_rows([H|T], Acc) ->
	[TargetID, TypeID, Duration, StateData] = H,
	State = if size(StateData) =< 1 ->
				   	none;
			true ->
					binary_to_term(StateData)
			end,
	load_buffs_rows(T, [#buff{target_id=TargetID, 
							  duration=Duration, 
							  state=State, 
							  type=get_type(TypeID)
							 }|Acc]).

%% --------------------------------------------------------------------
% get id from type
get_typeid(Type) ->
	case lists:keyfind(Type, 1, get_types_def()) of
		false -> 0;
		{_, TypeID} -> TypeID
	end.

%% --------------------------------------------------------------------
% get type from id
get_type(TypeID) ->
	case lists:keyfind(TypeID, 2, get_types_def()) of
		false -> unknown;
		{Type, _} -> Type
	end.

%% --------------------------------------------------------------------
get_types_def() ->
[
	{war_buff, 		1 },
	{war_debuff, 	2 },
	{god_buff,		3 }
].
