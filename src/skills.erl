%% Author: arksu
%% Created: 04.08.2012
%% Description: 
-module(skills).


-export([
		 get_knw_base/1,
		 load_knowledges/1,
		 knowledge_buy/3,
		 skill_buy/3,
		 skill_gain_ap/3,
		 get_skill_type2id/1,
		 get_knw_type2id/1,
		 get_skill/2,
		 get_actions_def/0
		 ]).

-include("defines.hrl").
-include("types.hrl").



set_skill_max_bar(#skill{level=L} = S) -> S#skill{max_bar = L*10}.

%-----------------------------------------------------------------------------------------------
% получить условия для покупки скилла
get_skill_cond(Skill) ->
	case Skill of
		fishing -> 			[{exp, 1000}, {knw, agriculture, 5}]; % скилл требует 1000 exp, agriculture>=5
		hunting -> 			[{exp, 500}];
		stone_cutting -> 	[{exp, 500}];
		stone_trim -> 		[{exp, 1000}, {skill, stone_cutting, 2}]; % нужно экспы. и уровень stone_cutting >= 2
		masonry -> 			[{exp, 250}];
		ceramics -> 		[{exp, 700}];
		glassmaking -> 		[{exp, 1500}];
		brawling -> 		[{exp, 1500}];
		bushido -> 			[{exp, 5000}];
		_ -> []
	end.

%---------------------------------------------------------------------------
% какие действия дают скиллы в меню действий
get_actions_def() ->
	[
	 		{farming, [{"root","harvest"}] },
			{stone_cutting, [ {"root","root_stoneworking"}, {"root_stoneworking", "make_runestone"} ] }
	
	 ].

%-----------------------------------------------------------------------------------------------
formula_knw_up(KnwLv) -> KnwLv * 100.
formula_knw_down(KnwLv) -> (KnwLv-1) * 50.					

%-----------------------------------------------------------------------------------------------
% SYSTEM #######################################################################################
%-----------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------
% есть ли такой скилл у игрока и его уровень выше 0
% вернет уровень скилла или false
get_skill(Skills, SkillName) when is_record(Skills, player_state) -> #player_state{skills=L} = Skills, get_skill(L, SkillName);
get_skill(L, SkillName) when is_list(L) ->
	KnwType = get_skill_knw(SkillName),
	% ищем знание
	case lists:keytake(KnwType, #knowledge.type, L) of
		false -> error(knw_not_exist);
		{value, #knowledge{skills=KS}, _Tail} ->
			% ищем скилл
			case lists:keytake(SkillName, #skill.type, KS) of
				false -> 
					% такого скилла нет. скорее всего еще не взяли или не купили знание чтобы он стал доступным
					false;
				{value, #skill{level=SL}, _SkillsTail} ->
					if 
						SL > 0 -> SL;
						true -> false
					end
			end
	end.
	

%-----------------------------------------------------------------------------------------------
% добавить очки действий в нужный скилл, передается без изменений от действия
skill_gain_ap(#player_state{skills=L, send_pid=SendPid} = State, 
			  SkillName, AP) ->
	KnwType = get_skill_knw(SkillName),
	case lists:keytake(KnwType, #knowledge.type, L) of
		false -> error(knw_not_exist);
		{value, #knowledge{skills=KS, level=KnwLv}=K, Tail} ->
			case lists:keytake(SkillName, #skill.type, KS) of
				false -> 
					% такого скилла нет. скорее всего еще не взяли или не купили знание чтобы он стал доступным
					State;
				{value, #skill{bar=CV, max_bar=MV, level=SL, id=SkillID}=S, SkillsTail} ->
					NS = if 
						% скилл еще не взят. ничего не делаем
						(SL < 1) ->
							S;
						% уровень знания ниже уровня скилла. ничего не делаем. софткап.
						(KnwLv < SL) ->
							S;
						% бар заполнен. но уровень скилла уже равен уровню знания. софткап
						(CV+AP >= MV) andalso (KnwLv == SL) ->
							db_server:skill_update(SkillID, SL, MV),
							S#skill{bar=MV};							
						% если бар заполнен апнем уровень скилла, уровень знания выше уровня скилла
						(CV+AP >= MV) andalso (KnwLv > SL) ->
							db_server:skill_update(SkillID, SL+1, 0),
							set_skill_max_bar(S#skill{level=SL+1, bar=0});
						% бар не заполнен. добавим ап в бар
						true ->
							db_server:skill_update(SkillID, SL, CV+AP),
							S#skill{bar=CV+AP}
					end,
					NK = K#knowledge{skills=[NS|SkillsTail]},
					NewSkills = [NK|Tail],
					case player:is_dialog_open(State, "dlg_stat") of  
						true -> player_net:send_all_skills(SendPid, NewSkills);
						false -> ok
					end,
					State#player_state{skills = NewSkills}
			end
	end.

%-----------------------------------------------------------------------------------------------
% получить знание к которму закреплен скилл
get_skill_knw(SkillName) ->
	get_skill_knw(get_knw_def(), SkillName).
get_skill_knw([], _) -> error(knw_not_exist_for_skill);
get_skill_knw([{_,Type,_,Skills}|Tail], SkillName) ->
	case lists:keyfind(SkillName, 1, Skills) of
		false -> get_skill_knw(Tail, SkillName);
		_ -> Type
	end.
	
%-----------------------------------------------------------------------------------------------
% получить список скиллов которые дает знание учитывая уровень знания
get_knw_skills(#knowledge{type=Type, level=L}) ->
	case lists:keyfind(Type, 2, get_knw_def()) of
		false -> error(knw_not_exist);
		{_,_,_,List} -> 
			get_knw_skills(List, L, [])
	end.
get_knw_skills([],_,Acc) -> Acc;
get_knw_skills([  {Name, SkillLv}  |T], Lv, Acc) when Lv >= SkillLv -> get_knw_skills(T, Lv, [Name|Acc]);
get_knw_skills([_|T], Lv, Acc) -> get_knw_skills(T, Lv, Acc). 

%-----------------------------------------------------------------------------------------------
% получить направление знания
get_knw_base(Type) ->
	case lists:keyfind(Type, 2, get_knw_def()) of
		false -> error;
		{_,_,T,_} -> T
	end.

%-----------------------------------------------------------------------------------------------
knowledge_buy(#player_state{skills=L, send_pid = SendPid, exp=Exp, charid=CharID} = State, 
			   inc, KnwName) ->
	?DEBUG("knowledge_buy inc ~p ~p",[KnwName, L]),
	case lists:keytake(list_to_atom(KnwName), #knowledge.type, L) of
		false -> error(knw_not_exist);
		{value, #knowledge{level=Lv, skills=KS, type=KType, id=KnwID}=K, Tail} ->
			% проверим хватает ли экспы для поднятия уровня
			ReqExp = formula_knw_up(Lv),
			Base = get_knw_base(KType),
			HaveExp = player:get_exp_val(Exp, Base),
			if 
				HaveExp >= ReqExp ->
					NewKnwID = if 
						Lv == 0 -> db_server:knowledge_add(CharID, KType, 1), db_server:knowledge_get_id(CharID, KType);
						Lv > 0 -> db_server:knowledge_update(KnwID, Lv+1), KnwID;
						true -> KnwID
					end,
					
					AllS = get_knw_skills(K#knowledge{level=Lv+1}),
					% надо добавить в общий список те которые доступны но еще не взяты и их нет в бд
					NKS = merge_available_skills(KS, AllS),
					NK = K#knowledge{level=Lv+1, skills=NKS, id=NewKnwID},
					
					NewSkills = [NK|Tail],
					case player:is_dialog_open(State, "dlg_stat") of  
						true -> player_net:send_all_skills(SendPid, NewSkills);
						false -> ok
					end,
					NewExp = player:add_exp(Exp, Base, -ReqExp),
					db_server:player_update_exp(CharID, NewExp),
					S1 = State#player_state{skills = NewSkills, exp=NewExp},
					player_net:send_stat(S1),
					S1;
				true ->
					player_net:send_system_msg(SendPid, "havnt required exp for knw up"),
					State
			end
	end;
knowledge_buy(#player_state{skills=L, send_pid = SendPid, exp=Exp, charid=CharID} = State, 
			   dec, KnwName) ->
	?DEBUG("knowledge_buy inc ~p ~p",[KnwName, L]),
	case lists:keytake(list_to_atom(KnwName), #knowledge.type, L) of
		false -> error(knw_not_exist);
		{value, #knowledge{level=Lv, type=KType, skills=KS, id=KnwID}=K, Tail} ->
			% вернем часть экспы игроку
			GainExp = formula_knw_down(Lv),
			Base = get_knw_base(KType),

			if 
				Lv > 1 ->				
					db_server:knowledge_update(KnwID, Lv-1),
					% надо убрать скиллы которые еще не изучены и которые не попадают под уровень знания
					OldKS = remove_zero_skills(KS, []),
					AllS = get_knw_skills(K#knowledge{level=Lv-1}),
					NKS = merge_available_skills(OldKS, AllS),
					
					NK = K#knowledge{level=Lv-1, skills=NKS},
					
					NewSkills = [NK|Tail],
					case player:is_dialog_open(State, "dlg_stat") of  
						true -> player_net:send_all_skills(SendPid, NewSkills);
						false -> ok
					end,
					NewExp = player:add_exp(Exp, Base, GainExp),
					db_server:player_update_exp(CharID, NewExp),
					S1 = State#player_state{skills = NewSkills, exp=NewExp},
					player_net:send_stat(S1),
					S1;
				true ->
					player_net:send_system_msg(SendPid, "level of knw is too low"),
					State
			end

	end.

remove_zero_skills([], Acc) -> Acc;
remove_zero_skills([#skill{level=0}|T], Acc) -> remove_zero_skills(T, Acc);
remove_zero_skills([H|T], Acc) -> remove_zero_skills(T, [H|Acc]).
%-----------------------------------------------------------------------------------------------
% купить скилл
skill_buy(#player_state{skills=L, send_pid=SendPid, exp=Exp, access_level=AccessLevel, charid=CharID} = State, 
		   KnwName, SkillName) ->
	?DEBUG("skill_buy ~p ~p",[KnwName, SkillName]),
	case lists:keytake(list_to_atom(KnwName), #knowledge.type, L) of
		false -> error(knw_not_exist);
		{value, #knowledge{skills=KS, type=KType, id=KnwID}=K, Tail} ->
			case lists:keytake(list_to_atom(SkillName), #skill.type, KS) of
				false -> error(skill_not_exist);
				{value, #skill{type=SkillType, level=0}=S, STail} ->
					% надо проверить условия покупки скилла
					Base = get_knw_base(KType),
					HaveExp = player:get_exp_val(Exp, Base),
					Cond = get_skill_cond(SkillType),
					ReqExp = case lists:keyfind(exp, 1, Cond) of
								 false -> 0;
								 {_, Q} -> Q
							 end,
					if 
						HaveExp >= ReqExp ->
									KnwCond = lists:keydelete(exp, 1, Cond),
									case test_skill_buy_cond(L,KnwCond) of
										ok ->					
											% меняем уровень скилла на 1, ставим бар
											db_server:skill_add(KnwID, SkillType, 1, 0),
											NewID = db_server:skill_get_id(KnwID, SkillType),
											NS = [set_skill_max_bar(S#skill{level=1, id=NewID}) | STail],
											NK = K#knowledge{skills=NS},
											
											NewExp = player:add_exp(Exp, Base, -ReqExp),
											db_server:player_update_exp(CharID, NewExp),
											
											NewSkills = [NK|Tail],
											case player:is_dialog_open(State, "dlg_stat") of  
												true -> player_net:send_all_skills(SendPid, NewSkills);
												false -> ok
											end,
											player_net:send_actions_list(SendPid, player:get_actions(NewSkills,AccessLevel)),
											S1 = State#player_state{skills = NewSkills, exp=NewExp},
											player_net:send_stat(S1),
											S1;
										Reason ->
											player_net:send_system_msg(SendPid, Reason),
											State
									end;
						true ->
							player_net:send_system_msg(SendPid, "havnt required exp for buy skill"),
							State
					end;
							
				{value, _, _} -> error(skill_level_not_zero)
			end
	end.

% проверить все уровни знаний нужных для покупки скилла
test_skill_buy_cond(_, []) -> ok;
test_skill_buy_cond(Knw, [{knw, KName, KLv}|Tail]) ->
	case lists:keyfind(KName, #knowledge.type, Knw) of
		false -> "no knw "++atom_to_list(KName);
		#knowledge{level=Lv} ->
			if 
				Lv >= KLv -> test_skill_buy_cond(Knw, Tail);
				true -> "knw level too low "++atom_to_list(KName) ++ " ("++integer_to_list(KLv)++")"
			end
	end;
test_skill_buy_cond(List, [{skill, SkillName, SkillLv}|Tail]) ->
	case get_skill(List, SkillName) of
		false -> "no skill "++atom_to_list(SkillName);
		Lv ->
			if 
				Lv >= SkillLv -> test_skill_buy_cond(List, Tail);
				true -> "skill level too low "++atom_to_list(SkillName) ++ " ("++integer_to_list(SkillLv)++")"
			end
	end.

get_all_knws() ->
	lists:map(fun
				 ({_,N,_,_}) -> N
			  end, get_knw_def()).

%-----------------------------------------------------------------------------------------------
load_knowledges(CharID) ->
	Rows = db_server:knowledge_load(CharID),
	L = parse_knowledge_rows(Rows, []),
	AllKnowledges = get_all_knws(),
	megre_knowledges(L, AllKnowledges).

%-----------------------------------------------------------------------------------------------
megre_knowledges(All, []) -> All;
megre_knowledges(Loaded, [H|T]) ->
	case lists:keyfind(H, #knowledge.type, Loaded)of
		false -> megre_knowledges([#knowledge{type=H, base=get_knw_base(H)}|Loaded], T);
		_ -> megre_knowledges(Loaded, T)
	end.

%-----------------------------------------------------------------------------------------------
parse_knowledge_rows([], Acc) -> Acc;
parse_knowledge_rows([ [ID, TypeID, Level] |T], Acc) ->
	Type = get_knw_id2type(TypeID),
	K = #knowledge{id=ID, level=Level, type=Type, base=get_knw_base(Type)},
	% загружаем скиллы из бд
	LoadedSkills = load_skills(ID),
	% получаем все доступные скиллы 
	AllSkills = get_knw_skills(K),
	% надо добавить в общий список те которые доступны но еще не взяты и их нет в бд
	Skills = merge_available_skills(LoadedSkills, AllSkills),
	
	parse_knowledge_rows(T, [K#knowledge{skills = Skills} | Acc]).

%-----------------------------------------------------------------------------------------------
merge_available_skills(All, []) -> All;
merge_available_skills(LoadedSkills, [H|T]) ->
	case lists:keyfind(H, #skill.type, LoadedSkills)of
		false -> 
			RE = case lists:keyfind(exp, 1, get_skill_cond(H)) of
				false -> 0;
				{_,Q} -> Q
			end,
			S = #skill{type=H, req_exp=RE},
			merge_available_skills([S|LoadedSkills], T);
		_ -> merge_available_skills(LoadedSkills, T)
	end.

%-----------------------------------------------------------------------------------------------
load_skills(KnwID) ->
	Rows = db_server:skills_load(KnwID),
	parse_skill_rows(Rows, []).

%-----------------------------------------------------------------------------------------------
parse_skill_rows([], Acc) -> Acc;
parse_skill_rows([ [Id, TypeID, Level, Bar] | T], Acc) ->
	S = #skill{bar=Bar, id=Id, level=Level, type=get_skill_id2type(TypeID)},
	parse_skill_rows(T, [set_skill_max_bar(S)|Acc]).
	
%-----------------------------------------------------------------------------------------------
get_knw_id2type(TypeID) ->
	case lists:keyfind(TypeID, 1, get_knw_def()) of
		false -> 0;
		{_, T, _, _} -> T
	end.
get_knw_type2id(Type) ->
	case lists:keyfind(Type, 2, get_knw_def()) of
		false -> 0;
		{T, _, _, _} -> T
	end.
%-----------------------------------------------------------------------------------------------
get_skill_id2type(TypeID) ->
	case lists:keyfind(TypeID, 1, get_skill_def()) of
		false -> 0;
		{_, T} -> T
	end.
get_skill_type2id(Type) ->
	case lists:keyfind(Type, 2, get_skill_def()) of
		false -> 0;
		{T, _} -> T
	end.
%-----------------------------------------------------------------------------------------------
% id, name, base
get_knw_def() -> [
	 {1, 	agriculture,	nature, [
									 {forestry, 1}, 
									 {farming, 1} 
									]},
	 
	 {2, 	survival,		nature, [
									 {hunting, 3},
									 {fishing, 2},
									 {foraging, 1}
									 ]},
	 
	 {3, 	stoneworking,	industry, [
									 {stone_cutting, 1},
									 {stone_trim, 5}
									]},
	 
	 {4, 	architecture,	industry, [
									 {masonry, 1} 
									]},
				 
	 {5, 	pottery,		industry, [
									 {ceramics, 1},
									 {glassmaking, 6}
									]},
	 {6, 	unarmed_combat,	combat, [
									 {brawling, 1},
									 {bushido, 3}
									]}	 
	 
	 ].  
%-----------------------------------------------------------------------------------------------
get_skill_def() -> [
	 {1, 	farming},
	 {2, 	forestry},
	 {3, 	hunting},
	 {4,	fishing},
	 {5, 	foraging},
	 {6, 	stone_cutting},
	 {7,	masonry},
	 {8,	stone_trim},
	 {9,	ceramics},
	 {10,	glassmaking},
	 {11,	brawling},
	 {12,	bushido}
	 ].  


