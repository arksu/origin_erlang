%% Author: arksu
%% Created: 26.01.2012
%% Description: TODO: Add description to player
-module(player).

-include("types.hrl").

-export([
		 get_strength/1,
		 get_perception/1,
		 get_stealth/1,
		 get_agi/1,
		 get_cons/1,
		 eat_fep/2,
		 get_max_fep/1,
		 get_actions/2,
		 get_action_reuse/1,
		 get_current_speed/1,
		 get_hp_regen_speed/1,
		 get_stamina_regen_speed/1,
		 get_action_min_stamina/1,
		 get_action_stamina/1,
		 get_action_time/1,
		 get_equip_param/1,
		 is_dialog_open/2,
		 get_exp_val/2,
		 add_exp/3,
		 is_command_allow/2
		]).

-include("defines.hrl").

% ##################################################################################
% # DEFINES #
% ##################################################################################
	


%% --------------------------------------------------------------------
% получить силу чара с учетом всех скиллов и бафов. конечное значение
get_strength(#player_state{stats=Stats}) ->
	#player_stats{str=#stat{level=Lv}} = Stats,
	Lv*10.

%% --------------------------------------------------------------------
% получить зрение чара с учетом всех скиллов и бафов. конечное значение
get_perception(#player_state{stats=Stats}) ->
	#player_stats{perc=#stat{level=Lv}} = Stats, Lv.

get_cons(#player_state{stats=Stats}) ->
	#player_stats{cons=#stat{level=Lv}} = Stats, Lv.

get_agi(#player_state{stats=Stats}) ->
	#player_stats{agi=#stat{level=Lv}} = Stats, Lv.

%% --------------------------------------------------------------------
get_max_fep(#player_state{stats=#player_stats{
				agi=#stat{level=Agi},
				cons=#stat{level=Cons},
				perc=#stat{level=Perc},
				dex=#stat{level=Dex},
				str=#stat{level=Str},
				int=#stat{level=Int},
				wis=#stat{level=Wis},
				cha=#stat{level=Cha}
				}}) ->
	#player_stats{
		agi	 = Agi*10,
		cons = Cons*10,
		dex	 = Dex*10,
		int	 = Int*10,
		perc = Perc*10,
		str	 = Str*10,
		cha  = Cha*10,
		wis  = Wis*10
	}.

%% --------------------------------------------------------------------
% получить скрытность
get_stealth(_PlayerState) -> 0.

% ##################################################################################
% # SYSTEM #
% ##################################################################################

get_equip_param(#player_state{equip=#equip{
		body=Body,
		foots=Foots,
		head=Head,
		legs=Legs,
		lhand=LHand,
		rhand=RHand 
										    }}) ->
	L = [
		 get_equip_item(Head), 
		 get_equip_item(Body),
		 get_equip_item(LHand),
		 get_equip_item(RHand),
		 get_equip_item(Legs),
		 get_equip_item(Foots)
		  ],
	{equip, L}.

get_equip_item(none) -> none;
get_equip_item(#item{type=T}) -> T.

%% --------------------------------------------------------------------
eat_fep(FoodFep, #player_state{stats=#player_stats{
				agi=Agi,
				cons=Cons,
				perc=Perc,
				dex=Dex,
				str=Str,
				int=Int,
				wis=Wis,
				cha=Cha
				}} = PlayerState) ->
	#player_stats{
		agi	 = AgiFood,
		cons = ConsFood,
		dex	 = DexFood,
		int	 = IntFood,
		perc = PercFood,
		str	 = StrFood,
		cha  = ChaFood,
		wis  = WisFood
	} = FoodFep,
	
	#player_stats{
		agi	 = AgiMax,
		cons = ConsMax,
		dex	 = DexMax,
		int	 = IntMax,
		perc = PercMax,
		str	 = StrMax,
		cha  = ChaMax,
		wis  = WisMax
	} = get_max_fep(PlayerState),
	
	PlayerState#player_state{stats = #player_stats{
		agi=add_fep(AgiFood, AgiMax, Agi),
		cons=add_fep(ConsFood, ConsMax, Cons),
		dex=add_fep(DexFood, DexMax, Dex),
		int=add_fep(IntFood, IntMax, Int),
		perc=add_fep(PercFood, PercMax, Perc),
		str=add_fep(StrFood, StrMax, Str),
		cha=add_fep(ChaFood, ChaMax, Cha),
		wis=add_fep(WisFood, WisMax, Wis)
	}}.

%% --------------------------------------------------------------------
add_fep(FoodFep, MaxFep, #stat{level=Level, fep=Fep}) when Fep + FoodFep >= MaxFep -> #stat{level=Level+1, fep=0};
add_fep(FoodFep, _MaxFep, #stat{level=Level, fep=Fep}) -> #stat{level=Level, fep=Fep + FoodFep}.

%% --------------------------------------------------------------------
% сколько минимум стамины требуется для выполнения действия. если стамина упадет ниже - действие прервется
get_action_min_stamina(Action) ->
	case Action of
		dig_hole -> 5;
		plow -> 5;
		_ -> 3
	end.

%% --------------------------------------------------------------------
% сколько стамины тратится за 1 тик действия
get_action_stamina(Action) ->
	case Action of
		dig_hole -> 1500;
		plow -> 1300;
		_ -> 500
	end.

get_action_time(Action) -> 
	case Action of
		dig -> 2000;
		dig_hole -> 15000;
		_ -> 3000
	end.

%% --------------------------------------------------------------------
% получить список доступных действий
get_actions(Skills, AccessLevel) ->
    A = [
	 {"root","lift"},
     {"root","destroy"},
	 {"root","repair"},
	 
	 {"root","plow"},
	 {"root","dig"},
	 {"root","dig_hole"},
     {"root","root_combat"},
	 {"root","root_craft"},
	 {"root","root_build"},

     {"root_combat","attack_buff"},
     {"root_combat","attack_debuff"},
	 {"root_combat","attack_punch"},

%% 	 {"root_craft","craft_brick_clay_raw"},
	 {"root_craft","craft_stone_axe"},
	 {"root_craft","craft_wood_shoes"},
%% 	 {"root_craft","craft_pants_skin"},

	 {"root_build","lay_stone"},
	 {"root_build","lay_grass"},
	 {"root_build","build_corner_home_wood"},
	 {"root_build","build_corner_fence"},
	 {"root_build","build_chair"},
	 {"root_build","build_claim"},
	 {"root_build","build_bonfire"},
	 {"root_build","build_jar"}

    ],
	AA = if (AccessLevel > 100) -> A ++ [
     {"root","root_admin"},
     {"root_admin","spawn_inv_drawing_box"},
     {"root_admin","spawn_inv_fabric_bag"},
	 {"root_admin","spawn_inv_seed_carrot"},

	 {"root_admin","tile_water_deep"}, 
	 {"root_admin","tile_water_low"},
	 {"root_admin","tile_sand"},
	 {"root_admin","tile_forest_fir"},
	 {"root_admin","tile_forest_leaf"},
	 {"root_admin","tile_grass"},
	 {"root_admin","tile_sett"},
	 {"root_admin","tile_plowed"},
	 {"root_admin","tile_swamp"},
	 {"root_admin","tile_dirt"},
	 {"root_admin","tile_house"},
	 {"root_admin","tile_cellar"},
	 {"root_admin","tile_cave"},
	 {"root_admin","tile_hole"},

	 {"root_admin","remove"},
	 {"root_admin","spawn_box"},

 	 {"root_build","spawn_corner_home_wood"},
 	 {"root_build","spawn_wally_home_wood"},
 	 {"root_build","spawn_wallwindowy_home_wood"},
 	 {"root_build","spawn_walldoory_home_wood"},

	 {"root_admin","spawn_plant_wild"},

	 {"root_combat","attack_godbuff"},

	 {"root_admin","spawn_fir"},
	 {"root_admin","spawn_pine"},
	 {"root_admin","spawn_oak"},
	 {"root_admin","spawn_pear"},
	 {"root_admin","spawn_apple"},
	 {"root_admin","spawn_stone"},
	 {"root_admin","spawn_runestone"},
	 {"root_admin","spawn_rabbit"}
	]; true -> A end,
	
	lists:append([AA, add_actions(skills:get_actions_def(), Skills, []) ]).

add_actions([], _, Acc) -> Acc;
add_actions([{Skill, Actions}|T], SkillsList, Acc) ->
	case skills:get_skill(SkillsList, Skill) of
		false -> add_actions(T, SkillsList, Acc);
		_ -> add_actions(T, SkillsList, lists:append(Actions, Acc))
	end.
			

%---------------------------------------------------------------------------
% сколько мс надо на реген 1 единицы хп
get_hp_regen_speed(_State) ->
	5000.
% сколько мс надо на реген 1 единицы стамины
get_stamina_regen_speed(State) ->
	case buff:get_buff(State, god_buff) of
		none -> 3000;
		_ -> 100
	end.

%---------------------------------------------------------------------------
% получить текущую скорость бега в единицах в секунду
get_current_speed(#player_state{speed = Speed, stamina=Stamina}) ->
	if
		Stamina < 5 -> 0;
		(Stamina < 50) andalso (Speed > 2) -> 35;
		(Stamina < 60) andalso (Speed > 3) -> 55;
	   	true ->
			case Speed of
				1 -> 20;
				2 -> 35;
				3 -> 55;
				4 -> 80;
				_ -> 0
			end
	end.


%% --------------------------------------------------------------------
get_action_reuse(Name) ->

	case Name of
		{player, war_buff, _} -> 2000;
		{player, war_debuff, _} -> 2000;
		{player, punch, _} -> 2000;

%% 		seed -> 1000;
%% 		lift -> 1000;
%% 		plow -> 1000;
		dig_hole -> 3000;
%% 		craft -> 5000;
		_ -> 0
	end.

%-----------------------------------------------------------------------------
is_dialog_open(#player_state{dialogs=L}, Name) -> lists:member(Name, L).
%-----------------------------------------------------------------------------
get_exp_val(#exp{combat=C,industry=I,nature=N}, Base) ->
	case Base of
		combat -> C;
		industry -> I;
		nature -> N
	end.
add_exp(#exp{combat=C,industry=I,nature=N}=E, Base, Value) ->
	case Base of
		combat -> E#exp{combat=C+Value};
		industry -> E#exp{industry=I+Value};
		nature -> E#exp{nature=N+Value}
	end.


%-----------------------------------------------------------------------------
is_command_allow(Cmd, AccessLevel) ->
	case Cmd of
		who -> true;
		_ ->
			if 
				(AccessLevel >= 100) -> true;
				true -> false
			end
	end.