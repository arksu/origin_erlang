%% Author: arksu
%% Created: 20.10.2011
%% Description: TODO: Add description to farming
-module(farming).


-export([is_seed/1, get_plant_filter/0, get_plant_type/1, is_plant/1, is_ripe/1,
		 get_harvest_ticks/1, harvest/2, generate_state/2,
		 get_add_param/1,
		 save_db_data/1, load_db_data/1
		 ]).

-include("defines.hrl").
-include("base_io.hrl").
-include("types.hrl").
-include("objects.hrl").
-include("net.hrl").

% является ли объект семечкой
is_seed(Type) ->
	case Type of
		seed_carrot -> true;
		carrot -> true;
		seed_wild -> true;
		seed_wheat -> true;

		_ -> false
	end.

% получить тип растения из семечки когда садим
get_plant_type(SeedType) -> 
	case SeedType of
		seed_carrot -> 		plant_carrot;
		carrot -> 			plant_carrot;
		seed_wheat ->		plant_wheat;
		
		seed_wild ->  case randoms:get(2) of
						  % все эти объекты должны спокойно проходится игроком. не давать коллизий
							1 -> plant_carrot;
							2 -> plant_wheat
					  end;

		_ -> error
	end.

% получить стейт объекта растущего растения
generate_state(_PlayerState, #item{type=Type} = SeedItem) ->
	Count = case Type of
		seed_carrot -> 4;
		carrot -> 4;
		seed_wheat -> 12;
		_ -> 3
	end,
	#obj_plant{seed=SeedItem, count=Count}.

% фильтрующая функция для растений, если в тайле есть хоть одно растение - не даст посадить
get_plant_filter() ->
	fun
	   (plant_carrot) -> true;
	   (plant_wheat) -> true;

	   % UPDATE types.c => is_plant !!!!!
	   (_) -> false
	end.

% созрело ли растение (можно ли собрать)
is_ripe(#object{state=State, type=Type}) ->
	#obj_plant{stage=Stage} = State,
	case Type of
		plant_carrot ->
			if Stage >= 3 -> true; true -> false end;
		plant_wheat ->
			if Stage >= 3 -> true; true -> false end;
		_ -> false
	end.

% количество времени нужное для сбора урожая
get_harvest_ticks(#object{type=Type}) ->
	case Type of
		plant_carrot -> 4500;
		plant_wheat -> 2000;
		_ -> 2000
	end.

% собрать урожай. вернет {Items, Objects} - Items - что положим в инвентарь,
% Objects - что заспавним на месте растения (q, HP!, id - берем вручную)
harvest(#object{type=Type, state=#obj_plant{seed=SeedItem}}, PlayerState) ->
	if (is_record(SeedItem, item)) ->
   	#item{q=Q} = SeedItem,
	case Type of
		plant_carrot -> {spawn_plant_items(carrot, Q, PlayerState, 2), []};
		plant_wheat -> {spawn_plant_items(seed_wheat, Q, PlayerState, 2), [inventory:spawn_item(straw, 10, 1)]};
		_ ->
			{[],[]}
	end;
	true -> {[],[]}
	end.

% получить разброс по качеству
get_quality_range(_PlayerState) ->
	randoms:get(7) - 4.

% -------------------------------------------------------------------------------------------------

% получить параметр количества растений в тайле
get_add_param(#object{state=#obj_plant{count=Count}}) ->
	[{count, Count}].

% заспавнить указанное количество объектов в инвентарь. с разбросом по качеству
spawn_plant_items(Type, Q, PlayerState, Count) ->
	spawn_plant_items(Type, Q, PlayerState, Count, []).
spawn_plant_items(_, _, _, 0, Acc) -> Acc;
spawn_plant_items(Type, Q, #player_state{skills=Skills} = PlayerState, Count, Acc) ->
	case skills:get_skill(Skills, farming) of
		false -> error(havnt_farming_skill);
		FarmingLvl -> 
			Q1 = Q+get_quality_range(PlayerState),
			QNew = if 
					   Q1 < 1 -> 1;
					   Q1 > FarmingLvl -> FarmingLvl;
					   true -> Q1
				   end,
			spawn_plant_items(Type, Q, PlayerState, Count-1, [inventory:spawn_item(Type, QNew, 1)|Acc])
	end.

is_plant(Type) ->
	Fun = get_plant_filter(),
	Fun(Type).

save_db_data(#obj_plant{seed=Seed, stage=Stage}) ->
    list_to_binary([write_int(Stage),inventory:save_db_data(Seed)]).

load_db_data(Data) ->
	{Stage, Bin} = read_int(Data),
	{Item,_} = inventory:load_db_data(Bin),
	?DEBUG("plant loaded ~p stage=~p",[Item,Stage]),
    #obj_plant{seed=Item, stage=Stage}.