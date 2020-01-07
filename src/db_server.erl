%% Author: arksu
%% Created: 28.01.2011
%% Description: 
-module(db_server).
-include("mysql.hrl").
-include("map.hrl").

%% API
-export([]).
-compile(export_all).

-include("defines.hrl").
-include("base_io.hrl").
-include("types.hrl").

%% log(_Module, _Line, _Level, _FormatFun) ->
%%     ok.


start_link() ->
    ?DEBUG("~p starting...~n" ,[?MODULE]),

    Cfg = config:get_config(),
    DB_host = config:get_value(Cfg, db_host),
    DB_user = config:get_value(Cfg, db_user),
    DB_pass = config:get_value(Cfg, db_pass),
    DB_name = config:get_value(Cfg, db_name),
 
    Pid = mysql:start_link(mysql_connection, DB_host, undefined, DB_user, DB_pass, DB_name, fun(_,_,_,_) -> ok end),
%%     Pid = mysql:start_link(mysql_connection, DB_host, undefined, DB_user, DB_pass, DB_name),


    mysql:connect(mysql_connection, DB_host, undefined, DB_user, DB_pass, DB_name, true),
    mysql:connect(mysql_connection, DB_host, undefined, DB_user, DB_pass, DB_name, true),
    mysql:connect(mysql_connection, DB_host, undefined, DB_user, DB_pass, DB_name, true),
    mysql:connect(mysql_connection, DB_host, undefined, DB_user, DB_pass, DB_name, true),
    ?DEBUG("~p started~n" ,[?MODULE]),
    P = spawn_link(?MODULE, req_loop, []),
    register(db_request, P),
    Pid.

%%----------------------------------------------------------------------------
req_loop() ->
    receive
        {request, Request} ->
            %?DEBUG("DB_SERVER_WORKER: execute request ~p",[Request]),
            mysql:fetch(mysql_connection, <<Request>>),
            req_loop();

		{request_params, Tag, Query, Params} ->
			%?DEBUG("DB_SERVER_WORKER: execute request ~p",[Query]),
	        mysql:prepare(Tag, Query),
        	mysql:execute(mysql_connection, Tag, Params),
			req_loop();

        {is_live, Pid} ->
            Pid ! {db_request_live},
            req_loop()
    end.

execute(Request) ->
    %?DEBUG("DB_SERVER: execute ~p",[Request]),
    db_request ! {request, Request}.
execute(Tag, Query, Params) ->
    %?DEBUG("DB_SERVER: execute ~p ~p",[Tag, Query]),
	case a1_utils:list_exist([db_request], registered()) of 
		false -> 
			?CRITICAL_MSG("db request not exist!"), 
			error(db_request_not_exist);
		true ->
    		db_request ! {request_params, Tag, Query, Params}
	end.


%##################################################################################################################
%##################################################################################################################
%-------------------------------------------------------------------------------------------------

%% заполнить гриды в базе одного супергрида, 27500 - количество гридов. в 1 сг - 2500 гридов, нужно 11 уровней. 1 земля и 10 вниз.
map_fill_sg() ->
    %{A1,A2,A3} = now(),
    %random:seed(A1, A2, A3),
    map_fill_sg_acc(0, "sg_0").
%------------------------
fill_grid(A) ->
    fill_grid_acc([], A, 0).

fill_grid_acc(Acc, _, 10000) ->
    Acc;
fill_grid_acc(Acc, A, Num) ->
    fill_grid_acc([A|Acc], A, Num+1).
%------------------------
fill_levels() ->
    fill_levels_acc([], 0).

fill_levels_acc(Acc, 10000) ->
    Acc;
fill_levels_acc(Acc, Num) ->
    fill_levels_acc([(random:uniform(3)-1)|Acc], Num+1).
%------------------------
map_fill_sg_acc(27500,_) -> ok;
map_fill_sg_acc(Num, SgName) ->
    DataTile = fill_grid(Num rem 2),
    DataLevel = fill_grid(0),
    mysql:prepare(dbreq_map_fill_sg_acc,
                  list_to_binary("INSERT INTO `" ++ SgName ++ "` (`id`, `data`) VALUES (?, ?);")),
    mysql:execute(mysql_connection, dbreq_map_fill_sg_acc, [ (Num), lists:append(DataTile, DataLevel)]),

    if ((Num rem 1000) == 0) ->
           io:fwrite("sg=~p, ~p~n",[SgName, Num]); true -> ok end,

    map_fill_sg_acc(Num+1, SgName).

% нагенерить объекты для супегрида
gen_terrain() ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
    gen_terrain(15000).

gen_terrain(0) ->
    ok;
gen_terrain(Count) ->
    X = random:uniform(?SUPERGRID_FULL_SIZE)-1,
    Y = random:uniform(?SUPERGRID_FULL_SIZE)-1,
	{TX,TY} = map_server:tilify(X, Y),
    ID = Count + 626000,
    {{_, _}, {_, _}, {_, _}, _, Grid, _} = map_server:get_grid_coord(X, Y, 0),

    mysql:prepare(dbreq_gen_terrain,
                  <<"INSERT INTO `sg_obj_0` (`id`, `grid`, `x`, `y`, `type`) VALUES (?,?, ?, ?, ?);">>),
    mysql:execute(mysql_connection, dbreq_gen_terrain, [
                                  ID, Grid, TX + (?TILE_SIZE div 2), TY+(?TILE_SIZE div 2), 59
                                                        ]),
    if ((Count rem 1000) == 0) -> io:fwrite("~p~n",[Count]); true -> ok end,
    gen_terrain(Count-1).

gen_object(Sg, X,Y,ID,Type) ->
    {{_, _}, {_, _}, {_, _}, _, Grid, _} = map_server:get_grid_coord(X, Y, 0),

    mysql:prepare(dbreq_gen_terrain,list_to_binary(
                  "INSERT INTO `sg_obj_"++integer_to_list(Sg)++"` (`id`, `grid`, `x`, `y`, `type`) VALUES (?,?, ?, ?, ?);")),
    mysql:execute(mysql_connection, dbreq_gen_terrain, [
                     ID, Grid, X + (?TILE_SIZE div 2), Y+(?TILE_SIZE div 2), Type
                                                        ]).

%##################################################################################################################
%##################################################################################################################
%##################################################################################################################


%-------------------------------------------------------------------------------------------------

login_get_last_time([]) -> {err};
login_get_last_time(Login) ->
    mysql:prepare(dbreq_login_get_last_time,
          <<"SELECT UNIX_TIMESTAMP(time) as unixt FROM account WHERE login=?">>),

    try mysql:execute(mysql_connection, dbreq_login_get_last_time, [Login]) of
    {data, #mysql_result{ rows = [] }} ->
                [];
    {data, #mysql_result{ rows = [[Time|_]|_] }} ->
                Time
    catch
        _:_ ->
                []
    end.

%-------------------------------------------------------------------------------------------------
% get pass from db if not logged in
login_get_pass([]) -> {err};
login_get_pass(A) ->
	?DEBUG("DB_SERVER: login_get_pass"),
    mysql:prepare(dbreq_login_get_pass,
          <<"SELECT `pass`, `id`, `last_char` FROM `account` WHERE `status`=0 AND `login`=?">>),
    try mysql:execute(mysql_connection, dbreq_login_get_pass, [A]) of
    {data, #mysql_result{ rows = [] }} ->
                [];
    {data, #mysql_result{ rows = [[Pass,AccID,LastChar|_]|_] }} ->
                       {binary_to_list(Pass), AccID, LastChar}
    catch
        _:_ ->
                []
    end.

%-------------------------------------------------------------------------------------------------
% update logged time
login_update_time(AccountID) ->
    ?DEBUG("login_update_time"),
    Time = timer:now_diff(now(), {0,0,0}) div 1000000,
            mysql:prepare(dbreq_login_check_time2,
            <<"UPDATE `account` SET `time`=from_unixtime(?) WHERE `id`=? LIMIT 1">>),
            %% UNIX_TIMESTAMP(date)
            mysql:execute(mysql_connection, dbreq_login_check_time2, [Time,AccountID]).

%-------------------------------------------------------------------------------------------------
% if wrong cookie must return [], or if right - return binary Cookie
login_get_cookie(Login, Pwd, CharID) ->
    {_,S,_} = now(),
    B = list_to_binary([<<S:32/integer-signed>>|Login]),
	?DEBUG("login_get_cookie bin=~p",[B]),
    Cookie = erlang:md5(B),
    Time = timer:now_diff(now(), {0,0,0}) div 1000000,
    mysql:prepare(dbreq_login_get_cookie,
    <<"UPDATE account SET cookie=?, time=from_unixtime(?), last_char=? WHERE login=? AND pass=? LIMIT 1">>),
    %% UNIX_TIMESTAMP(date)
    mysql:execute(mysql_connection, dbreq_login_get_cookie, [Cookie,Time,CharID,Login,Pwd]),
    Cookie.

%-------------------------------------------------------------------------------------------------

login_get_chars(AccID) ->
    mysql:prepare(dbreq_login_get_chars,
        list_to_binary("SELECT id, name FROM `char` WHERE account_id=?;")),
    try mysql:execute(mysql_connection, dbreq_login_get_chars, [AccID]) of
    {data, #mysql_result{ rows = [] }} ->
                [];
    {data, #mysql_result{ rows = Data }} ->
                Data
    catch
        _:_ ->
                []
    end.

%-------------------------------------------------------------------------------------------------

% get binary grid data, Sg - supergrid num, Grid - index of minimap (grid); return binary | []
map_get_grid(_Sg, Grid) when Grid >= (?INSTANCE_BEGIN_ID * ?SUPERGRID_SIZE * ?SUPERGRID_SIZE) ->
        mysql:prepare(dbreq_map_get_grid,
            <<"SELECT `data`, `time` FROM `instance` WHERE grid=? LIMIT 1;">>),
        try mysql:execute(mysql_connection, dbreq_map_get_grid, [Grid]) of
        {data, #mysql_result{ rows = [] }} ->
                    [];
        {data, #mysql_result{ rows = [[Data,Time|_]|_] }} ->
                    {Data, Time}
        catch
            _:_ ->
                    []
        end;
map_get_grid(Sg, Grid) ->
        Name = "sg_" ++ integer_to_list(Sg),
        mysql:prepare(dbreq_map_get_grid,
            list_to_binary("SELECT `data`, `time` FROM `"++Name++"` WHERE `id`=? LIMIT 1;")),
        try mysql:execute(mysql_connection, dbreq_map_get_grid, [Grid]) of
        {data, #mysql_result{ rows = [] }} ->
                    [];
        {data, #mysql_result{ rows = [[Data,Time|_]|_] }} ->
                    {Data, Time}
        catch
            _:_ ->
                    []
        end.

map_set_grid(Sg, Grid, Data, Time) ->
        Name = "sg_" ++ integer_to_list(Sg),
		execute(dbreq_map_set_grid,
				list_to_binary("UPDATE `"++Name++"` SET `data`=?, `time`=? WHERE `id`=?;"),
				[Data,Time,Grid]).
map_set_grid(Sg, Grid, Data) ->
        Name = "sg_" ++ integer_to_list(Sg),
		mysql:prepare(dbreq_map_set_grid,
				list_to_binary("UPDATE `"++Name++"` SET `data`=? WHERE `id`=?;")),
		mysql:execute(mysql_connection, dbreq_map_set_grid, [Data,Grid]).

map_set_grid_time(Sg, Grid, Time) ->
        Name = "sg_" ++ integer_to_list(Sg),
		execute(dbreq_map_set_grid_time,
				list_to_binary("UPDATE `"++Name++"` SET `time`=? WHERE `id`=?;"),
				[Time,Grid]).


%-------------------------------------------------------------------------------------------------
% check cookie and return Charid | err
game_check_cookie(Cookie) ->
    Time = timer:now_diff(now(), {0,0,0}) div 1000000,
    % find acc by cookie, get lastchar and check that is account char
    mysql:prepare(dbreq_game_check_cookie,
        <<"SELECT `account`.`last_char`, UNIX_TIMESTAMP(`account`.`time`) FROM `account`, `char` WHERE `account`.`cookie` = ? and `account`.`last_char` = `char`.`id` and `account`.`id` = `char`.`account_id`">>),
    try mysql:execute(mysql_connection, dbreq_game_check_cookie, [Cookie]) of
    {data, #mysql_result{ rows = []}} ->
                err;
    {data, #mysql_result{ rows = [[Charid, TimeDB|_]|_] }} ->
        % give 20 secs for login to game server
        if Time - TimeDB < 20 ->
                Charid;
           true ->
                err
        end
    catch _:_ -> err
    end.
%    db_server:start()
%    C = db_server:login_get_cookie("ark","123", 2)
%    S = <<107,43,152,254,161,30,81,175,48,67,177,146,247,25,187,105>>
%    B = db_server:game_check_cookie(S)
%    A C S

%%-------------------------------------------------------------------------------------------------
% load char params return {X,Y,Name,AccountID,Hp,SHp,HHp} | err
player_load(CharID) ->
    mysql:prepare(dbreq_player_load,
        list_to_binary("SELECT `account_id`,`access_level`,`name`,`x`,`y`,`l`,`hp`,`shp`,`hhp`,`stamina`,"++
	"`exp_nature`,`exp_industry`,`exp_combat`,`reuse_time`, `reuse_len`,"++
	"`str_level`, `str_fep`, `cons_level`, `cons_fep`, `perc_level`, `perc_fep`,"++
	"`agi_level`, `agi_fep`, `dex_level`, `dex_fep`, `int_level`, `int_fep`,"++
	"`wis_level`, `wis_fep`, `cha_level`, `cha_fep`"++
	" FROM `char` WHERE `id` = ?")),
    try mysql:execute(mysql_connection, dbreq_player_load, [CharID]) of
    {data, #mysql_result{ rows = []}} ->
                err;
    {data, #mysql_result{ rows = [[AccountID,AccessLevel,Name,X,Y,Lv,Hp,SHp,HHp,Stamina,ExpNature,ExpIndustry,ExpCombat,
								   ReuseTime,ReuseLen,
								   StrLevel, StrFep, ConsLevel, ConsFep, PercLevel, PercFep,
								   AgiLevel, AgiFep, DexLevel, DexFep, IntLevel, IntFep,
								   WisLevel, WisFep, ChaLevel, ChaFep
								  |_]|_] }} ->
                {AccountID,AccessLevel,Name,X,Y,Lv,Hp,SHp,HHp,Stamina,
				 ExpNature,ExpIndustry,ExpCombat,
				 ReuseTime,ReuseLen,
				 StrLevel, StrFep, ConsLevel, ConsFep, PercLevel, PercFep,
				 AgiLevel, AgiFep, DexLevel, DexFep, IntLevel, IntFep,
				 WisLevel, WisFep, ChaLevel, ChaFep
				};
    _ -> err
    catch _:_ -> err
    end.

%%-------------------------------------------------------------------------------------------------
% save player coords
player_update_coord(CharID, X, Y, Lv) ->
	?DEBUG("player_update_coord charid=~p <~p ~p ~p>",[CharID, X, Y, Lv]),
    mysql:prepare(dbreq_player_update_coord,
                  <<"UPDATE `char` SET `x`=?, `y`=?, `l`=? WHERE `char`.`id`=?">>),
    mysql:execute(mysql_connection, dbreq_player_update_coord, [X, Y, Lv, CharID]).

%%-------------------------------------------------------------------------------------------------
% save player reuse
player_update_reuse(CharID, Time, Len) ->
        mysql:prepare(dbreq_player_update_reuse,
                  <<"UPDATE `char` SET `reuse_time`=?, `reuse_len`=? WHERE `char`.`id`=?">>),
        mysql:execute(mysql_connection, dbreq_player_update_coord, [Time, Len, CharID]).

%%-------------------------------------------------------------------------------------------------
% save player exp
player_update_exp(CharID, #exp{combat=C, industry=I, nature=N}) -> player_update_exp(CharID, N, I, C).
player_update_exp(CharID, ExpNature, ExpIndustry, ExpCombat) ->
        mysql:prepare(dbreq_player_update_exp,
                  <<"UPDATE `char` SET `exp_nature`=?, `exp_industry`=?, `exp_combat`=? WHERE `char`.`id`=?">>),
        mysql:execute(mysql_connection, dbreq_player_update_exp, [ExpNature,ExpIndustry,ExpCombat, CharID]).

%%-------------------------------------------------------------------------------------------------
% save player hp
player_update_hp(CharID, Val) ->
        mysql:prepare(dbreq_player_update_hp,
                  <<"UPDATE `char` SET `hp`=? WHERE `char`.`id`=?">>),
        mysql:execute(mysql_connection, dbreq_player_update_hp, [Val, CharID]).
%%-------------------------------------------------------------------------------------------------
% save player stamina
player_update_stamina(CharID, Val) ->
        mysql:prepare(dbreq_player_update_stamina,
                  <<"UPDATE `char` SET `stamina`=? WHERE `char`.`id`=?">>),
        mysql:execute(mysql_connection, dbreq_player_update_stamina, [Val, CharID]).

%%-------------------------------------------------------------------------------------------------
% add object to db
object_add(Object) ->
    #object{id=ObjID, type=Type, coord={X,Y,Lv}, time=Time, q=Q, hp={HP,SHP} } = Object,
    case map_server:get_grid_coord(X,Y,Lv) of
		{coord_error} ->
			?WARNING_MSG("object_add: error coord ~p ~p ~p",[X,Y,Lv]);
		{{_,_},{_,_},{_,_}, Sg, Grid, _} ->
    		Name = integer_to_list(Sg),
    		TypeID = objects_server:get_typeid(Type),
    		mysql:prepare(dbreq_object_add, list_to_binary(
    			"INSERT INTO `sg_obj_"++Name++"` (`id`, `grid`, `x`, `y`, `type`, `data`,`time`,`q`,`hp`,`shp`) VALUES (?,?, ?, ?, ?,?,?, ?,?,?);")),
    		mysql:execute(mysql_connection, dbreq_object_add, [
                                  ObjID, Grid, X, Y, TypeID,objects_server:save_object_data(Object), Time, Q, HP, SHP
                                                        ])
	end.

%%-------------------------------------------------------------------------------------------------
% update free id
object_update_free_id(ID) ->
    mysql:prepare(dbreq_object_update_free_id,
        <<"UPDATE `server` SET `val`=? WHERE `val_name` = ?">>),
    mysql:execute(mysql_connection, dbreq_object_update_free_id, [ID,"freeid"]).

%%-------------------------------------------------------------------------------------------------
% get free next object id
object_get_free_id() ->
    mysql:prepare(dbreq_object_get_free_id,
        <<"SELECT `val` FROM `server` WHERE `server`.`val_name` = ? ;">>),
    try mysql:execute(mysql_connection, dbreq_object_get_free_id, ["freeid"]) of
    {data, #mysql_result{ rows = []}} ->
                err;
    {data, #mysql_result{ rows = [[Data|_]|_] }} ->
                Data;
    A -> A
    catch _:_ -> err
    end.

%%-------------------------------------------------------------------------------------------------
% delete object from db
object_remove(Sg, ObjID) ->
    Name = integer_to_list(Sg),

	% удаляем объект из таблицы супергрида
    mysql:prepare(dbreq_object_remove, list_to_binary(
        "DELETE FROM `sg_obj_"++Name++"` WHERE `id` = ? LIMIT 1;")),
    mysql:execute(mysql_connection, dbreq_object_remove, [ObjID]).
	% а также надо грохнуть все вещи и инвентари связанные с ним

%%-------------------------------------------------------------------------------------------------
% update object pos in db if need change sg table
object_update_pos({OldX,OldY,OldLv}, Object) ->
    ?DEBUG("object_update_pos, ~p ~p ~p ~p", [OldX,OldY,OldLv, Object]),
    #object{id=ObjID, type=Type, coord={ToX,ToY,ToLv}, time=Time, q=Q, hp={HP,SHP}} = Object,
    % only if coord is changed
%%     if (ToX == OldX) and (ToY == OldY) and (ToLv == OldLv) -> ok;
%%     true ->
    OldSg = map_server:get_sg(OldX,OldY,OldLv),
    case map_server:get_grid_coord(ToX, ToY, ToLv) of
		{coord_error} ->
			?WARNING_MSG("object_update_pos: error coord ~p ~p ~p",[ToX,ToY,ToLv]);
		{{_,_},{_,_},{_,_}, Sg, Grid, _} ->
    		?DEBUG("old sg=~p sg = ~p",[OldSg,Sg]),
    		if (OldSg =/= Sg) ->
		           object_remove(OldSg, ObjID),
        		   object_add(Object);
		    true ->
        		Name = integer_to_list(Sg),
		        TypeID = objects_server:get_typeid(Type),
        		?DEBUG("type= ~p",[TypeID]),
		        mysql:prepare(dbreq_object_update_pos, list_to_binary(
        		"UPDATE `sg_obj_"++Name++"` SET `x`=?, `y`=?, `grid`=?, `type`=?, `data`=?, `time`=?, `q`=?, `hp`=?, `shp`=? WHERE `id` = ?")),
        		mysql:execute(mysql_connection, dbreq_object_update_pos, [ToX,ToY,Grid,TypeID,
																  objects_server:save_object_data(Object),
																  Time, Q, HP, SHP,
																  ObjID])
%%     end
    		end
	end.

%%-------------------------------------------------------------------------------------------------
% load static objects from bd by grid, return list of objects
% грузим чисто статические объекты. динамика уже загружена.
objects_load_grid(Sg, Grid) ->
    Name = "sg_obj_" ++ integer_to_list(Sg),

    mysql:prepare(dbreq_objects_load_grid, list_to_binary(
        "SELECT `id`,`grid`,`x`,`y`,`type`,`hp`,`shp`,`data`,`time`,`q`,`hp`,`shp` FROM `"++Name++"` WHERE `grid` = ? AND `type` < ?")),
    try mysql:execute(mysql_connection, dbreq_objects_load_grid, [Grid, ?MIN_TICK_ID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.

%%-------------------------------------------------------------------------------------------------
% load static objects from bd by grid, return list of objects
% грузим ВСЕ объекты по гриду
objects_all_load_grid(Sg, Grid) ->
    Name = "sg_obj_" ++ integer_to_list(Sg),

    mysql:prepare(dbreq_objects_load_grid, list_to_binary(
        "SELECT `id`,`grid`,`x`,`y`,`type`,`hp`,`shp`,`data`,`time`,`q`,`hp`,`shp` FROM `"++Name++"` WHERE `grid` = ?")),
    try mysql:execute(mysql_connection, dbreq_objects_load_grid, [Grid]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.

%%-------------------------------------------------------------------------------------------------
% загрузить тик объекты для всего супергрида
objects_load_tick_supergrid(Sg) ->
    Name = "sg_obj_" ++ integer_to_list(Sg),

    mysql:prepare(dbreq_objects_load_tick_supergrid, list_to_binary(
        "SELECT `id`,`grid`,`x`,`y`,`type`,`hp`,`shp`,`data`,`time`,`q`,`hp`,`shp` FROM `"++Name++"` WHERE `type` >= ? AND `type` < ?")),
    try mysql:execute(mysql_connection, dbreq_objects_load_tick_supergrid, [?MIN_TICK_ID,?MAX_TICK_ID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.



%%-------------------------------------------------------------------------------------------------
% load inventory by inv_id
inventory_load(InvID) ->
    mysql:prepare(dbreq_inventory_load, list_to_binary(
        "SELECT `id`,`type`,`q`,`amount`,`ticks_left`,`ticks`,`x`,`y`,`num` FROM `inventory` WHERE `x`<200 AND `inv_id` = ?")),
    try mysql:execute(mysql_connection, dbreq_inventory_load, [InvID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.
%%-------------------------------------------------------------------------------------------------
% load inventory hand by inv_id, return only 1 item in array
inventory_load_hand(InvID) ->
    mysql:prepare(dbreq_inventory_load_hand, list_to_binary(
        "SELECT `id`,`type`,`q`,`amount`,`ticks_left`,`ticks`,`x`,`y`,`num` FROM `inventory` WHERE `x`=255 AND `inv_id` = ? LIMIT 1")),
    try mysql:execute(mysql_connection, dbreq_inventory_load_hand, [InvID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.
%%-------------------------------------------------------------------------------------------------
% загрузить эквип чара inv_id
inventory_load_equip(InvID) ->
    mysql:prepare(dbreq_inventory_load, list_to_binary(
        "SELECT `id`,`type`,`q`,`amount`,`ticks_left`,`ticks`,`x`,`y`,`num` FROM `inventory` WHERE `x`>200 AND `x`<220 AND `inv_id` = ?")),
    try mysql:execute(mysql_connection, dbreq_inventory_load, [InvID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.
%%-------------------------------------------------------------------------------------------------
% inventory remove item
inventory_remove(InventoryID, X) ->
    mysql:prepare(dbreq_inventory_remove, list_to_binary(
        "DELETE FROM `inventory` WHERE `inv_id` = ? AND `x` = ? ;")),
    mysql:execute(mysql_connection, dbreq_inventory_remove, [InventoryID, X]).
%%-------------------------------------------------------------------------------------------------
% inventory remove item
inventory_remove(ObjID) ->
    mysql:prepare(dbreq_inventory_remove, list_to_binary(
        "DELETE FROM `inventory` WHERE `id` = ? ;")),
    mysql:execute(mysql_connection, dbreq_inventory_remove, [ObjID]).
%%-------------------------------------------------------------------------------------------------
% удалить весь инвентарь владельца
inventory_remove_owner(InventoryID) ->
    mysql:prepare(dbreq_inventory_remove_owner, list_to_binary(
        "DELETE FROM `inventory` WHERE `inv_id` = ? ;")),
    mysql:execute(mysql_connection, dbreq_inventory_remove_owner, [InventoryID]).
%%-------------------------------------------------------------------------------------------------
% add to inventory.
inventory_add(Item, InventoryID) ->
    #item{id=ObjID, type=Type, q=Q, amount=Amount, ticks_left=TicksLeft, ticks=Ticks, x=X,y=Y,num=Num} = Item,
    TypeID = objects_server:get_typeid(Type),

    mysql:prepare(dbreq_inventory_add, list_to_binary(
    "INSERT INTO `inventory` (`id`, `inv_id`,`type`,`q`,`amount`,`ticks_left`,`ticks`,`x`,`y`,`num`) VALUES (?,?,?,?,?,?,?,?,?,?);")),
    mysql:execute(mysql_connection, dbreq_inventory_add, [
                                  ObjID,InventoryID,TypeID,Q,Amount,TicksLeft, Ticks, X,Y,Num
                                                        ]).
%%-------------------------------------------------------------------------------------------------
% update item in inventory
inventory_update(none, _) -> ok;
inventory_update(Item, InventoryID) ->
	?DEBUG("inventory_update ~p ~p",[Item, InventoryID]),
    #item{id=ObjID, type=Type, q=Q, amount=Amount, ticks_left=TicksLeft, ticks=Ticks, x=X,y=Y,num=Num} = Item,
    TypeID = objects_server:get_typeid(Type),

    mysql:prepare(dbreq_inventory_update, list_to_binary(
        "UPDATE `inventory` SET `inv_id`=?, `type`=?, `q`=?, `amount`=?, `ticks_left`=?, `ticks`=?, `x`=?, `y`=?, `num`=? WHERE `id` = ?")),
        mysql:execute(mysql_connection, dbreq_inventory_update, [
                                  InventoryID,TypeID,Q,Amount,TicksLeft, Ticks, X,Y,Num,ObjID]).
%%-------------------------------------------------------------------------------------------------
% обновить текущий онлайн
update_online_count(Count) ->
	Time = timer:now_diff(now(), {0,0,0}) div 1000000,
    mysql:prepare(dbreq_update_online_count, list_to_binary(
        "UPDATE `server` SET `val`=?, `time`=from_unixtime(?)  WHERE `val_name` = ?")),
        mysql:execute(mysql_connection, dbreq_update_online_count, [Count, Time, "online_count"]).

%%-------------------------------------------------------------------------------------------------
% записать событие входа игрока
player_logged_result(Login, AccountID, Result) when (Result == true) or (Result == already_logged) ->
	Res = case Result of
		true -> 0;
		bad_pass -> 1;
		already_logged -> 2;
		not_found -> 3
	end,
	mysql:prepare(dbreq_player_logged_result1, list_to_binary(
    "INSERT INTO `log_login` (`account_id`,`login`,`result`) VALUES (?,?,?);")),
    mysql:execute(mysql_connection, dbreq_player_logged_result1, [AccountID, Login, Res]);
player_logged_result(Login, _AccountID, Result) ->
	Res = case Result of
		true -> 0;
		bad_pass -> 1;
		already_logged -> 2;
		not_found -> 3;
		fail_account_online -> 4
	end,
	mysql:prepare(dbreq_player_logged_result2, list_to_binary(
    "INSERT INTO `log_login` (`login`,`result`) VALUES (?,?);")),
    mysql:execute(mysql_connection, dbreq_player_logged_result2, [Login, Res]).

%%-------------------------------------------------------------------------------------------------
% получить грид на котором остановилась работа краулера в последний раз
get_last_crawler_grid(Sg) ->
    mysql:prepare(dbreq_get_last_crawler_grid,
        <<"SELECT `val` FROM `server` WHERE `server`.`val_name` = ? ;">>),
    try mysql:execute(mysql_connection, dbreq_get_last_crawler_grid, ["crawler_"++integer_to_list(Sg)]) of
    {data, #mysql_result{ rows = []}} ->
                err;
    {data, #mysql_result{ rows = [[Data|_]|_] }} ->
                Data;
    A -> A
    catch _:_ -> err
    end.
%%-------------------------------------------------------------------------------------------------
% обновить грид краулера.
update_last_cralwer_grid(Sg, Grid) ->
	execute(dbreq_update_last_cralwer_grid,
				list_to_binary("UPDATE `server` SET `val`=? WHERE `val_name`=?;"),
				[Grid, "crawler_"++integer_to_list(Sg)]).

%%-------------------------------------------------------------------------------------------------
% получить текущее время сервера
get_server_time() ->
 	mysql:prepare(dbreq_get_server_time,
        <<"SELECT `val` FROM `server` WHERE `server`.`val_name` = ? ;">>),
    try mysql:execute(mysql_connection, dbreq_get_server_time, ["global_time"]) of
    {data, #mysql_result{ rows = []}} ->
                err;
    {data, #mysql_result{ rows = [[Data|_]|_] }} ->
                Data;
    A -> A
    catch _:_ -> err
    end.

%%-------------------------------------------------------------------------------------------------
% обновить текущее время сервера
update_server_time(T) ->
    execute(dbreq_update_server_time, list_to_binary(
			"UPDATE `server` SET `val`=? WHERE `val_name`=?;"),
			[T, "global_time"]).

%%-------------------------------------------------------------------------------------------------
% загрузить бафы из базы
buff_load(TargetID) ->
	mysql:prepare(dbreq_buff_load, list_to_binary(
        "SELECT `target_id`,`type`,`duration`,`state` FROM `buffs` WHERE `target_id`= ?")),
    try mysql:execute(mysql_connection, dbreq_buff_load, [TargetID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.

%%-------------------------------------------------------------------------------------------------
% удалить бафф
buff_remove(TargetID, Type) ->
	mysql:prepare(dbreq_buff_remove, list_to_binary(
        "DELETE FROM `buffs` WHERE `target_id` = ? AND `type`=?;")),
    mysql:execute(mysql_connection, dbreq_buff_remove, [TargetID, buff:get_typeid(Type)]).

%%-------------------------------------------------------------------------------------------------
% обновить бафф
buff_update(#buff{target_id=TargetID, type=Type, duration=Duration, state = State}) ->
	TypeID = buff:get_typeid(Type),

    mysql:prepare(dbreq_buff_update, list_to_binary(
        "UPDATE `buffs` SET `duration`=?, `state`=? WHERE `target_id` = ? AND `type` = ?")),
        mysql:execute(mysql_connection, dbreq_buff_update, [
                                  Duration, term_to_binary(State), TargetID, TypeID]).

%%-------------------------------------------------------------------------------------------------
% добавить бафф
buff_add(#buff{target_id=TargetID, type=Type, duration=Duration, state = State}) ->
	TypeID = buff:get_typeid(Type),

    mysql:prepare(dbreq_buff_add, list_to_binary(
    "INSERT INTO `buffs` (`target_id`, `duration`,`type`,`state`) VALUES (?,?,?,?);")),
    mysql:execute(mysql_connection, dbreq_buff_add, [
                                  TargetID, Duration, TypeID, term_to_binary(State)
                                                        ]).


%%-------------------------------------------------------------------------------------------------
% обновить стат
stat_update(#player_stats{
				agi=#stat{level=Agi, fep=AgiFep},
				cons=#stat{level=Cons, fep=ConsFep},
				perc=#stat{level=Perc, fep=PercFep},
				dex=#stat{level=Dex, fep=DexFep},
				str=#stat{level=Str, fep=StrFep},
				int=#stat{level=Int, fep=IntFep},
				wis=#stat{level=Wis, fep=WisFep},
				cha=#stat{level=Cha, fep=ChaFep}
				}, CharID) ->
    mysql:prepare(dbreq_stat_update, list_to_binary(
        "UPDATE `char` SET `agi_level`=?, `agi_fep`=?,`cons_level`=?,`cons_fep`=?,`perc_level`=?,`perc_fep`=?,"++
		"`dex_level`=?,`dex_fep`=?,`str_level`=?,`str_fep`=?,`int_level`=?,`int_fep`=?,`wis_level`=?,`wis_fep`=?,"++
		"`cha_level`=?,`cha_fep`=?"++
			" WHERE `id` = ?")),
        mysql:execute(mysql_connection, dbreq_stat_update, [
                                 Agi, AgiFep, Cons, ConsFep, Perc, PercFep, Dex, DexFep, Str, StrFep, Int, IntFep,
								 Wis, WisFep, Cha, ChaFep,
								 CharID]).

%%-------------------------------------------------------------------------------------------------
% добавить бафф
bug_report_add(Acc, Subj, Text) ->
    mysql:prepare(dbreq_bug_report_add, list_to_binary(
    "INSERT INTO `bug_report` (`acc_id`, `subj`, `text`) VALUES (?,?,?);")),
    mysql:execute(mysql_connection, dbreq_bug_report_add, [
                                  Acc, Subj, Text
                                                        ]).

%%-------------------------------------------------------------------------------------------------
% load 
knowledge_load(CharID) ->
    mysql:prepare(dbreq_knowledge_load, list_to_binary(
        "SELECT `id`,`type`,`level` FROM `knowledges` WHERE `char_id` = ?")),
    try mysql:execute(mysql_connection, dbreq_knowledge_load, [CharID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.
knowledge_get_id(CharID, Type) ->
	TypeID = skills:get_knw_type2id(Type),
    mysql:prepare(dbreq_knowledge_get_id, list_to_binary(
        "SELECT `id` FROM `knowledges` WHERE `char_id` = ? AND `type` = ? LIMIT 1;")),
    try mysql:execute(mysql_connection, dbreq_knowledge_get_id, [CharID, TypeID]) of
    {data, #mysql_result{ rows = [[KnwID]] }} ->
                KnwID;
    _ -> err
    catch _ -> err
    end.
	
knowledge_add(CharID, Type, Level) ->
	TypeID = skills:get_knw_type2id(Type),
	
	mysql:prepare(dbreq_knowledge_add, list_to_binary(
		"INSERT INTO `knowledges` (`char_id`,`type`,`level`) VALUES (?,?,?);")),
	mysql:execute(mysql_connection, dbreq_knowledge_add, [
                          CharID, TypeID, Level
                                                ]).
knowledge_update(ID, Level) ->
	mysql:prepare(dbreq_knowledge_update, list_to_binary(
		"UPDATE `knowledges` SET `level`=? WHERE `id`=?;")),
	mysql:execute(mysql_connection, dbreq_knowledge_update, [
                          Level, ID
                                                ]).

%%-------------------------------------------------------------------------------------------------
% load 
skills_load(KnwID) ->
    mysql:prepare(dbreq_skills_load, list_to_binary(
        "SELECT `id`,`type`,`level`,`bar` FROM `skills` WHERE `knw_id` = ?")),
    try mysql:execute(mysql_connection, dbreq_skills_load, [KnwID]) of
    {data, #mysql_result{ rows = Rows }} ->
                Rows;
    _ -> err
    catch _ -> err
    end.
skill_get_id(KnwID, Type) ->
	TypeID = skills:get_skill_type2id(Type),
    mysql:prepare(dbreq_skill_get_id, list_to_binary(
        "SELECT `id` FROM `skills` WHERE `knw_id` = ? AND `type` = ? LIMIT 1;")),
    try mysql:execute(mysql_connection, dbreq_skill_get_id, [KnwID, TypeID]) of
    {data, #mysql_result{ rows = [[ResultID]] }} ->
                ResultID;
    _ -> err
    catch _ -> err
    end.

skill_add(0, _Type, _Level, _Bar) -> error(zero_knw_id);
skill_add(KnwID, Type, Level, Bar) ->
	TypeID = skills:get_skill_type2id(Type),
	
	mysql:prepare(dbreq_skill_add, list_to_binary(
		"INSERT INTO `skills` (`knw_id`,`type`,`level`,`bar`) VALUES (?,?,?,?);")),
	mysql:execute(mysql_connection, dbreq_skill_add, [
                          KnwID, TypeID, Level, Bar
                                                ]).

skill_update(ID, Level, Bar) ->
	mysql:prepare(dbreq_skill_update, list_to_binary(
		"UPDATE `skills` SET `level`=?, `bar`=? WHERE `id`=?;")),
	mysql:execute(mysql_connection, dbreq_skill_update, [
                          Level, Bar, ID
                                                ]).

chat_log(Msg, Nick, Channel, CharID) ->
	mysql:prepare(dbreq_chat_log, list_to_binary(
		"INSERT INTO `log_chat` (`char_id`,`msg`,`nick`,`channel`) VALUES (?,?,?,?);")),
	mysql:execute(mysql_connection, dbreq_chat_log, [
                          CharID, Msg, Nick, Channel
                                                ]).

claim_load(Sg, Grid) ->
	{X,Y,_} = map_server:get_map_coord(Sg, Grid),
	L2 = X div ?TILE_SIZE, T2 = Y div ?TILE_SIZE,
	R2 = L2 + ?GRID_SIZE-1, B2 = T2 + ?GRID_SIZE-1,
	?DEBUG("claim load rect : x ~p-~p  y ~p-~p",[L2,R2,T2,B2]),
    mysql:prepare(dbreq_claim_load, list_to_binary(
        "SELECT `owner_id`,`object_id`,`l`,`t`,`r`,`b`,`exp_combat`,`exp_industry`,`exp_nature` FROM `claims` WHERE 
	(((`l` > ?) and (`l` <= ?)) or ((`r` > ?) and (`r` <= ?)) or
  	((`l` < ?)  and (`r` >= ?)) or ((`l` < ?) and (`r` >= ?))) and
  	(((`t` > ?) and (`t` <= ?)) or ((`b` > ?) and (`b` <= ?)) or
  	((`t` < ?)  and (`b` >= ?)) or ((`t` < ?) and (`b` >= ?)))
;")),
    try mysql:execute(mysql_connection, dbreq_claim_load, [
														   L2, R2, L2, R2, L2, L2, R2, R2,
														   T2, B2, T2, B2, T2, T2, B2, B2
														  ]) of
    {data, #mysql_result{ rows = R }} ->
                R;
    _ -> err
    catch _ -> err
    end.
claim_get(OwnerID) ->
    mysql:prepare(dbreq_claim_get, list_to_binary(
        "SELECT `object_id`,`l`,`t`,`r`,`b`,`exp_combat`,`exp_industry`,`exp_nature` FROM `claims` WHERE `owner_id`=?;")),
    try mysql:execute(mysql_connection, dbreq_claim_get, [OwnerID]) of
    {data, #mysql_result{ rows = [[ObjectID, L,T,R,B, EC,EI,EN]] }} ->
                #claim_personal{owner_id=OwnerID, object_id=ObjectID, rect={L,T,R,B}, exp=#exp{combat=EC, industry=EI, nature=EN}};
    _ -> err
    catch _ -> err
    end.
claim_add(#claim_personal{owner_id=OwnerID, object_id=ObjectID, rect={L,T,R,B}, exp=#exp{combat=EC, industry=EI, nature=EN}}) ->	
	mysql:prepare(dbreq_claim_add, list_to_binary(
		"INSERT INTO `claims` (`owner_id`,`object_id`,`l`,`t`,`r`,`b`,`exp_combat`,`exp_industry`,`exp_nature`) VALUES (?,?,?,?,?,?,?,?,?);")),
	mysql:execute(mysql_connection, dbreq_claim_add, [
                       OwnerID, ObjectID, L,T,R,B, EC, EI, EN
                                                ]).

claim_changed(#claim_personal{object_id=ObjectID, owner_id=OwnerID, rect={L,T,R,B}, exp=#exp{combat=EC, industry=EI, nature=EN}}) ->
	mysql:prepare(dbreq_claim_changed, list_to_binary(
		"UPDATE `claims` SET `object_id`=?, `l`=?, `t`=?, `r`=?, `b`=?, `exp_combat`=?, `exp_industry`=?, `exp_nature`=? WHERE `owner_id`=?;")),
	mysql:execute(mysql_connection, dbreq_claim_changed, [
                      ObjectID, L,T,R,B, EC, EI, EN, OwnerID
                                                ]).
	
claim_remove(OwnerID) ->
	mysql:prepare(dbreq_claim_remove, list_to_binary(
        "DELETE FROM `claims` WHERE `owner_id` = ?;")),
    mysql:execute(mysql_connection, dbreq_claim_remove, [OwnerID]).
	