%%% -------------------------------------------------------------------
%%% Author  : arksu
%%% Description :
%%%
%%% Created : 18.02.2011
%%% -------------------------------------------------------------------
-module(map_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("map.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,
         status/0,
         update_tile/5,

         get_tile/2,
		 set_tile/3,
		 get_dig_ticks/1,
		 can_lay_stone/1,
		 can_lay_grass/1,
		 can_plow/1,
		 can_dig_hole/1,
		 can_dig/1,
		 spawn_tile_items/1,
         spawn_all_map_workers/0,
         map_worker_loop/1,

		 get_coord_sg/1,

         get_sg/3,
		 get_sg/1,
		 get_sg_grid/1,
         get_sg_grid/3,
		 get_sg_grid_index/3,
         get_grid_coord/3,
         get_grid_bounds/2,
         get_map_coord/2,
		 get_level/1,
		 tilify/2,
		 tilify_center/2]).

% данные о гриде передаются в виде списка тайлов


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("defines.hrl").
-include("types.hrl").

%% ====================================================================
-record(worker_state, {sg=error,
					   grids = [] % список {Grid, Data}
		}).

%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
status() ->
    gen_server:cast(?MODULE, {status}).

% сменить тайл
% вызывать из процесса игрока
update_tile(SPid, Sg, Grid, Index, Tile) ->
    gen_server:cast(?MODULE, {update_tile, SPid, Sg, Grid, Index, Tile}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ?DEBUG("map server start. minx=~p miny=~p maxx=~p maxy=~p",
        [?MIN_X_COORD, ?MIN_Y_COORD, ?MAX_X_COORD, ?MAX_Y_COORD]),
    MapWorkers = spawn_all_map_workers(),
    {ok, MapWorkers}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, MapWorkers) ->
    Reply = ok,
    {reply, Reply, MapWorkers}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({update_tile, SPid, Sg, Grid, Index, Tile}, MapWorkers) ->
	a1_utils:send_msg_sg(MapWorkers, Sg, {update_tile, SPid, Sg, Grid, Index, Tile}),
    {noreply, MapWorkers};

handle_cast({status}, MapWorkers) ->
    ?DEBUG("status ~p", [MapWorkers]),
    {noreply, MapWorkers}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({unload_grid, Sg, Grid}, MapWorkers) ->
	a1_utils:send_msg_sg(MapWorkers, Sg, {unload_grid, Grid}),
	{noreply, MapWorkers};

handle_info({update_grid, SPid, Sg, Grid, Data}, MapWorkers) ->
	a1_utils:send_msg_sg(MapWorkers, Sg, {update_grid, SPid, Sg, Grid, Data}),
	{noreply, MapWorkers};

% grid
handle_info({get_cache_grid, {Sg, Grid}, ToPid}, MapWorkers) ->
	a1_utils:send_msg_sg(MapWorkers, Sg, {get_cache_grid, Grid, ToPid}),
	{noreply, MapWorkers};

handle_info(Info, MapWorkers) ->
	?WARNING_MSG("unhandled msg ~p",[Info]),
    {noreply, MapWorkers}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _MapWorkers) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

% spawn all sg workers and return list of pids {Pid, SgNum}
spawn_all_map_workers() ->
    spawn_all_map_workers(?SUPERGRID_COUNT, []).
spawn_all_map_workers(0, Acc) ->
    Acc;
spawn_all_map_workers(Count, Acc) ->
    P = spawn_map_worker(Count-1),
    spawn_all_map_workers(Count-1, [{P, Count-1}|Acc]).

% spawn process and return Pid
spawn_map_worker(Sg) ->
    spawn_link(?MODULE, map_worker_loop, [#worker_state{sg=Sg, grids=[]}]).

map_worker_loop(#worker_state{sg=Sg, grids=Grids} = State) ->
    Result =
	receive
		% получить грид, отослать его сообщением запрашивающему
		{get_cache_grid, Grid, From} ->
			?DEBUG("get_cache_grid ~p",[Grid]),
			case lists:keyfind(Grid, 1, Grids) of
				false ->
					?DEBUG("no grid in map cache, load from db..."),
					% грида у меня нет. грузим из бд
            		case db_server:map_get_grid(Sg, Grid) of
						% грид не найден
            			[] ->
							?CRITICAL_MSG("grid not found in db! sg=~p grid~p", [Sg, Grid]),
							State;
                		Data ->
							% шлем данные тому кто запросил
							From ! {map_grid_loaded, Sg, Grid, Data},
							% уведомим кэш сервер
							%cache_server ! {grid_ready, map, Sg, Grid},
							% запоминаем грид у себя в кэше
							State#worker_state{ grids=[ {Grid, Data}|Grids ] }
        		    end;
				{Grid, Data} ->
					?DEBUG("grid in cache!"),
					% шлем данные тому кто запросил
					From ! {map_grid_loaded, Sg, Grid, Data},
					% уведомим кэш сервер
					%cache_server ! {grid_ready, map, Sg, Grid},
					State
			end;
		% выгрузить грид из памяти
		{unload_grid, Grid} ->
			?DEBUG("unload grid ~p",[Grid]),
			case lists:keytake(Grid, 1, Grids) of
				false ->
					?WARNING_MSG("havnt grid to unload! sg=~p grid=~p",[Sg, Grid]),
					State;
				{value, {Grid,_}, NewGrids} ->
					State#worker_state{grids=NewGrids}
			end;

        % обновить весь грид
        {update_grid, SPid, Sg, Grid, Data} ->
            case lists:keytake(Grid, 1, Grids) of
                false ->
                    State;
                {value, _, NewGrids} ->
                    db_server:map_set_grid(Sg, Grid, Data),
					% если пид верный - уведомим его
					case SPid of
						none -> ok;
						_ -> SPid ! {map_grid_loaded, Sg, Grid, Data}
					end,
                    State#worker_state{grids=[{Grid,Data}|NewGrids]}
            end;

		% обновить тайл. заменить указанным
        {update_tile, SPid, Sg, Grid, Index, Tile} ->
			case lists:keytake(Grid, 1, Grids) of
				false ->
                    ?WARNING_MSG("update_tile: fail, grid_data, not_found"),
					State;
                {value, {Grid,Data}, NewGrids} ->
                    NewData = set_tile(Data, Index, Tile),
                    ?DEBUG("update_tile: index=~p",[Index]),
                    db_server:map_set_grid(Sg, Grid, NewData),
                    SPid ! {map_grid_loaded, Sg, Grid, NewData},
					State#worker_state{grids=[{Grid,NewData}|NewGrids]}
            end;

		Msg -> ?WARNING_MSG("MAP_WORKER: unhandled message: ~p", [Msg]), State
    end,
    map_worker_loop(Result).

%% --------------------------------------------------------------------
% получить тайл из грида по указанному индексу
% return #tile
get_tile(Data, Index) ->
	cutils:get_tile(Data, Index).

%% --------------------------------------------------------------------
% поменять тайл в данных грида
% вернет новый грид
set_tile(Data, Index, Tile)->
	cutils:set_tile(Data, Index, Tile).

%% --------------------------------------------------------------------

%% get grid coord by abs coord
get_grid_coord(X, Y, _) when  (X <  ?MIN_X_COORD) orelse
                              (X >= ?MAX_X_COORD) orelse
                              (Y <  ?MIN_Y_COORD) orelse
                              (Y >= ?MAX_Y_COORD) ->
	%?CRITICAL_MSG("get_grid_coord: error coord ~p ~p ~p",[X,Y,Lv]),
    {coord_error};
get_grid_coord(X, Y, Lv) ->
    SgX = X div (?SUPERGRID_FULL_SIZE),
    SgY = Y div (?SUPERGRID_FULL_SIZE),
    Sg = ?LEFT_SUPERGRID + SgX + (SgY + ?TOP_SUPERGRID) * (?LEFT_SUPERGRID + ?RIGHT_SUPERGRID),
    GridX = (X rem (?SUPERGRID_FULL_SIZE)) div (?GRID_FULL_SIZE),
    GridY = (Y rem (?SUPERGRID_FULL_SIZE)) div (?GRID_FULL_SIZE),
    Grid = GridX + GridY * ?SUPERGRID_SIZE + Lv*?SUPERGRID_SIZE*?SUPERGRID_SIZE,
    IndexX = (X rem (?GRID_FULL_SIZE)) div (?TILE_SIZE),
    IndexY = (Y rem (?GRID_FULL_SIZE)) div (?TILE_SIZE),
    Index = IndexX + IndexY * ?GRID_SIZE,
    {{SgX, SgY}, {GridX, GridY}, {IndexX, IndexY}, Sg, Grid, Index}.


get_map_coord(Sg, Grid) ->
    Lv = Grid div (?SUPERGRID_SIZE*?SUPERGRID_SIZE),
    ModGrid = Grid rem (?SUPERGRID_SIZE*?SUPERGRID_SIZE),
    X = (Sg rem (?LEFT_SUPERGRID + ?RIGHT_SUPERGRID)) * ?SUPERGRID_FULL_SIZE + (ModGrid rem (?SUPERGRID_SIZE)) * ?GRID_FULL_SIZE,
    Y = (Sg div (?TOP_SUPERGRID + ?BOTTOM_SUPERGRID)) * ?SUPERGRID_FULL_SIZE + (ModGrid div (?SUPERGRID_SIZE)) * ?GRID_FULL_SIZE,
    {X,Y,Lv}.

get_level(Grid) ->
	Grid div (?SUPERGRID_SIZE*?SUPERGRID_SIZE).

get_sg(X,Y,Lv) ->
    case map_server:get_grid_coord(X,Y,Lv) of
        {coord_error} ->
			%?CRITICAL_MSG("get_sg: error coord ~p ~p ~p",[X,Y,Lv]),
			coord_error;
        {{_,_},{_,_},{_,_}, Sg, _, _} -> Sg
    end.
get_sg({X,Y,Lv}) ->
    case map_server:get_grid_coord(X,Y,Lv) of
        {coord_error} ->
			%?CRITICAL_MSG("get_sg: error coord ~p ~p ~p",[X,Y,Lv]),
			coord_error;
        {{_,_},{_,_},{_,_}, Sg, _, _} -> Sg
    end.
get_sg_grid(X,Y,Lv) ->
    case map_server:get_grid_coord(X,Y,Lv) of
        {coord_error} ->
			%?CRITICAL_MSG("get_sg_grid: error coord ~p ~p ~p",[X,Y,Lv]),
			coord_error;
        {{_,_},{_,_},{_,_}, Sg, Grid, _} -> {Sg,Grid}
    end.
get_sg_grid({X,Y,Lv}) ->
    case map_server:get_grid_coord(X,Y,Lv) of
        {coord_error} ->
			%?CRITICAL_MSG("get_sg_grid: error coord ~p ~p ~p",[X,Y,Lv]),
			coord_error;
        {{_,_},{_,_},{_,_}, Sg, Grid, _} -> {Sg,Grid}
    end.

get_sg_grid_index(X,Y,Lv) ->
    case map_server:get_grid_coord(X,Y,Lv) of
		{coord_error} ->
			%?CRITICAL_MSG("get_sg_grid_index: error coord ~p ~p ~p",[X,Y,Lv]),
			coord_error;
		{{_,_},{_,_},{_,_}, Sg, Grid, Index} -> {Sg,Grid,Index}
	end.

get_coord_sg({X,Y,Lv}) ->
	case map_server:get_grid_coord(X,Y,Lv) of
        {coord_error} ->
			%?CRITICAL_MSG("get_coord_sg: error coord ~p ~p ~p",[X,Y,Lv]),
			coord_error;
        {{_,_},{_,_},{_,_}, Sg, _, _} -> Sg
    end.

tilify(X,Y) ->
	{(X div ?TILE_SIZE) * ?TILE_SIZE,(Y div ?TILE_SIZE) * ?TILE_SIZE}.

tilify_center(X,Y) ->
	{(X div ?TILE_SIZE) * ?TILE_SIZE + ?TILE_SIZE div 2,(Y div ?TILE_SIZE) * ?TILE_SIZE + ?TILE_SIZE div 2}.


% получить рект грида в координатах
get_grid_bounds(Sg,Grid) ->
    {X,Y,_} = get_map_coord(Sg,Grid),
    {X,Y,X+?GRID_SIZE*?TILE_SIZE,Y+?GRID_SIZE*?TILE_SIZE}.


%% --------------------------------------------------------------------
% получить время на копку тайла
get_dig_ticks(_Type) ->
	5000.
%% --------------------------------------------------------------------
% что получается от тайла после копки
spawn_tile_items(#tile{type=Type}) ->
	case Type of
		water_low -> 	[inventory:spawn_item(clay, 10, 1)];
		sand ->		 	[inventory:spawn_item(sand, 10, 1)];
		_ -> 			[inventory:spawn_item(soil, 10, 1)]
	end.

%% --------------------------------------------------------------------
% можно ли замостить тайл камнем?
can_lay_stone(TileType) ->
	case TileType of
		grass 		-> true;
		plowed 		-> true;
		forest_leaf -> true;
		forest_fir 	-> true;
		dirt 		-> true;
		sand 		-> true;

		_ -> false
	end.

% можно ли засеять тайл травой?
can_lay_grass(TileType) ->
	case TileType of
		sett 		-> true;
		plowed 		-> true;
		dirt 		-> true;
		sand 		-> true;

		_ -> false
	end.

% можно ли вспахать тайл?
can_plow(TileType) ->
	case TileType of
		dirt 		-> true;
		forest_fir 	-> true;
		forest_leaf -> true;
		grass 		-> true;
		
		
		_ -> false
	end.

can_dig_hole(TileType) ->
	case TileType of
		water_low -> false;
		water_deep -> false;
		hole -> false;
		sett -> false;
		_ -> true
	end.

can_dig(TileType) ->
	case TileType of
		water_deep -> false;
		sett -> false;
		_ -> true
	end.





