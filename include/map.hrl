%        top
%         |
%         |
%         |
%         |
%-left---------right
%         |
%         |
%         |
%         |
%         bottom

%% how much sg left and top behind center of world
-define(LEFT_SUPERGRID, 0).
-define(TOP_SUPERGRID, 0).
%% how much sg after center of world
-define(RIGHT_SUPERGRID, 1).
-define(BOTTOM_SUPERGRID, 1).

%% tile size
-define(TILE_SIZE, 12). 
%% size of minimap (1 grid)
-define(GRID_SIZE, 100).
%% size of supergrid in grids (minimap)
-define(SUPERGRID_SIZE, 50).

%% max levels 0 - 10
-define(MAX_LEVEL, 10).


%% Lv for instance id
-define(INSTANCE_BEGIN_ID, 400).
%-------------------------------------------------------------------------------------------------
 
%% count of all supergrids 
-define(SUPERGRID_COUNT, (?LEFT_SUPERGRID + ?RIGHT_SUPERGRID) * (?TOP_SUPERGRID + ?BOTTOM_SUPERGRID)).

-define(GRID_FULL_SIZE, ?TILE_SIZE * ?GRID_SIZE).	
-define(SUPERGRID_FULL_SIZE, ?TILE_SIZE * ?GRID_SIZE * ?SUPERGRID_SIZE).

-define(MIN_X_COORD, -?LEFT_SUPERGRID * ?SUPERGRID_FULL_SIZE).
-define(MIN_Y_COORD, -?TOP_SUPERGRID * ?SUPERGRID_FULL_SIZE).
-define(MAX_X_COORD, ?RIGHT_SUPERGRID * ?SUPERGRID_FULL_SIZE).
-define(MAX_Y_COORD, ?BOTTOM_SUPERGRID * ?SUPERGRID_FULL_SIZE).

%-------------------------------------------------------------------------------------------------
