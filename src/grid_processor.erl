%% Author: arksu
%% Created: 08.01.2012
%% Description: TODO: Add description to grid_processor
-module(grid_processor).

-export([process/5]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("../bin/grid_processor", 0).


% [#visible_obj]
% _NeedSpawn = 0 | 1
process(_Neighbors, _Sg, _Grid, _GTime, _NeedSpawn) ->
	exit(nif_library_not_loaded).