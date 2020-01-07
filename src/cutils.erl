%% Author: arksu
%% Created: 17.01.2012
%% Description: Utils for erlang written in pure C
-module(cutils).

-export([
		 get_tile/2, 
		 set_tile/3, 
		 get_object_typeid/1, 
		 get_object_type/1, 
		 is_object_item/1, 
		 get_object_grids/6, 
		 get_object_bounds/1,
		 get_object_collision/2
		]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("../bin/cutils", 0).


get_tile(_Data, _Index) ->
	exit(nif_library_not_loaded).

set_tile(_Data, _Index, _Tile) ->
	exit(nif_library_not_loaded).

get_object_type(_) ->
	exit(nif_library_not_loaded).

get_object_typeid(_) ->
	exit(nif_library_not_loaded).

is_object_item(_) ->
	exit(nif_library_not_loaded).

get_object_grids(_Type, _X, _Y, _XX, _YY, _Lv) ->
	exit(nif_library_not_loaded).

get_object_bounds(_Type) ->
	exit(nif_library_not_loaded).

get_object_collision(_Type1, _Type2) ->
	exit(nif_library_not_loaded).