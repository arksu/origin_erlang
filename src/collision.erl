%% Author: arksu
%% Created: 09.01.2012
%% Description: TODO: Add description to collision
-module(collision).

-export([process/10]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("../bin/collision", 0).


% [#visible_obj]
process(_Grids, _ObjID, _Type, _VirtualType, _OldX, _OldY, _X, _Y, _Lv, _MoveType) ->
	exit(nif_library_not_loaded).