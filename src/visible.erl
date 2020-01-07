%% Author: arksu
%% Created: 04.01.2012
%% Description: TODO: Add description to visible
-module(visible).

-export([process/7]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("../bin/visible", 0).


% [#visible_obj]
process(_Buf, _OldList, _X, _Y, _Sight, _myID, _GlobalTime) ->
	exit(nif_library_not_loaded).