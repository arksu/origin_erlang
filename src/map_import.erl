%% Author: arksu
%% Created: 12.10.2011
%% Description: TODO: Add description to map_import
-module(map_import).

-compile(export_all).
-include("defines.hrl").

run(Sg) ->
	import_grid(Sg, 0),
	import_obj(Sg).

%---------------------------------------------------------------------------------
import_grid(_Sg, Grid) when Grid >= 2500 -> ok;
import_grid(Sg, Grid) ->
	% формируем имя файла
	Filename = "map/grid_"++integer_to_list(Grid)++".map",
	% читаем
	case file:read_file(Filename) of
		{ok, Bin} ->
			% пишем в базу
			?DEBUG("grid import ~p file=~p",[Grid, Filename]),
			db_server:map_set_grid(Sg, Grid, Bin);
		{error, _Reason} ->
			%?DEBUG("file ~p error: ~p",[Filename, _Reason]),
			error_read_file
	end,
	import_grid(Sg, Grid+1).
%---------------------------------------------------------------------------------
import_obj(Sg) ->
	% формируем имя файла
	Filename = "map/objects.bin",
	% читаем
	case file:read_file(Filename) of
		{ok, Bin} ->
			import_obj(Sg,Bin, 1);
		{error, _Reason} ->
			%?DEBUG("file ~p error: ~p",[Filename, _Reason]),
			ok
	end.
%---------------------------------------------------------------------------------
import_obj(_,<<>>, _) -> ok;
import_obj(Sg, <<X:32/integer-signed-little, Y:32/integer-signed-little, Type:8, Bin/binary>>, Count) ->
	if (Count rem 1000) == 0 ->
	?DEBUG("x=~p y=~p type=~p id=~p", [X,Y,Type, Count]);
	true -> ok end,


	db_server:gen_object(Sg, X, Y, 2000+Count,
			case Type of
				1 -> 4;
				2 -> 59;
				3 -> 5;
				4 -> 8;
				_ -> 4
			end
						 ),
	import_obj(Sg, Bin, Count+1).
