%% Author: arksu
%% Created: 23.07.2011
%% Description: TODO: Add description to a1_utils
-module(a1_utils).

-export([list_exist/2, send_msg/2, send_msg_sg/3, wait_servers/1,
		 get_grids/1,
		 get_in_rect/2,
		 keydelete/3
		 ]).

-include("map.hrl").
-include("types.hrl").

%-compile(export_all).

list_exist([], _) -> true;
list_exist(List1, List2) ->
	[H|T] = List1,
	case lists:member(H, List2) of
		true -> list_exist(T, List2);
		false -> false
	end.

% send Msg to all Pids in list
send_msg([], _) -> ok;
send_msg([{P,_}|T], Msg) ->  P ! Msg,  send_msg(T, Msg);
send_msg([P|T], Msg) ->  P ! Msg,  send_msg(T, Msg).
 
% отправить сообещиние в нужный воркер супергрида
% List = [{Pid, Sg}]
send_msg_sg(List, Sg, Msg) ->
    lists:foreach(fun ({Pid, SgN}) -> 
                           if SgN == Sg -> Pid ! Msg; true -> ok end
                  end, List).

% [Names] = list of server names need to be started
wait_servers(Names) ->
	case list_exist(Names, registered()) of 
		true -> ok;
		false -> sleep(333), wait_servers(Names)
	end.

sleep(T) -> receive after T -> true end.

%---------------------------------------------------------------------------------------
% get 9 grids from coord, return [{Sg,Grid}]
get_grids({Sg, Grid}) ->
	get_grids(map_server:get_map_coord(Sg, Grid));
get_grids({X,Y,Lv}) ->
    Size = ?GRID_FULL_SIZE,
                    get_grids({X-Size,   Y-Size, Lv},     
                    get_grids({X-Size,   Y, Lv},          
                    get_grids({X-Size,   Y+Size, Lv},     
                    
                    get_grids({X,        Y-Size, Lv},     
                    get_grids({X,        Y, Lv},          
                    get_grids({X,        Y+Size, Lv},     
                    
                    get_grids({X+Size,   Y-Size, Lv},     
                    get_grids({X+Size,   Y, Lv},          
                    get_grids({X+Size,   Y+Size, Lv},     [] ) ) ) ) ) ) ) ) ).
    
get_grids({X,Y,Lv}, Acc) ->
    case map_server:get_grid_coord(round(X), round(Y), Lv) of
        {coord_error} ->
            Acc;
        {{_,_},{_,_},{_,_},Sg, Grid, _} ->
            [{Sg,Grid}|Acc]
    end.


%---------------------------------------------------------------------------------------
% попадает ли точка в прямоугольник
get_in_rect({X,Y}, {L,T,R,B}) when (X >= L) and (X =< R) and (Y >= T) and (Y =< B) -> true;
get_in_rect(_, _) -> false.

%---------------------------------------------------------------------------------------
% удаляет все вхождения ключа
keydelete(K, N, L) when is_integer(N), N > 0 ->
    keydelete3(K, N, L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key -> keydelete3(Key, N, T);
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].
%---------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------
