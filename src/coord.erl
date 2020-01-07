%% Author: arksu
%% Created: 19.08.2012
%% Description: TODO: Add description to coord
-module(coord).

-export([]).
-compile(export_all).

% получить дистанцию между точками
get_distance({X1,Y1,_}, {X2,Y2,_}) ->
    math:sqrt( (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1) ).

get_distance(X1,Y1,X2,Y2) ->
    math:sqrt( (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1) ).

% пересекаются ли прямоугольники
is_rect_intersect({L1,T1,R1,B1},{L2,T2,R2,B2}) when
  (((L1 > L2) and (L1 =< R2)) or ((R1 > L2) and (R1 =< R2)) or
  ((L2 > L1)  and (L2 =< R1)) or ((R2 > L1) and (R2 =< R1))) and
  (((T1 > T2) and (T1 =< B2)) or ((B1 > T2) and (B1 =< B2)) or
  ((T2 > T1)  and (T2 =< B1)) or ((B2 > T1) and (B2 =< B1))) -> true;
is_rect_intersect(_,_) -> false.

% попадает ли точка в прямоугольник
get_in_rect({X,Y}, {L,T,R,B}) when (X >= L) and (X =< R) and (Y >= T) and (Y =< B) -> true;
get_in_rect(_, _) -> false.

% сделать правильный RECT из координат для блокировки
make_rect(X1, Y1, X2, Y2) ->
	{L,R} = if 
				X2 > X1 -> {X1, X2};
				true -> {X2, X1}
			end,
	{T,B} = if 
				Y2 > Y1 -> {Y1, Y2};
				true -> {Y2, Y1}
			end,
	{L,T,R,B}.

add_rect_size({L,T,R,B}, Sz) ->
	{L-Sz, T-Sz, R+Sz, B+Sz}.

%%   (((l > L2) and (l =< R2)) or ((r > L2) and (r =< R2)) or
%%   ((l < L2)  and (r >= L2)) or ((l < R2) and (r >= R2))) and
%%   (((t > T2) and (t =< B2)) or ((b > T2) and (b =< B2)) or
%%   ((t < T2)  and (b >= T2)) or ((t < B2) and (b >= B2)))