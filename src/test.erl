-module(test).
-compile(export_all).

-include("defines.hrl").
-include("base_io.hrl").

gen() ->
    gen(625000, []).

gen(0, Acc) ->
    Acc;
gen(Count, Acc) ->
    gen(Count-1, [{random:uniform(55000), random:uniform(55000), random:uniform(55)}|Acc]).



%% max(N)
%% Create N processes then destroy them
%% See how much time this takes
 
testing() ->
	io:format("stack:~p~n" ,[erlang:get_stacktrace()]).

max(N) ->
	io:format("stack:~p~n" ,[erlang:get_stacktrace()]),
	testing(),
    
        Max = erlang:system_info(process_limit),
        io:format("Maximum allowed processes:~p~n" ,[Max]),
        statistics(runtime),
        statistics(wall_clock),
        L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
        {_, Time1} = statistics(runtime),
        {_, Time2} = statistics(wall_clock),
        lists:foreach(fun(Pid) -> Pid ! die end, L),
        U1 = Time1 * 1000 / N,
        U2 = Time2 * 1000 / N,
        io:format("Process spawn time=~p (~p) microseconds~n" ,
                [U1, U2]).


wait() ->
        receive
                die -> void
        end.
 

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].

tt([]) -> ok;
tt(List) ->
	[H|T] = List,
	io:fwrite("~p~n", [H]),
	tt(T).


m2() ->
	DataTile = db_server:fill_grid(2),
    DataLevel = db_server:fill_levels(),
	Sg = 0, Grid = 2000,
	{X,Y,Lv} = map_server:get_map_coord(Sg, Grid),
	?DEBUG("map coord ~p ~p ~p",[X,Y,Lv]),
	
	L = lists:append(DataTile, DataLevel),
	?DEBUG("len = ~p",[length(L)]),
    grid_update_server:send_test_msg( 
	  list_to_binary([write_byte(10),write_int_BE(0),write_int_LE(256),L])
	  ).
m1() ->
    grid_update_server:send_test_msg(
	  list_to_binary([write_byte(9),write_int_BE(0),write_int_LE(256)])
	  ).
m3() ->
    grid_update_server:send_test_msg(
	  list_to_binary([write_byte(11)])
	  ).

msg(M) ->
    grid_update_server:send_test_msg(M).

fun1() ->
	fun 
	   (X) when X == 2 -> true;
	   (_) -> false 
	end.

filter([],_) -> true;
filter([H|T], F) ->
	case F(H) of 
		true -> false;
		false -> filter(T,F)
	end.
lt([], Acc) -> lists:reverse(Acc);
lt([H|T], Acc) -> lt(T, [H|Acc]).

ln(0, Acc) -> Acc;
ln(C, Acc) -> ln(C-1, [C|Acc]).


some(P) ->
	receive
		some -> ok
	after 10000 -> P ! {done}
	end.

start() ->
	process_flag(trap_exit, true),
	P = spawn_link(?MODULE, some, [self()]),
	loop(P).

loop(P) ->
	receive
		{ping, From} -> io:fwrite("pong"), From ! pong, loop(P);
		M -> io:fwrite("msg ~p ~n", [M]), loop(P)
	end.
