%% Author: arksu
%% Created: 13.02.2012
%% Description: управление списком гридов для своего уровня земли. добавление удаление хранение

-module(grid_manager).


-export([
			start_link/1, start/1
	   ]). 

%% грид менеджер хранит список всех гридов одного уровня земли

-record(state, {
				level = error,	% уровень земли
				sg_list = [], 	% список супергридов #sg_rec, чтобы быстрее вести поиск.
				grids = []		% список пидов гридов, дублируем тут чтобы быстрее рассылать мессагу о тике
		}).

-record(sg_rec, {
				sg = error,  	% номер супергрида
				grid_list = []	% список пидов процессов гридов #grid_rec
		}).

-record(grid_rec, {
				grid = error,	% номер грида
				pid = error		% ид процесса грида
		}).

-include("defines.hrl").

%%-------------------------------------------------------------------------------------------
% заспавнить грид менеджера на указанный уровень
start_link(Level) ->
	spawn_link(?MODULE, start, [#state{level=Level}]).

%%-------------------------------------------------------------------------------------------
% старт грид менеджера
start(State) ->
	% ловим завершения процессов гридов
	process_flag(trap_exit, true),
	main_loop(State).

%%-------------------------------------------------------------------------------------------
% основной цикл
main_loop(State) ->
	main_loop(
	receive
		{get_grid_exist, Sg, Grid, From} ->  % get_grid_exist_ack
			#state{sg_list=List} = State,  

			% ищем супергрид
			case lists:keytake(Sg, #sg_rec.sg, List) of
				false ->
					From ! {get_grid_exist_ack, false},
					State;

				{value, #sg_rec{grid_list=GridList}, _Tail} ->
					% ищем грид внутри супергрида
					case lists:keyfind(Grid, #grid_rec.grid, GridList) of
						false ->
							From ! {get_grid_exist_ack, false},
							State;

						#grid_rec{pid=P} ->
							From ! {get_grid_exist_ack, P},
							State
					end
			end;
		% получить грид
		{get_grid, Sg, Grid, From} ->
			#state{sg_list=List, grids=GL} = State,  

			% ищем супергрид
			case lists:keytake(Sg, #sg_rec.sg, List) of
				false ->
					% добавим новый сг в список и заспавним грид
					P = grid:start_link(Sg, Grid),
					From ! {get_grid_ack, P},
					State#state{grids=[P|GL], % добавим в список
								% добавим в супергрид
								sg_list=[
							#sg_rec{sg=Sg, grid_list=[
											#grid_rec{grid=Grid, pid=P}
													 ]}
							| List
										 ]};

				{value, #sg_rec{grid_list=GridList}=SgRec, Tail} ->
					% ищем грид внутри супергрида
					case lists:keyfind(Grid, #grid_rec.grid, GridList) of
						false ->
							% заспавним грид
							P = grid:start_link(Sg, Grid),
							From ! {get_grid_ack, P},

							% добавим в список
							State#state{grids=[P|GL], % добавим в список
										sg_list=[
										SgRec#sg_rec{grid_list=[
													#grid_rec{grid=Grid, pid=P} | GridList
															   ]}
										| Tail
												]};
						#grid_rec{pid=P} ->
							From ! {get_grid_ack, P},
							State
					end
			end;

		% нормальное завершение процесса - ничего не делаем
		{'EXIT', Pid, normal} ->
			?DEBUG("exit normal msg ~p",[Pid]),
			State;
		% чей то краш
		{'EXIT', Pid, Reason} ->
			#state{sg_list=List, grids=GL} = State, 
			case lists:member(Pid, GL) of
				false ->
					?WARNING_MSG("exit process!!!  ~p ~p",[Pid, Reason]),
					% завершился процесс не грида, крашимся
					exit({exit_process, Pid, Reason});
				true ->
					?DEBUG("exit my grid ~p ~p",[Pid, Reason]),
					State#state{grids=lists:delete(Pid, GL), sg_list = delete_grid(Pid, List, [])}
			end;

		% игровой тик. разошлем сообщение всем гридам
		{tick, DT, T} ->
			#state{grids=GL} = State,
			lists:foreach(fun (P) -> P ! {tick, DT, T} end, GL),
			State;

		M ->
			?WARNING_MSG("unhandled msg ~p", [M]), State
	end).

%%-------------------------------------------------------------------------------------------
% удалить грид из списка супергридов по его пиду
delete_grid(_, [], Acc) -> Acc;
delete_grid(P, [#sg_rec{grid_list=GL}=S|T], Acc) ->
	case lists:keytake(P, #grid_rec.pid, GL) of
		false -> 
			delete_grid(P, T, [S|Acc]);
		{value, _, Tail} ->
			lists:append([S#sg_rec{grid_list=Tail}|Acc], T)
	end.
	
	
	