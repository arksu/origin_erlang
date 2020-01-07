%%% -------------------------------------------------------------------
%%% Author  : arksu
%%% Description : world server
%%%
%%% Created : 13.02.2012
%%% -------------------------------------------------------------------
-module(world_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([
			get_next_id/0,
			get_grid/3,
   			get_grid/2,
   			get_grid/1,
			get_grid_exist/1,
			get_grid_exist/2,
			get_grid_exist/3
		 ]).

-export([start_link/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("map.hrl").
-include("defines.hrl").
-include("types.hrl").

-record(state, {
					free_id = error,
					levels = [] % список пидов грид менеджеров
		}).
-record(level_rec, {
					level = error,
					grid_manager_pid = error
		}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%----------------------------------------------------------------------------------------------------
% получить пид грида
get_grid(Sg, Grid) ->
	gen_server:call(?MODULE, {get_grid, Sg, Grid}).
get_grid({Sg, Grid}) ->
	gen_server:call(?MODULE, {get_grid, Sg, Grid});
get_grid({X,Y,Lv}) ->
	get_grid(map_server:get_sg_grid(X,Y,Lv)).
get_grid(X,Y,Lv) ->
	get_grid(map_server:get_sg_grid(X,Y,Lv)).

%----------------------------------------------------------------------------------------------------
% получить пид грида который реально существует. иначе вернет false
get_grid_exist(Sg, Grid) ->
	gen_server:call(?MODULE, {get_grid_exist, Sg, Grid}).
get_grid_exist({Sg, Grid}) ->
	gen_server:call(?MODULE, {get_grid_exist, Sg, Grid});
get_grid_exist({X,Y,Lv}) ->
	get_grid(map_server:get_sg_grid(X,Y,Lv)).
get_grid_exist(X,Y,Lv) ->
	get_grid(map_server:get_sg_grid(X,Y,Lv)).

%----------------------------------------------------------------------------------------------------
% return new obj_id for object, send msg to caller {new_id, ObjID}
get_next_id() ->
    gen_server:cast(?MODULE, {get_next_id, self()}),
    receive
        {new_id, ObjID} -> ObjID
    after 5000 ->
            timeout_get_next_freeid
    end.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ?INFO_MSG("~p start",[?MODULE]),
	a1_utils:wait_servers([db_request]),
	L = spawn_grid_managers(),

    case db_server:object_get_free_id() of
        err ->
            ?CRITICAL_MSG("FAILED GET FREE ID"),
            {ok, #state{levels = L}};
        ID ->
            ?DEBUG("free id is ~p", [ID]),
            {ok, #state{free_id = ID,
                        levels = L
                        }}
    end.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_grid, Sg, Grid}, _From, #state{levels=Levels}=State) ->
	Level = map_server:get_level(Grid),
	% ищем левел
	case lists:keyfind(Level, #level_rec.level, Levels) of
		false -> ?CRITICAL_MSG("level not found ~p", [Level]), exit(error_level);
		#level_rec{grid_manager_pid=P} ->
			% запрашиваем у него грид
			P ! {get_grid, Sg, Grid, self()},
			receive
					{get_grid_ack, PGrid} -> {reply, PGrid, State}
			after 5000 ->
					?CRITICAL_MSG("timeout wait get_grid ~p ~p", [Sg, Grid]), exit(error_timeout)
			end
	end;
handle_call({get_grid_exist, Sg, Grid}, _From, #state{levels=Levels}=State) ->
	Level = map_server:get_level(Grid),
	% ищем левел
	case lists:keyfind(Level, #level_rec.level, Levels) of
		false -> ?CRITICAL_MSG("level not found ~p", [Level]), exit(error_level);
		#level_rec{grid_manager_pid=P} ->
			% запрашиваем у него грид
			P ! {get_grid_exist, Sg, Grid, self()},
			receive
					{get_grid_exist_ack, PGrid} -> {reply, PGrid, State}
			after 5000 ->
					?CRITICAL_MSG("timeout wait get_grid ~p ~p", [Sg, Grid]), exit(error_timeout)
			end
	end;

handle_call(Request, _From, State) ->
    Reply = ok,
    ?WARNING_MSG("unhandled call ~p",[Request]),
    {reply, Reply, State}.


% return new obj_id for object, send msg to caller {new_id, ObjID}
handle_cast({get_next_id, From}, State) ->
    #state{free_id = Free} = State,
    db_server:object_update_free_id(Free+1),
    From ! {new_id, Free},
    {noreply, State#state{ free_id = Free+1 } };

handle_cast(Msg, State) ->
    ?WARNING_MSG("unhandled cast ~p",[Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tick, DT, T}, #state{levels=Levels}=State) ->
	lists:foreach(fun (#level_rec{grid_manager_pid=P}) ->
						   P ! {tick, DT, T}
				  end, Levels),

    {noreply, State};

handle_info(Info, State) ->
    ?WARNING_MSG("unhandled msg ~p",[Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
% заспавнить грид менеджеры для всех уровней
spawn_grid_managers() ->
	spawn_grid_managers(?MAX_LEVEL, []).

spawn_grid_managers(-1, Acc) -> Acc;
spawn_grid_managers(Level, Acc) ->
	P = grid_manager:start_link(Level),
	spawn_grid_managers(Level - 1,
		[#level_rec{level=Level, grid_manager_pid=P} | Acc]
					   ).
