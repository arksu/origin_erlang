%% Author: arksu
%% Created: 28.06.2011
%----------------------------------------------------------------------------------------------------
% сетевой пакет
-record(packet, {
        id=error,
		data=error
                      }).


%----------------------------------------------------------------------------------------------------
-record(exp, {
        nature=0,
		industry=0,
		combat=0
                      }).
%----------------------------------------------------------------------------------------------------
% тайл
-record(tile, {
               	type = none, % тип тайла (лес, вода, луг)
				flag = 0
			  % !!!!!! update cutils.c !!!!!!!!!!
            }).
%----------------------------------------------------------------------------------------------------
% гейм объект - находящийся на земле.
-record(object, {
                 id = 0, % !!!! не менять положение. завязан поиск по списку.
                 type = none,  % !!!! не менять положение. завязан МОДУЛЬ НА С
                 coord = {0,0,0},
                 params = [],
                    % ObjectParam = {type, Data}
                 state = none,
				 time = 0, % время создания (изменения) объекта % !!!! не менять положение. завязан МОДУЛЬ НА С
				 hp = {10, 10},
				 q = 1
                 }).

%----------------------------------------------------------------------------------------------------
-record(item, {
        id = error, % !!!! не менять положение. завязан поиск по списку.
		type = error, % !!!! не менять положение. завязан поиск по списку.
		q = error,
		amount = error,
		ticks_left = 0, % сколько осталось до полного прогресса
		ticks = 0, % сколько всего тиков
		x = 0, % всегда должно быть меньше 200 (условие загрузки инвентаря)
		% 255 - вещь в руке
		% 201 - эквип. голова.
		% 202 - эквип. тело.
		% 203 - эквип. левая рука.
		% 204 - эквип. правая рука.
		% 205 - эквип. ноги.
		% 206 - эквип. ступни (ботинки).
		y = 0,
		num = 0,
		items = []
                      }).
%----------------------------------------------------------------------------------------------------
% deprecated
-record(mouse_click, {
        btn = error,
		x = 0, % координаты внутренного элемента по которому щелкнули (для инвентаря - координаты слота)
		y = 0,
		mx = 0, % координаты мыши внутри контрола
		my = 0,
		amx = 0, % абсолютные координаты мыши на экране
		amy = 0
                      }).
%----------------------------------------------------------------------------------------------------
-record(inv_click, { 
		objid = 0,
		inv_objid = 0,
        btn = error,
		mod = error,
		x = 0, 
		y = 0,
		ox = 0,
		oy = 0
                      }).
%----------------------------------------------------------------------------------------------------
% эквип игрока, в каждом слоте - итем
-record(equip, {
					head = none,
					lhand = none,
					rhand = none,
					body = none,
					legs = none,
					foots = none
                      }).
%----------------------------------------------------------------------------------------------------
% объект видимости
-record(visible_obj, {
					  x,
					  y,
					  id,
					  trans = 0, % прозрачен ли объект 1 или 0
					  stealth = 0, % скрытность объекта = int
					  typeid = 0, % ид типа
					  timer = 0 % время последнего апдейта когда объект видим
                      }).
%----------------------------------------------------------------------------------------------------
% эффект наложенный на персонажа. может быть как положительным так и отрицательным...
-record(buff, {
		target_id = 0, 	% ид цели на которую действует бафф
		duration = 0, 	% сколько времени осталось до конца, мсек
		time = 0, 		% сколько всего времени длится баф
		type = error, 	% тип бафа
		state = none 	% состояние
                      }).
%----------------------------------------------------------------------------------------------------
% стат игрока
-record(stat, {
				level = 1, % уровень
				fep = 0 % сколько уже отожрано
}).
%----------------------------------------------------------------------------------------------------
% список статов
-record(player_stats, {
				% каждое значение это #stat
			   	cons = 0,
			   	str = 0,
			   	perc = 0,
				agi = 0,
				dex = 0,
				int = 0,
				wis = 0,
				cha = 0
                      }).
%----------------------------------------------------------------------------------------------------
-record(knowledge, {
				id = 0,			
				base = none,	% nature, industry, combat
				type = none,	% название
				level = 0, 		% уровень
				skills = [] 	
}).
%----------------------------------------------------------------------------------------------------
-record(skill, {
				id = 0,
				type = none,	% название
				level = 0, 		% уровень (если 0 - значит не активен и очки не добавляются. если >=1 значит активен и добавляем очки опыта)
				bar = 0, 		% сколько уже получено
				max_bar = 0,	% максимальное значение бара для заполнения
				req_exp = 0		% сколько экспы надо чтобы взять скилл
}).

%----------------------------------------------------------------------------------------------------
% грид игрока
-record(player_grid, {
					  coord = error, 	% {Sg, Grid}
					  pid = error		% process id
					 }).

%----------------------------------------------------------------------------------------------------
% запись сервера игроков 
-record(player_server_rec, {
					  	socket = error,		% socket
					  	pid = error,		% process id
						account_id = none	%
					 }).

%----------------------------------------------------------------------------------------------------
-record(player_state, {
		parent = error,			% pid of player_server
		inited = false,			% logged?
        state = {none, none}, % {STATE, STATE_PARAMS}
        % {stay, none}
        % {move_point, {LastTime, X,Y}}
        % {move_object, {LastTime, TargetID} }
        % {move_follow, {LastTime, TargetID} }
        % {move_virtual_object, {LastTime, X,Y, Type } - have_collision

        name = [],              % name )
        accountid = none,       % acc id
		access_level = 0,       % admin rights
        charid = none,          % my objid
        action = none,          % action none | {ActionName, TicksToEnd, TicksLength, LastProgress, ObjID}
        control = self,         % what we control
        linked_to = none, 		% linked to object, that set my pos, couldnt control self #object
        linked_objid = none,    % linked obj for action, must be set to none with any move
        coord = {0, 0, 0},  	% {X, Y, Lv}
        speed = 3,				% current speed, stage (1-4)
		exp = #exp{},			% свободный опыт игрока
        skills = [],            % player skills for craft, build, etc...
        equip = #equip{},       % equipory. suit, weapons, tools, etc...
        inventory = [],         % inventory items list
		buffs = [], 			% список баффов наложенных на игрока
		stats = #player_stats{},% статы

        lift_object = none,     % lifting object #object
		dialogs = [],			% список названий открытых диалогов на клиенте
        reuse_time = {0, 0},    % откат скилла. глобальный {сколько осталось до конца, длина}
		next_action = none,		% следующее действие, запускается сразу после отката. текст. из списка доступных действий игроку

		target_objid = none,	% ид объекта цели. на которую будут производится действия
        item_hand = none,       % item in hand
        cursor = arrow,         % cursor mode arrow | lift ....
        context = none,         % items for context menu. {ObjID, [string]} | none, if none - havnt context menu
        place_obj = none,       % what we placing in client to build or spawn: none | {Type, ItemObjID, Action};  Action = place | build
        progress = none,        % value of progress last sended to client

        last_tick = 0,          % for connection timeout

		grids = [],				% гриды с которыми работаем #player_grid

        socket = none,          % tcp socket
		send_pid = none,		% send process pid

        visible_last_coord = {0,0,none}, % last coord that updated visible
        visible_zone = {0,0,0,0},  % visible zone L,T,R,B in game coords (11 points in tile)
        visible_buffer = [],    % [#visible_obj] список объектов попадающих в область видимости, все до фильтрации
		visible_timer = 0,		% таймер для апдейта видимости объектов
		visible_list = [],		% [#visible_obj] - список реально видимых объектов с учетом областей видимости, отфильтрованный список
		visible_client = [],	% [#visible_obj] список видимых клиенту объектов, те на которые сделали запрос инфы в сессию

        % health
		hp_regen_timer = 0,		% таймер регена хп.
        hp = 0, 				% 0 - 100
        shp = 0,
        hhp =0,

		stamina_regen_timer = 0, % таймер регена стамины
		stamina_consume_timer = 0, % таймер расхода стамины
		stamina = 0				% 0 - 100
                      }).

%----------------------------------------------------------------------------------------------------
% грид для работы с данными карты
-record(grid_state, {
			 	coord={0,0},		% {Sg, Grid}
				tiles=none,			% binary data of tiles
				static=[],
				tick=[],
				dyn=[],
				dynamic_pids=[],	% список процессов обслуживающих динамические объекты #dynamic_pid
				level=error,		% уровень грида. вычисляется напрямую из номера грида, нужен тут для уменьшения вычислений
				claims=[],			% список клаймов которые затрагивают грид #claim_personal

				is_loaded=false,	% true | false загружен ли грид
				on_load=[],			% список процессов которые надо уведомить при загрузке
				linked=[],			% список процессов кому я нужен. при деактивации того процесса мне придет unload
				worker_map=error,	% пид рабочего по загрузке карты
				worker_obj=error,	% пид рабочего по загрузке объектов грида
				worker_claims=error,% pid claims loader worker

				notify=[],			% список пидов подписавшихся на этот грид
				is_activate_requested = false, % была запрошена активация грида
				is_active=false,	% активен ли грид. становится только когда загружены все соседи
				neighbors=[],		% список соседних гридов #neighbor_rec

				time_updated = none,			% время моего последнего апдейта
				time_tick_updated = none,		% время последнего апдейта тик объектов
				process_tries_count = 0,		% количество попыток совершить апдейт грида
				locked = []						% #grid_lock_area - список залоченных областей (для обсчета коллизий)
    	}).

%----------------------------------------------------------------------------------------------------
% грид для работы с данными карты
-record(grid_lock_area, {
				area = error, % RECT область которая заблокирована
				owner = error % pid процесс который запросил  
				
					 }).

%----------------------------------------------------------------------------------------------------
% соседний грид
-record(neighbor_rec, {
				pid=error,			% ид процесса
				coord=error,		% координаты
				is_loaded=false,	% загружен ли сосед
				state=none,			% его состояние  (defines.h)
				state_recv_time=0 	% глобальное время получения стейта
		}).

%----------------------------------------------------------------------------------------------------
-record(claim_personal, {
				owner_id = 0,			% ид владельца (ид чара)
				object_id = 0,			% ид объекта менеджера
				rect = error,			% размер в тайловых координатах (LTRB)
				exp = #exp{},			% сколько экспы вкачано в клайм
				time_last_action = 0	% глобальное время последнего действия игрока, также обновляется при уменьшении клайма
		}).
