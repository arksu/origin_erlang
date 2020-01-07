% кнопки мыши
-define(MB_LEFT, 0).
-define(MB_RIGHT, 1).
-define(MB_MIDDLE, 2).
-define(MB_DOUBLE, 3).

-define(MOD_CONTROL, 1).
-define(MOD_SHIFT, 2).
-define(MOD_ALT, 4).


-define(GAME_TICK, 5). % период между гейм тиками (сессия, сервер объектов)
-define(UPDATE_ONLINE_TIME, 10000). % период между апдейтами онлайна в бд
-define(PING_TICK, 10000). % период проверки процесса игрока
-define(PROGRESS_STAGES, 30). % стадий прогресса (песочные часы)

-define(PING_TIMEOUT, 310). % макс время ожидания ответа от клиента на пинг пакет (секунды)
% макс раз в это время должен придти пакет пинга от клиента. сервер отвечает понгом сразу на пакет.
% периодичность посылки пингов задается еще и на клиенте

-define(PLAYER_VISIBLE_DISTANCE_RECT, 550). % size of visible zone
-define(PLAYER_VISIBLE_UPDATE, 100). % visible rect moved
-define(PLAYER_MOVE_VECTOR_LENGTH, 350). % length vector for line move
-define(PLAYER_COLLISION_DISTANCE, 100). % must be more than any single object, rect size = x*2
-define(COLLISION_ITERATION_LENGTH, 3).
-define(LOG_PATH, "origin.log"). % имя лог файла и путь к нему
-define(MIN_TICK_ID, 1000). % нижнй предел ид тик объектов
-define(MAX_TICK_ID, 2000). % верхний предел ид тик объектов
-define(TIMER_RESOLUTION, 333). % разрешение таймера времени всего сервера
-define(VISIBLE_UPDATE_TIME, 500). % время между обновлениями зон видимости
-define(MOVE_TRY_COUNT, 10). % сколько попыток для обсчета движения

% move types
-define(MOVE_LAND, 0).
-define(MOVE_WATER, 1).
-define(MOVE_SWIMMING, 2).

% knowledges
-define(KNW_SURVIVAL, 1).
-define(KNW_AGRICULTURE, 2).

%%----------------------------------------------------------------------------------------
sleep(T) -> receive after T -> true end.



%% ---------------------------------
%% Logging mechanism

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    a1_logger:debug_msg(?MODULE,?LINE,Format, Args)).
-define(DEBUG(Format),
    a1_logger:debug_msg(?MODULE,?LINE,Format, [])).

-define(INFO_MSG(Format, Args),
    a1_logger:info_msg(?MODULE,?LINE,Format, Args)).
-define(INFO_MSG(Format),
    a1_logger:info_msg(?MODULE,?LINE,Format, [])).

-define(WARNING_MSG(Format, Args),
    a1_logger:warning_msg(?MODULE,?LINE,Format, Args)).
-define(WARNING_MSG(Format),
    a1_logger:warning_msg(?MODULE,?LINE,Format, [])).

-define(ERROR_MSG(Format, Args),
    a1_logger:error_msg(?MODULE,?LINE,Format, Args)).
-define(ERROR_MSG(Format),
    a1_logger:error_msg(?MODULE,?LINE,Format, [])).

-define(CRITICAL_MSG(Format, Args),
    a1_logger:critical_msg(?MODULE,?LINE,Format, Args)).
-define(CRITICAL_MSG(Format),
    a1_logger:critical_msg(?MODULE,?LINE,Format, [])).