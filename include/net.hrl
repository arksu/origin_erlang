-define(HEADER_LEN,							3).

-define(GAMESERVER_HELO, 					100).
-define(GAMESERVER_COOKIE, 					101).
-define(GAMESERVER_PING, 					102). % C
-define(GAMESERVER_PONG, 					103).
-define(GAMESERVER_LOGGED, 					104).
-define(GAMESERVER_SET_KIN_INFO, 			105).
-define(GAMESERVER_OBJ_MOVE, 				106).
-define(GAMESERVER_OBJ_DELETE, 				107).
-define(GAMESERVER_MAP_DATA, 				108).
-define(GAMESERVER_MAP_CLICK, 				109). % C
-define(GAMESERVER_OBJ_CLICK, 				110). % C NOT USED
-define(GAMESERVER_IACT_CLICK, 				111). % C
-define(GAMESERVER_OBJ_LINE_MOVE, 			112).
-define(GAMESERVER_OBJ_DELETE_PARAM, 		113).
-define(GAMESERVER_SET_DRAWABLE, 			114).
-define(GAMESERVER_CONTEXT_MENU, 			115).
-define(GAMESERVER_CONTEXT_ACTION, 			116). % C
-define(GAMESERVER_SETTINGS, 				117). % not used
-define(GAMESERVER_PROGRESS, 				118).
-define(GAMESERVER_CLIENT_SAY, 				119). % C
-define(GAMESERVER_SERVER_SAY, 				120).
-define(GAMESERVER_ACTIONS_LIST, 			121).
-define(GAMESERVER_ACTION, 					122). % C
-define(GAMESERVER_SET_FOLLOW, 				123).
-define(GAMESERVER_SET_LIGHT, 				124).
-define(GAMESERVER_CURSOR, 					125).
-define(GAMESERVER_GUI_ADD, 				126).
-define(GAMESERVER_GUI_REMOVE, 				127).
-define(GAMESERVER_GUI_UPDATE, 				128).
-define(GAMESERVER_GUI_CLICK, 				129). % C
-define(GAMESERVER_GUI_DESTROY, 			130). % C
-define(GAMESERVER_PLACE_OBJECT, 			131).
-define(GAMESERVER_SET_OPENED, 				132).
-define(GAMESERVER_OBJ_CLEAR_PARAMS, 		133).
-define(GAMESERVER_SYSTEM_MSG, 				134).
-define(GAMESERVER_CRAFT_LIST, 				135).
-define(GAMESERVER_CRAFT_CLICK, 			136).
-define(GAMESERVER_GAIN_EXP, 				137).
-define(GAMESERVER_FLY_TEXT, 				138).
-define(GAMESERVER_BUFF_ADD, 				139).
-define(GAMESERVER_BUFF_DELETE, 			140).
-define(GAMESERVER_TARGET, 					141).
-define(GAMESERVER_TARGET_RESET, 			142). % C
-define(GAMESERVER_REUSE_TIME,              143).
-define(GAMESERVER_SET_PARAM, 				144).
-define(GAMESERVER_CLIENT_SPEED, 			145).
-define(GAMESERVER_SET_SPEED, 				146). % C
-define(GAMESERVER_SET_PLAYER_PARAM,		147). % параметры только моего игрока
-define(GAMESERVER_OBJ_TYPE, 				148). % тип объекта
-define(GAMESERVER_BUG_REPORT, 				149). % C
-define(GAMESERVER_KNOWLEDGE, 				150). % 
-define(GAMESERVER_KNOWLEDGE_INC,			151). % C
-define(GAMESERVER_KNOWLEDGE_DEC,			152). % C
-define(GAMESERVER_SKILL_BUY,				153). % C
-define(GAMESERVER_DIALOG_OPEN,				154). % C
-define(GAMESERVER_DIALOG_CLOSE,			155). % C
-define(GAMESERVER_OBJECT_CLOSE,			156). % S
-define(GAMESERVER_OBJECT_VISUAL_STATE,		157). % S
-define(GAMESERVER_OBJECT_VISUAL_STATE_ACK, 158). % C
-define(GAMESERVER_CLAIM_REMOVE,			159). % S
-define(GAMESERVER_CLAIM_CHANGE,			160). % S
-define(GAMESERVER_INVENTORY,				162). % S
-define(GAMESERVER_INVENTORY_CLICK,			163). % C
-define(GAMESERVER_SET_HAND,				164). % S
-define(GAMESERVER_EQUIP,					165). % S
-define(GAMESERVER_EQUIP_CLICK,				166). % C


 

send_packet(SendPid, Type, Data) when is_binary(Data) ->
	SendPid ! {send, [write_word(size(Data)+?HEADER_LEN), write_byte(Type), Data]};

send_packet(SendPid, Type, Data) when is_list(Data) ->
    send_packet(SendPid, Type, list_to_binary(Data)).

send_raw_packet(Socket, Type, Data) when is_binary(Data) ->
	gen_tcp:send(Socket, [write_word(size(Data)+?HEADER_LEN), write_byte(Type), Data]).
