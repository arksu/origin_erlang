
%----------------------------------------------------------------------------------------------------
% стейт для объектов растений
-record(obj_plant, {
				seed = none, % семечко из которого посадили, качество определится из семечка
				stage = 0, % номер стадии роста !!!! не менять положение. завязан МОДУЛЬ НА С
				count = 3 % сколько единиц растет
				   }).

%----------------------------------------------------------------------------------------------------
% стейт для объектов ворот
-record(obj_gate, {
				opened = false
				   }).

%----------------------------------------------------------------------------------------------------
% стейт для стула
-record(obj_chair, {  
				direction = 7 % направление в которое направлен
				   }).
%----------------------------------------------------------------------------------------------------
% стейт для клайма
-record(obj_claim, {  
				owner_id = 0 % кто построил
				   }).
%----------------------------------------------------------------------------------------------------
% стейт для костра
-record(obj_bonfire, {  
				inv = [], % список вещей которые над / в костре (инвентарь)
				fuel = [], % топливо, то что лежит внизу (инвентарь)
				fire = false % горит или нет
				   }).
