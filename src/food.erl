%% Author: arksu
%% Created: 31.01.2012
%% Description: TODO: Add description to food
-module(food).

-include("types.hrl").


-export([
		 is_food/1, 
		 get_fep/1,
		 get_result/1
		]).


% ##################################################################################
% # DEFINES #
% ##################################################################################
	

%% --------------------------------------------------------------------
% является ли едой вещь (пкм - съесть)
is_food(Type) ->
	case Type of
		carrot -> true;
		apple -> true;
		_ -> false
	end.

%% --------------------------------------------------------------------
% получить добавление фепа для еды
get_fep(#item{type=Type, q=Q}) ->
	case Type of
		carrot -> #player_stats{perc=Q};
		apple -> #player_stats{dex=Q};
		_ -> #player_stats{}
	end.

%% --------------------------------------------------------------------
% во что превращается еда когда ее съешь
get_result(#item{type=Type}) ->
	case Type of
		apple -> apple_stub; % яблоко в огрызок
		_ -> false % ни во что не превращается. уничтожается.
	end.