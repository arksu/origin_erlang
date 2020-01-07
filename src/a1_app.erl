%% Author: arksu
%% Created: 31.01.2011
%% Description: TODO: Add description to a1
-module(a1_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-include("defines.hrl").

start() -> 
	application:start(a1).
	%start(none, none).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
	Cfg = config:get_config(), 
    log_level:set(config:get_value(Cfg, log_level)),
    LogPath = get_log_path(),
    error_logger:add_report_handler(a1_logger_h, LogPath),
    ?DEBUG("start ~p",[a1_app]),
    Sup = a1_sup:start_link(),
    Sup.
 
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% @spec () -> string()
%% @doc Returns the full path to the ejabberd log file.
%% It first checks for application configuration parameter 'log_path'.
%% If not defined it checks the environment variable A1_LOG_PATH.
%% And if that one is neither defined, returns the default value:
%% "ejabberd.log" in current directory.
get_log_path() ->
    case application:get_env(log_path) of
    {ok, Path} ->
        Path;
    undefined ->
        case os:getenv("A1_LOG_PATH") of
        false ->
            ?LOG_PATH;
        Path ->
            Path
        end
    end.