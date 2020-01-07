%% Author: arksu
%% Created: 31.03.2011
%% Description: TODO: Add description to config
-module(config).

-export([get_config/0, get_value/2]).

-define(CONFIG_PATH, "a1.cfg").

%% If a1 isn't yet running in this node, then halt the node
exit_or_halt(ExitText) ->
    case [Vsn || {a1, _Desc, Vsn} <- application:which_applications()] of
    [] ->
        timer:sleep(1000),
        halt(ExitText);
    [_] ->
        exit(ExitText)
    end.

%% return terms in config file
get_config() ->
    load_file(get_a1_config_path()).

%% get value from config
get_value([], _OptName) -> 
    none;
get_value(Terms, OptName) ->
    [{OName, Val}|T] = Terms,
    case OName of 
        OptName -> Val;
        _ -> get_value(T, OptName)
    end.
    
%% @doc Get the filename of the configuration file.
%% The filename can be specified with: erl -config "/path/to/a1.cfg".
%% It can also be specified with the environtment variable A1_CONFIG_PATH.
%% If not specified, the default value 'a1.cfg' is assumed.
%% @spec () -> string()
get_a1_config_path() ->
    case application:get_env(config) of
    {ok, Path} -> Path;
    undefined ->
        case os:getenv("A1_CONFIG_PATH") of
        false ->
            ?CONFIG_PATH;
        Path ->
            Path
        end
    end.

%% @doc Load the a1 configuration file.
%% It also includes additional configuration files and replaces macros.
%% This function will crash if finds some error in the configuration file.
%% @spec (File::string()) -> ok
load_file(File) ->
    get_plain_terms_file(File). 

%% @doc Read an a1 configuration file and return the terms.
%% Input is an absolute or relative path to an ejabberd config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
%% @spec(string()) -> [term()]
get_plain_terms_file(File1) ->
    File = get_absolute_path(File1),
    case file:consult(File) of
    {ok, Terms} ->
        Terms;
    {error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
        ExitText = {config_problem, File, Reason, LineNumber},
        exit_or_halt(ExitText);
    {error, Reason} ->
        ExitText = {config_problem, File, Reason},
        exit_or_halt(ExitText)
    end.

%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an a1 configuration file.
%% And returns an absolute path to the configuration file.
%% @spec (string()) -> string()
get_absolute_path(File) ->
    case filename:pathtype(File) of
    absolute ->
        File;
    relative ->
        Config_path = get_a1_config_path(),
        Config_dir = filename:dirname(Config_path),
        filename:absname_join(Config_dir, File)
    end.

