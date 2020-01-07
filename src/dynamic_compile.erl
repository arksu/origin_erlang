%%%-------------------------------------------------------------------
%%% File : dynamic_compile.erl
%%% Description :
%%% Authors : Mats Cronqvist <mats.cronqvist@ericsson.com>
%%%           Chris Newcombe <chris.newcombe@gmail.com> 
%%%           Jacob Vorreuter <jacob.vorreuter@gmail.com>

%%%-------------------------------------------------------------------
-module(dynamic_compile).

-export([from_string/1, from_string/2]).

-import(lists, [reverse/1, keyreplace/4]).

from_string(CodeStr) ->
    from_string(CodeStr, []).

from_string(CodeStr, CompileFormsOptions) ->
    Filename = "compiled_from_string",
    Ms0    = dict:new(), 
    InitMD = Ms0,
    IncludeSearchPath = ["." | reverse([Dir || {i, Dir} <- CompileFormsOptions])],
    {RevForms, _OutMacroDict} = scan_and_parse(CodeStr, Filename, 1, [], InitMD, IncludeSearchPath),
    Forms = reverse(RevForms),
    case compile:forms(Forms, CompileFormsOptions) of
        {ok, ModuleName, CompiledCodeBinary} when is_binary(CompiledCodeBinary) ->
            {ModuleName, CompiledCodeBinary};
        {ok, ModuleName, CompiledCodeBinary, []} when is_binary(CompiledCodeBinary) ->  % empty warnings list
            {ModuleName, CompiledCodeBinary};
        {ok, _ModuleName, _CompiledCodeBinary, Warnings} ->
            throw({?MODULE, warnings, Warnings});
        Other ->
            throw({?MODULE, compile_forms, Other})
    end.

scan_and_parse([], _CurrFilename, _CurrLine, RevForms, MacroDict, _IncludeSearchPath) ->
    {RevForms, MacroDict};
 
scan_and_parse(RemainingText, CurrFilename, CurrLine, RevForms, MacroDict, IncludeSearchPath) ->
    case scanner(RemainingText, CurrLine, MacroDict) of
	    {tokens, NLine, NRemainingText, Toks} ->
	        {ok, Form} = erl_parse:parse_form(Toks),
	        scan_and_parse(NRemainingText, CurrFilename, NLine, [Form | RevForms], MacroDict, IncludeSearchPath);
	    {macro, NLine, NRemainingText, NMacroDict} ->
	        scan_and_parse(NRemainingText, CurrFilename, NLine, RevForms,NMacroDict, IncludeSearchPath);
        {include, NLine, NRemainingText, IncludeFilename} ->
            IncludeFileRemainingTextents = read_include_file(IncludeFilename, IncludeSearchPath),
            IncludeMacroDict = MacroDict,

            {RevIncludeForms, IncludedMacroDict} = scan_and_parse(IncludeFileRemainingTextents, IncludeFilename, 1, [], IncludeMacroDict, IncludeSearchPath),
            NMacroDict = IncludedMacroDict,
	        scan_and_parse(NRemainingText, CurrFilename, NLine, RevIncludeForms ++ RevForms, NMacroDict, IncludeSearchPath);
        done ->
	        scan_and_parse([], CurrFilename, CurrLine, RevForms, MacroDict, IncludeSearchPath)
    end.

scanner(Text, Line, MacroDict) ->
    case erl_scan:tokens([],Text,Line) of
        {done, {ok,Toks,NLine}, LeftOverChars} ->
            case pre_proc(Toks, MacroDict) of
                {tokens,  NToks}      -> {tokens,  NLine, LeftOverChars, NToks};
                {macro,   NMacroDict} -> {macro,   NLine, LeftOverChars, NMacroDict};
                {include, Filename}   -> {include, NLine, LeftOverChars, Filename}
            end;
        {more, _Continuation} ->
            case is_only_comments(Text) of
                true  ->
                    done;
                false ->
                    throw({incomplete_term, Text, Line})
            end
    end.

is_only_comments(Text) -> is_only_comments(Text, not_in_comment).

is_only_comments([],       _)              -> true;
is_only_comments([$   |T], not_in_comment) -> is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\t |T], not_in_comment) -> is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\n |T], not_in_comment) -> is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$%  |T], not_in_comment) -> is_only_comments(T, in_comment);     % found start of a comment
is_only_comments(_,        not_in_comment) -> false;
is_only_comments([$\n |T], in_comment)     -> is_only_comments(T, not_in_comment); % found end of a comment
is_only_comments([_   |T], in_comment)     -> is_only_comments(T, in_comment).     % skipping over in-comment chars


pre_proc([{'-',_},{atom,_,define},{'(',_},{_,_,Name}|DefToks],MacroDict) ->
    false = dict:is_key(Name, MacroDict),
    case DefToks of
    	[{',',_} | Macro] ->
    	    {macro, dict:store(Name, {[], macro_body_def(Macro, [])},  MacroDict)};
    	[{'(',_} | Macro] ->
    	    {macro, dict:store(Name, macro_params_body_def(Macro, []), MacroDict)}
    end;

pre_proc([{'-',_}, {atom,_,include}, {'(',_}, {string,_,Filename}, {')',_}, {dot,_}], _MacroDict) ->
    {include, Filename};

pre_proc(Toks,MacroDict) ->
    {tokens, subst_macros(Toks, MacroDict)}.

macro_params_body_def([{')',_},{',',_} | Toks], RevParams) ->
    {reverse(RevParams), macro_body_def(Toks, [])};
macro_params_body_def([{var,_,Param} | Toks], RevParams) ->
    macro_params_body_def(Toks, [Param | RevParams]);
macro_params_body_def([{',',_}, {var,_,Param} | Toks], RevParams) ->
    macro_params_body_def(Toks, [Param | RevParams]).

macro_body_def([{')',_}, {dot,_}], RevMacroBodyToks) ->
    reverse(RevMacroBodyToks);
macro_body_def([Tok|Toks], RevMacroBodyToks) ->
    macro_body_def(Toks, [Tok | RevMacroBodyToks]).

subst_macros(Toks, MacroDict) ->
    reverse(subst_macros_rev(Toks, MacroDict, [])).

subst_macros_rev([{'?',_}, {_,LineNum,'LINE'} | Toks], MacroDict, RevOutToks) ->
    subst_macros_rev(Toks, MacroDict, [{integer,LineNum,LineNum}] ++ RevOutToks);

subst_macros_rev([{'?',_}, {_,_,Name}, {'(',_} = Paren | Toks], MacroDict, RevOutToks) ->
    case dict:fetch(Name, MacroDict) of
        {[], MacroValue} ->
            RevExpandedOtherMacrosToks = subst_macros_rev(MacroValue, MacroDict, []),
            subst_macros_rev([Paren|Toks], MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks);
        ParamsAndBody ->
            {NToks, Arguments} = subst_macros_get_args(Toks, []),
            ExpandedParamsToks = subst_macros_subst_args_for_vars(ParamsAndBody, Arguments),
            RevExpandedOtherMacrosToks = subst_macros_rev(ExpandedParamsToks, MacroDict, []),
            subst_macros_rev(NToks, MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks)
    end;

subst_macros_rev([{'?',_}, {_,_,Name} | Toks], MacroDict, RevOutToks) ->
    {[], MacroValue} = dict:fetch(Name, MacroDict),

    RevExpandedOtherMacrosToks = subst_macros_rev(MacroValue, MacroDict, []),
    subst_macros_rev(Toks, MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks);

subst_macros_rev([Tok|Toks], MacroDict,  RevOutToks) ->
subst_macros_rev(Toks, MacroDict, [Tok|RevOutToks]);
subst_macros_rev([], _MacroDict, RevOutToks) -> RevOutToks.

subst_macros_get_args([{')',_} | Toks], RevArgs) ->
    {Toks, reverse(RevArgs)};
subst_macros_get_args([{',',_}, {var,_,ArgName} | Toks], RevArgs) ->
    subst_macros_get_args(Toks, [ArgName| RevArgs]);
subst_macros_get_args([{var,_,ArgName} | Toks], RevArgs) ->
    subst_macros_get_args(Toks, [ArgName | RevArgs]).

subst_macros_subst_args_for_vars({[], BodyToks}, []) ->
    BodyToks;
subst_macros_subst_args_for_vars({[Param | Params], BodyToks}, [Arg|Args]) ->
    NBodyToks = keyreplace(Param, 3, BodyToks, {var,1,Arg}),
    subst_macros_subst_args_for_vars({Params, NBodyToks}, Args).

read_include_file(Filename, IncludeSearchPath) ->
    case file:path_open(IncludeSearchPath, Filename, [read, raw, binary]) of
        {ok, IoDevice, FullName} ->
            {ok, Data} = file:read(IoDevice, filelib:file_size(FullName)),
            file:close(IoDevice),
            binary_to_list(Data);
        {error, Reason} ->
            throw({failed_to_read_include_file, Reason, Filename, IncludeSearchPath})
    end.