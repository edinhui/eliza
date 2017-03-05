-module(eliza).

-record(script,{value}).
-record(initial, {init_word_list}).
-record(final, {final_word_list}).
-record(quit, {quit_word_list}).
-record(pre, {orig_word, replace_word_list}).
-record(post, {orig_word, replace_word_list}).
-record(synon, {sample, synon_list}).
-record(key, {keyword, priority, decomp_list}).
-record(decomp, {pattern, reasmb_index, reasmb_list}).
-record(reasmb, {rule, value}).



-export([import_script/1, 
         import_script_verbose/1, 
         get_line_tokens/1,
         compile_and_run/0]).


import_script(FullPath) ->
    case get_tokens(FullPath) of
        {ok, Tokens} ->   
            io:format("tokens:~p~n",[Tokens]),
            get_parsed(Tokens);
        ErrToken ->
            io:format("err:~p when get tokens~n",[ErrToken]),
            ErrToken
    end.


get_tokens(File) ->
    case file:open(File,[read]) of
        {ok, IO} ->
            token_loop(IO,[]);
        ErrorFile ->
            io:format("err when read file ~p~n",[ErrorFile]),
            ErrorFile
    end.

token_loop(IO, Acc) -> 
    case file:read_line(IO) of
        {ok, Line} -> 
            io:format("~p~n",[Line]),
            case eliza_leex:string(Line) of
                {ok, Tokens, _} -> 
                    token_loop(IO, Acc ++ Tokens);
                Err -> 
                    io:format("err:~p when get tokens on Line ~p~n",[Err,Line]),
                    Err
            end;
         eof -> 
             {ok, Acc};
         ErrFile ->
             io:format("file read line error ~p~n",[ErrFile]),
             ErrFile
     end.
         

get_parsed(Tokens) ->
    case eliza_yecc:parse(Tokens) of
        {ok, Res} -> 
            {ok, Res};
        Err ->
            io:format("err ~p when parse tokens~n", [Err]),
            Err
    end.

    
import_script_verbose(FilePath) -> 
    case file:open(FilePath,[read]) of
        {ok, IO} ->
            eliza_yecc:parse_and_scan({?MODULE,get_line_tokens,[IO]});
        Err ->
            io:format("open file err ~p~n",[Err])
    end.

get_line_tokens(IO) ->
    case file:read_line(IO) of
        {ok, Line} -> 
            io:format("~p~n",[Line]),
            Res = eliza_leex:string(Line),
            io:format("~p~n",[Res]),
            Res;
        eof -> 
            {eof, 1};
        Err -> 
            {error, Err, 1}
    end.
                     
compile_and_run() ->
    leex:file("./eliza_leex.xrl"),
    yecc:file("./eliza_yecc.yrl"),
    c:c(eliza_leex),
    c:c(eliza_yecc),
    c:c(eliza),
    eliza:import_script_verbose("./script_test").
         
classify_script(ScriptLs) ->
    classify_script(ScriptLs, undefined,undefined,[],[],[],[],[]).

classify_script([], Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    {Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs};

classify_script([#script{value = #initial{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Script, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs);

classify_script([#script{value = #final{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Script, QuitLs, PreLs, PostLs, SynonLs, KeyLs);
                                  
classify_script([#script{value = #quit{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, [Script | QuitLs], PreLs, PostLs, SynonLs, KeyLs);

classify_script([#script{value = #pre{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, [Script | PreLs], PostLs, SynonLs, KeyLs);

classify_script([#script{value = #post{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, [Script | PostLs], SynonLs, KeyLs);

classify_script([#script{value = #synon{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, PostLs, [Script | SynonLs], KeyLs);

classify_script([#script{value = #key{}} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, PostLs, SynonLs, [Script | KeyLs]).


