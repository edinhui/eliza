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



%-export([import_script/1, 
%         import_script_verbose/1, 
%         get_line_tokens/1,
%         compile_and_run/0]).

-compile(export_all).

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

classify_script([#script{value = #initial{} = Init} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Init, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs);

classify_script([#script{value = #final{} = Fin} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Fin, QuitLs, PreLs, PostLs, SynonLs, KeyLs);
                                  
classify_script([#script{value = #quit{} = Qt} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, [Qt | QuitLs], PreLs, PostLs, SynonLs, KeyLs);

classify_script([#script{value = #pre{} = Pre} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, [Pre | PreLs], PostLs, SynonLs, KeyLs);

classify_script([#script{value = #post{} = Post} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, [Post | PostLs], SynonLs, KeyLs);

classify_script([#script{value = #synon{} = Synon} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, PostLs, [Synon | SynonLs], KeyLs);

classify_script([#script{value = #key{} = Key} = Script | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, PostLs, SynonLs, [Key | KeyLs]).

convert_decomp_pattern_to_perl_style(#decomp{pattern = Pattern} = Decomp, SynonLs) ->
    PatternStar = lists:map(fun(Word) -> lists:foldl(fun replace_star/2, [], Word) end,
                            Pattern),
    PatternSynon = lists:map(fun(Word) -> replace_synon(Word, SynonLs) end, PatternStar),
    Decomp#decomp{pattern = PatternSynon}.
 
replace_star($*, Acc) ->
    Acc++"(.*)";
replace_star(Other, Acc) ->
    Acc ++ [Other].


replace_synon([], SynonLs) ->
    [];
replace_synon([$@],SynonLs) ->
    [$@]; 
replace_synon([$@|Rest] = Old, SynonLs) ->
    case lists:keysearch(Rest, 2, SynonLs) of
         false -> Old;
         {value, #synon{sample = Rest, synon_list = Synons}} ->
             "(" ++ string:join([Rest|Synons],"|") ++ ")"
    end; 
replace_synon(Other, SynonLs) ->
    Other.

extract_and_init_script(ScriptLs) ->
    {Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs} = 
        classify_script(ScriptLs),
    {Initial, Final, QuitLs, PreLs, PostLs, SynonLs,
     lists:map(fun(Key) -> pre_process_key(Key, SynonLs) end, KeyLs)}.

pre_process_key(#key{decomp_list = DecompLs} = Key, SynonLs) ->
    DecompLs1 = lists:map(fun(Decomp) -> convert_decomp_pattern_to_perl_style(Decomp, SynonLs) end,
                          DecompLs),
    DecompLs2 = lists:map(fun(Old) -> Old#decomp{reasmb_index = 1} end, DecompLs1),
    Key#key{decomp_list = DecompLs1}.
    
init_eliza(FullPath) ->
    case import_script(FullPath) of
        {ok, ScriptLs} -> extract_and_init_script(ScriptLs);
        Err -> Err
    end.   
