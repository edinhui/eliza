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

-define(XNONE, "xnone").
-define(VAR, "\([0-9]\)").
-define(OPTIONS, [global,{capture, all_but_first, list}]).
-define(SEP, " ,\n").
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

classify_script([#script{value = #initial{} = Init} | Rest], 
                _Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Init, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs);

classify_script([#script{value = #final{} = Fin} | Rest], 
                Initial, _Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Fin, QuitLs, PreLs, PostLs, SynonLs, KeyLs);
                                  
classify_script([#script{value = #quit{} = Qt} | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, [Qt | QuitLs], PreLs, PostLs, SynonLs, KeyLs);

classify_script([#script{value = #pre{} = Pre} | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, [Pre | PreLs], PostLs, SynonLs, KeyLs);

classify_script([#script{value = #post{} = Post} | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, [Post | PostLs], SynonLs, KeyLs);

classify_script([#script{value = #synon{} = Synon} | Rest], 
                Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs) ->
    classify_script(Rest, Initial, Final, QuitLs, PreLs, PostLs, [Synon | SynonLs], KeyLs);

classify_script([#script{value = #key{} = Key} | Rest], 
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


replace_synon([], _SynonLs) ->
    [];
replace_synon([$@], _SynonLs) ->
    [$@]; 
replace_synon([$@|Rest] = Old, SynonLs) ->
    case lists:keysearch(Rest, 2, SynonLs) of
         false -> Old;
         {value, #synon{sample = Rest, synon_list = Synons}} ->
             "(" ++ string:join([Rest|Synons],"|") ++ ")"
    end; 
replace_synon(Other, _SynonLs) ->
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
    Key#key{decomp_list = DecompLs2}.
    
init_eliza(FullPath) ->
    case import_script(FullPath) of
        {ok, ScriptLs} -> extract_and_init_script(ScriptLs);
        Err -> Err
    end.   

write_words(WordLs) ->
    io:format("Eliza: ~s~n",[string:join(WordLs, " ")]).

read() ->
    io:get_line("  I  : ").
talking() ->
    {Initial, Final, QuitLs, PreLs, PostLs, _SynonLs, KeyLs} = 
        init_eliza("./script"),
    write_words(Initial#initial.init_word_list), 
    talking_loop(Final, QuitLs, PreLs, PostLs, KeyLs).

talking_loop(Final, QuitLs, PreLs, PostLs, KeyLs) ->
    Sentence = read(),
    Words = string:tokens(Sentence, ?SEP),
    case quit_check(Words, QuitLs) of
        true -> 
            write_words(Final#final.final_word_list),
            halt();
        false ->  
            {AnswerWords, NewKeyLs} = 
                produce_answer(Words, PreLs, PostLs, KeyLs),
            write_words(AnswerWords),
            talking_loop(Final, QuitLs, PreLs, PostLs, NewKeyLs)
    end.

quit_check(WordLs, QuitLs) ->
    QuitWordLs = lists:foldl(fun(Quit, Acc) -> 
                                 Quit#quit.quit_word_list ++  Acc
                             end,
                             [],
                             QuitLs), 
    lists:any(fun(Word) -> 
                  lists:member(Word, 
                               QuitWordLs) 
              end,
              WordLs).  

produce_answer(Words, PreLs, PostLs, KeyLs) ->
    KeywordLs = collect_sorted_keyword_list(Words, KeyLs),
    WordsAfterPre = substitution(Words, PreLs),
    %write_words(WordsAfterPre),
    {WordsAfterKeyProcess, NewKeyLs}
        = key_process(WordsAfterPre, KeywordLs, KeyLs, PostLs), 
    %{substitution(WordsAfterKeyProcess, PostLs), NewKeyLs}.
    {WordsAfterKeyProcess, NewKeyLs}.
    
substitution(WordLs, ReplaceTupleLs) ->
    lists:foldl(fun(Word, Acc) -> 
                    case lists:keysearch(Word, 2, ReplaceTupleLs) of
                         false -> Acc ++ [Word];
                         {value, Tuple} -> Acc ++ element(3, Tuple)
                    end
                end,
                [], WordLs).

collect_sorted_keyword_list(WordsAfterPre, KeyLs) ->
    KeywordLs = 
        lists:foldl(fun(Word, Acc) -> 
                        case lists:keysearch(Word, 2, KeyLs) of
                             false -> Acc;
                             {value, #key{keyword=Word,
                                             priority=Pri}} ->
                                      [{Word, Pri} | Acc] end
                     end,
                     [], WordsAfterPre),
    lists:sort(fun({_KeyA, PreA}, {_KeyB, PreB}) ->
                    PreA > PreB
               end, KeywordLs).
                        
                   
key_process(WordLs, KeywordLs, KeyLs, PostLs) ->
    case loop_keywords(KeywordLs, WordLs, KeyLs, PostLs) of
        nomatch -> loop_keywords([{?XNONE,0}], WordLs, KeyLs, PostLs);
        {Res, NewKeyLs} -> {Res, NewKeyLs} 
    end.
    
loop_keywords([], _, _, _) ->
    nomatch;
loop_keywords([{Keyword, _Pre} | Rest], WordLs, KeyLs, PostLs) ->
    {value, #key{keyword = Keyword, decomp_list = DecompLs} = Key}  =
        lists:keysearch(Keyword, 2, KeyLs),
    case loop_decomp_list(DecompLs, WordLs, [], PostLs) of 
        {NewWords, NewDecompLs} ->
            {NewWords, lists:keyreplace(
                                 Keyword, 2, KeyLs, 
                                 Key#key{decomp_list = NewDecompLs})};
        nomatch ->
            loop_keywords(Rest, WordLs, KeyLs, PostLs) 
    end.

loop_decomp_list([], _Words, _NewDecompLs, _PostLs) ->
    nomatch;
loop_decomp_list([#decomp{pattern = PatternWordLs, 
                          reasmb_index = Idx,
                          reasmb_list = ReasmbLs} =  Decomp| Rest], 
                 Words, NewDecompLs, PostLs) ->
    case re:run(string:join(Words," "), 
                string:join(PatternWordLs," "),
                ?OPTIONS) of
        nomatch ->
            loop_decomp_list(Rest, Words, NewDecompLs ++ [Decomp], PostLs);
        {match, Captured} ->
            #reasmb{rule = Rule, value = WordLsOrKeyword} = lists:nth(Idx, ReasmbLs),
            case Rule of
                 replace -> 
                     NewWordLs = replace_var(WordLsOrKeyword, Captured, PostLs),
                     NewIdx = case (Idx+1) > length(ReasmbLs) of
                                  true -> 1;
                                  false -> Idx + 1
                              end,
                     {NewWordLs, 
                      NewDecompLs ++ 
                          [Decomp#decomp{reasmb_index = NewIdx}] ++ 
                          Rest};
                 goto -> ["GOTO", "IS","NOT","IMPLEMENTED"]
             end
    end.
        
replace_var(WordLs, Captured, PostLs) ->
    replace_var_loop(WordLs, Captured, PostLs, []).

replace_var_loop([], _Captured, _PostLs, NewWordLs) ->
    NewWordLs;        
replace_var_loop([Word|Rest], Captured, PostLs, NewWordLs) ->
    NewWordLs1 = 
        case re:run(Word, ?VAR, ?OPTIONS) of
            {match,[[CapIdxStr]]} -> 
                {CapIdx,_} = string:to_integer(CapIdxStr),
                [MatchStr] = lists:nth(CapIdx, Captured),
                MatchWordLs = string:tokens(MatchStr, ?SEP),
                PostSub = substitution(MatchWordLs, PostLs),
                NewWordLs ++ PostSub;
            nomatch -> 
                NewWordLs ++ [Word]
        end, 
    replace_var_loop(Rest, Captured, PostLs, NewWordLs1).
 


