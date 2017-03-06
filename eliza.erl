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
-define(OPTIONS, [{capture, all_but_first, list}]).
-define(SEP, " ,\n").
-define(ELIZA_PROMPT, " ELIZA : ~s~n").
-define(MY_PROMPT,    "   ME  : ").
%-export([import_script/1, 
%         import_script_verbose/1, 
%         get_line_tokens/1,
%         compile_and_run/0]).

-compile(export_all).
 
%% ============================================================================
%% eliza interact talking function. 
%% 
%% 1. the sentence broken down into words, separated by spaces.  All further
%%    processing takes place on these words as a whole, not on the individual
%%    characters in them.
%% 2. a set of pre-substitutions takes place.
%% 3. Eliza takes all the words in the sentence and makes a list of all
%%    keywords it finds.  It sorts this keyword list in descending weight.  It
%%    process these keywords until it produces an output.
%% 4. for the given keyword, a list of decomposition patterns is searched.
%%    The first one that matches is selected.  
%%    If no match is found, the next keyword is selected instead.
%% 5. for the matching decomposition pattern, a reassembly pattern is selected.  
%%    There may be several reassembly patterns, but only one is used for a given 
%%    sentence.  If a subsequent sentence selects the same decomposition pattern 
%%    the next reassembly pattern in sequence is used, until they have all been 
%%    used, at which point Eliza starts over with the first reassembly pattern.
%% 6. post-substitutions takes place for variable in reassembly pattern
%% 7. the resulting sentence is displayed as output
%% ============================================================================
talking() ->
    {Initial, Final, QuitLs, PreLs, PostLs, _SynonLs, KeyLs} = 
        init_eliza("./script"),
    write_words(Initial#initial.init_word_list), 
    talking_loop(Final, QuitLs, PreLs, PostLs, KeyLs).

%% input and output functions
write_words(WordLs) ->
    io:format(?ELIZA_PROMPT,[string:join(WordLs, " ")]).

read() ->
    io:get_line(?MY_PROMPT).

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
    case loop_decomp_list(DecompLs, WordLs, [], PostLs, KeyLs) of 
        {match, NewWords, NewDecompLs} ->
            {NewWords, lists:keyreplace(
                                 Keyword, 2, KeyLs, 
                                 Key#key{decomp_list = NewDecompLs})};
        {goto, GotoKeyword} ->
            loop_keywords([{GotoKeyword, 0}], WordLs, KeyLs, PostLs);
        nomatch ->
            loop_keywords(Rest, WordLs, KeyLs, PostLs) 
    end.

loop_decomp_list([], _Words, _NewDecompLs, _PostLs, _KeyLs) ->
    nomatch;
loop_decomp_list([#decomp{pattern = PatternWordLs, 
                          reasmb_index = Idx,
                          reasmb_list = ReasmbLs} =  Decomp| Rest], 
                 Words, NewDecompLs, PostLs, KeyLs) ->
    case re:run(string:join(Words," "), 
                string:join(PatternWordLs," "),
                ?OPTIONS) of
        nomatch ->
            loop_decomp_list(Rest, Words, 
                             NewDecompLs ++ [Decomp], PostLs, KeyLs);
        {match, Captured} ->
            #reasmb{rule = Rule, value = WordLsOrKeyword} = lists:nth(Idx, ReasmbLs),
            case Rule of
                 replace -> 
                     %io:format("~p,~p,~p,~p~n",[Words, PatternWordLs, Captured,WordLsOrKeyword]),
                     NewWordLs = replace_var(WordLsOrKeyword, Captured, PostLs),
                     NewIdx = case (Idx+1) > length(ReasmbLs) of
                                  true -> 1;
                                  false -> Idx + 1
                              end,
                     {match, 
                      NewWordLs, 
                      NewDecompLs ++ 
                          [Decomp#decomp{reasmb_index = NewIdx}] ++ 
                          Rest};
                 goto -> 
                     {goto, WordLsOrKeyword}
             end
    end.
        
replace_var(WordLs, Captured, PostLs) ->
    replace_var_loop(WordLs, Captured, PostLs, []).

replace_var_loop([], _Captured, _PostLs, NewWordLs) ->
    NewWordLs;        
replace_var_loop([Word|Rest], Captured, PostLs, NewWordLs) ->
    NewWordLs1 = 
        case re:run(Word, ?VAR, ?OPTIONS) of
            {match,[CapIdxStr]} -> 
                {CapIdx,_} = string:to_integer(CapIdxStr),
                MatchStr = lists:nth(CapIdx, Captured),
                MatchWordLs = string:tokens(MatchStr, ?SEP),
                PostSub = substitution(MatchWordLs, PostLs),
                NewWordLs ++ PostSub;
            nomatch -> 
                NewWordLs ++ [Word]
        end, 
    replace_var_loop(Rest, Captured, PostLs, NewWordLs1).
 

%% ============================================================================
%% functions to parse the script files
%% ============================================================================
import_script(FullPath) ->
    case get_tokens(FullPath) of
        {ok, Tokens} ->   
            %io:format("tokens:~p~n",[Tokens]),
            get_parsed(Tokens);
        ErrToken ->
            %io:format("err:~p when get tokens~n",[ErrToken]),
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
            %io:format("~p~n",[Line]),
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
         
%% ============================================================================

%% ============================================================================
%% eliza runtime initialization
%% ============================================================================
init_eliza(FullPath) ->
    case leex:file("./eliza_leex.xrl") of
        error -> io:format("lex file compile error, system halt!~n"),
                 halt();
        {error, Errs, Warnings} ->
            io:format("lex file compile met err: ~p~n, warning : ~p~n",
                      [Errs, Warnings]);
        _ -> ok
    end,

    case yecc:file("./eliza_yecc.yrl") of
        error -> io:format("yecc file compile error, system halt!~n"),
                 halt();
        {error, YeccErrs, YeccWarnings} ->
            io:format("yecc file compile met err: ~p~n, warning : ~p~n",
                      [YeccErrs, YeccWarnings]);
        _ -> ok
    end,
    
    lists:map(fun(Mod) -> 
                  case c:c(Mod) of
                      error -> io:format("compile and load ~p error!~n",
                                         [Mod]),
                               halt();
                      _ -> ok
                  end
              end,
              [eliza_leex, eliza_yecc, eliza]),

    case import_script(FullPath) of
        {ok, ScriptLs} -> extract_and_init_script(ScriptLs);
        Err -> Err
    end.   

extract_and_init_script(ScriptLs) ->
    {Initial, Final, QuitLs, PreLs, PostLs, SynonLs, KeyLs} = 
        classify_script(ScriptLs),
    {Initial, Final, QuitLs, PreLs, PostLs, SynonLs,
     lists:map(fun(Key) -> pre_process_key(Key, SynonLs) end, KeyLs)}.

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


pre_process_key(#key{decomp_list = DecompLs} = Key, SynonLs) ->
    DecompLs1 = lists:map(fun(Decomp) -> convert_decomp_pattern_to_perl_style(Decomp, SynonLs) end,
                          DecompLs),
    DecompLs2 = lists:map(fun(Old) -> Old#decomp{reasmb_index = 1} end, DecompLs1),
    Key#key{decomp_list = DecompLs2}.

%% ============================================================================
%% convert regrex in script to regrex style supported by re module
%% * -> (.*), @name -> (name1|name2|..|namex)
%% ============================================================================
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


