-module(eliza).
-export([import_script/1]).


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
    case file:read_file(File) of
        {ok, Bin} ->
            StrList = binary_to_list(Bin),
            %io:format(StrList),
            case eliza_leex:string(StrList) of
                {ok, Tokens,_} ->
                    {ok, Tokens};
                Error ->
                    Error

            end;
        ErrorFile ->
            io:format("err when read file ~p~n",[ErrorFile]),
            ErrorFile
    end.

get_parsed(Tokens) ->
    case eliza_yecc:parse(Tokens) of
        {ok, Res} -> 
            {ok, Res};
        Err ->
            io:format("err ~p when parse tokens~n", [Err]),
            Err
    end.

    

                              



                
