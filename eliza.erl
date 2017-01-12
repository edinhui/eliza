-module(eliza).


import_script(FullPath) ->
    case file:open(FullPath,[read]) of
        {ok, IODev} ->
