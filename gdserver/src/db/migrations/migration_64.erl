-module(migration_64).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    global,
    {        
        key,
        value
    }
).

migrate() ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=blueScoreExtraPercent, value=0})        
    end),
            
    {migrated, 64}.