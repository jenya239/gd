-module(migration_67).

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
        mnesia:write(#global{key=restartGameCost, value=1})
    end),
    
    {migrated, 67}.