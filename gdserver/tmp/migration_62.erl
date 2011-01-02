-module(migration_62).
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
        mnesia:write(#global{key=fuelCost3, value=13})        
    end),
            
    {migrated, 62}.
    