-module(migration_60).
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
        mnesia:write(#global{key=transferPriceTrain, value=500}),
        mnesia:write(#global{key=transferPricePlane, value=1}),        
        mnesia:write(#global{key=transferTimeTrain, value=20*60}),
        mnesia:write(#global{key=transferTimePlane, value=10}),
        mnesia:write(#global{key=transferMinLevel, value=5})
    end),
            
    {migrated, 60}.
    