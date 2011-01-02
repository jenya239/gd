-module(migration_68).

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
        mnesia:write(#global{key=raceNewbieID, value=16})
    end),
    
    {migrated, 68}.