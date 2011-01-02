-module(migration_25).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(global,
    {
        key,
        value
    }
).

migrate() ->
        {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#global{key=fuelCost, value=13})
            end),
    {migrated, 25}.
