-module(migration_57).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(global,
    {
        key,
        value
    }
).

migrate() ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=fuelCost1, value=13}),
        mnesia:write(#global{key=fuelCost2, value=13}),
        mnesia:delete({global,fuelCost})
    end),
    {migrated, 57}.