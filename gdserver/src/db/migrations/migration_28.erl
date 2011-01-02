-module(migration_28).

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
                mnesia:write(#global{key=repairRate, value=1}),
                mnesia:write(#global{key=repairDiscount, value=0})
            end),
    {migrated, 28}.
