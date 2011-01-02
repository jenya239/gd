-module(migration_33).

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
                mnesia:write(#global{key=upgradeMaxValue, value=1000}),
                mnesia:write(#global{key=upgradeMaxTimes, value=4})
            end),
    {migrated, 33}.
