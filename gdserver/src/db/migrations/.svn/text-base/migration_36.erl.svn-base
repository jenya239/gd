-module(migration_36).
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
                mnesia:write(#global{key=nitroImpulse, value=10}),
                mnesia:write(#global{key=nitroTime, value=20000})
            end),
    {migrated, 36}.
