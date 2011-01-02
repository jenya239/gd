-module(migration_52).
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
        mnesia:write(#global{key=tutorialReward, value=2200})
    end),
    {migrated, 52}.