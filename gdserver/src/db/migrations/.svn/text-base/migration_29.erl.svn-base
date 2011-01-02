-module(migration_29).

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
                mnesia:write(#global{key=vkontakteExchangeRate, value=10})
            end),
    {migrated, 29}.
