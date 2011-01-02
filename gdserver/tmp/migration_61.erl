-module(migration_61).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    route,
    {
        id,
        displayName,
        fileName,
        length,
        minLevel = 1,
        maxLevel = 100,
        maxPlayerCount = 10,
        city = undefined,
        moneyPrize = 5000,
        exp = 200,
        time = 60000,
        isHomeCity = true,
        isBattleCity = true
    }
).

migrate() ->
    {atomic, ok} = mnesia:transform_table(route, fun(OldRecord) ->
        list_to_tuple(tuple_to_list(OldRecord) ++ [true, true])
    end, record_info(fields, route), route),
            
    {migrated, 61}.
    