-module(migration_31).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").


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
    time = 60000
    }).

migrate() ->
    Fun = fun(OldRoute) -> list_to_tuple(tuple_to_list(OldRoute) ++ [200,60000]) end,
    {atomic,ok} = mnesia:transform_table(route, Fun, record_info(fields, route), route),
    {atomic,ok} = dbUuid:fix(route),
    {migrated, 31}.
