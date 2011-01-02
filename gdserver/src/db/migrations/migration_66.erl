-module(migration_66).

-include_lib("stdlib/include/qlc.hrl").

-export([migrate/0]).

-record(
    global,
    {
        key,
        value
    }
).

-record(
    routeOld,
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

-record(
    route,
    {
        id,
        displayName,
        fileName,
        length,
        minLevel = 1,
        maxLevel = 100,
        moneyPrize = 5000,
        isHomeCity = true,
        isBattleCity = true,
        difficulty = 1
    }
).

migrate() ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=minAvgSpeed, value=70}),
        mnesia:write(#global{key=maxAvgSpeed, value=180}),
        mnesia:write(#global{key=expMin, value=50}),
        mnesia:write(#global{key=expMax, value=900})
    end),
    Fun = fun(OldRecord) ->
            RouteOld = utils:changeRecordName(OldRecord, routeOld),
            #route{
                id = RouteOld#routeOld.id,
                displayName = RouteOld#routeOld.displayName,
                fileName = RouteOld#routeOld.fileName,
                length = RouteOld#routeOld.length,
                minLevel = RouteOld#routeOld.minLevel,
                maxLevel = RouteOld#routeOld.maxLevel,
                moneyPrize = RouteOld#routeOld.moneyPrize,
                isHomeCity = RouteOld#routeOld.isHomeCity,
                isBattleCity = RouteOld#routeOld.isBattleCity
            }
    end,
    {atomic, ok} = mnesia:transform_table(route, Fun, record_info(fields, route), route),
    {migrated, 66}.




