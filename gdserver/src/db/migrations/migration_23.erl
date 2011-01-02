-module(migration_23).
-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    user,
    {
    id,
    login,
    password,
    salt,
    email,
    displayName,
    image = "",
    currentCarID = 1,
    defaultColor = 1,
    level = 1,
    experience = 0,
    roles = [],
    triggers = [],
    date = erlang:now(),
    vkontakteID = 0,
    nitroCount = 5,
    rating = 0,
    city = undefined,
    money = 0,
    realMoney = 0,
    fuelVolume = 40
    }).

-record(
    car,
    {
    id,
    parentID,
    displayName,
    fileName,
    maxSpeed,
    acceleration,
    steering,
    minLevel,
    fuelConsumption = 0,
    fuelCapacity = 40
    }).

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
    moneyPrize = 5000
    }).

migrate() ->
    %% add one integer "money" field with value = 0
    FunUser = fun(UserOld) -> list_to_tuple(tuple_to_list(UserOld) ++ [0,40]) end,
    FunCar = fun(CarOld) -> list_to_tuple(tuple_to_list(CarOld) ++ [0,40]) end,
    FunRoute = fun(RouteOld) -> list_to_tuple(tuple_to_list(RouteOld) ++ [100,10,undefined,5000]) end,
    {atomic,ok} = mnesia:transform_table(user, FunUser, record_info(fields, user), user),
    {atomic,ok} = mnesia:transform_table(car, FunCar, record_info(fields, car), car),
    {atomic,ok} = mnesia:transform_table(route, FunRoute, record_info(fields, route), route),

    {migrated, 23}.