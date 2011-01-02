-module(migration_27).
-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

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

migrate() ->
    FunCar = fun(CarOld) -> CarOld#car{fuelConsumption = 1} end,
    {atomic,ok} = mnesia:transform_table(car, FunCar, record_info(fields, car), car),
    {migrated, 27}.
