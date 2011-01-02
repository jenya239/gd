-module(migration_35).
-include_lib("stdlib/include/qlc.hrl").
-define(MIGRATION_NUMBER, 35).
-export([migrate/0]).

-include("lib/eunit/include/eunit.hrl").

% -record(
%     car,
%     {
%     id,
%     parentID,
%     displayName,
%     fileName,
%     maxSpeed,
%     acceleration,
%     steering,
%     minLevel,
%     fuelConsumption = 0,
%     fuelCapacity = 40
%     }).

% -record(
%   itemClass,
%   {
%       id,
%       name,
%       category,
%       description = "",
%       price = 0,
%       targetCars,
%       usingType = slot, %slot или cast
%       slot,
%       usingCount = 0,
%       minLevel = 1,
%       maxLevel = 0,
%       durability = 100,
%       durabilityCoeff = 1,
%       power = 0,
%       speed = 0,
%       braking = 0,
%       controllability = 0,
%       targetType = self, %self, other, world
%     realPrice = 0
%   }).

% -record(
%     route,
%     {
%     id,
%     displayName,
%     fileName,
%     length,
%     minLevel = 1,
%     maxLevel = 100,
%     maxPlayerCount = 10,
%     city = undefined,
%     moneyPrize = 5000,
%     exp = 200,
%     time = 60000
%     }).

get(What, Terms) ->
	{value, Res0} = lists:keysearch( What, 1, Terms ),
  element( 2, Res0 ).

generateGameData(Terms) ->
	Routes = get(routes, Terms),
	Cars = get(cars, Terms),
	ItemClasses = get(itemClasses, Terms),

  Transaction = fun() ->
    lists:foreach(fun(Route) -> mnesia:write(Route) end, Routes),
		mnesia:write({uuid, route, 15}),
		lists:foreach(fun(Car) -> mnesia:write(Car) end, Cars),
		mnesia:write({uuid, cars, 10}),
		lists:foreach(fun(ItemClass) -> mnesia:write(ItemClass) end, ItemClasses),
		mnesia:write({uuid, itemClass, 100})
	end,

	{atomic, ok} = mnesia:clear_table(route),
	{atomic, ok} = mnesia:clear_table(car),
	{atomic, ok} = mnesia:clear_table(itemClass),
  crypto:start(),
	{atomic, ok} = mnesia:transaction(Transaction).

migrate() ->
	{ok, Terms} = file:consult("src/db/migrations/migration_35.data"),
	generateGameData(Terms),
	{migrated, ?MIGRATION_NUMBER}.
	