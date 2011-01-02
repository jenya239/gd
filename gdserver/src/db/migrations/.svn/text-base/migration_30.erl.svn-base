-module(migration_30).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
	itemClass,
	{
		id,
		name,
		category,
		description = "",
		price = 0,
		targetCars,
		usingType = slot, %slot или cast
		slot,
		usingCount = 0,
		minLevel = 1,
		maxLevel = 0,
		durability = 100,
		durabilityCoeff = 1,
		power = 0,
		speed = 0,
		braking = 0,
		controllability = 0,
		targetType = self, %self, other, world
                realPrice = 0
	}).

migrate() ->
    Fun = fun(OldItemClass) -> list_to_tuple(tuple_to_list(OldItemClass) ++ [0]) end,
    {atomic,ok} = mnesia:transform_table(itemClass, Fun, record_info(fields, itemClass), itemClass),
    {migrated, 30}.