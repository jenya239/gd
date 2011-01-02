-module(migration_38).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0, migrateItemClass/0]).

-record(global,
    {
        key,
        value
    }
).

-record(
	oldItemClass,
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
    
-record(
	itemClass,
	{
		id,
		name,
        junkName = "Обломки",
		category,
		description = "",
		price = 0,
        realPrice = 0,
        repairPrice = 1.3,
		targetCars,
		usingType = slot, %slot, cast, charge
		slot,
		usingCount = 0,
		minLevel = 1,
		maxLevel = 0,
		durabilityMax = 100,        
		power = 0,
		speed = 0,
		braking = 0,
		controllability = 0,
		targetType = self, %self, other, world
        available = true
	}).
    
-record(item,
    {id, itemsClassID, durability, durabilityMax}).

-record(
    userDetails,
    {
    id,
    inventory = [],
    equipment = []
   }). 

moveAllUserDetailsToItems() ->
    UserDetailsList = qlc:e(qlc:q([UD || UD <- mnesia:table(userDetails)])),
    lists:foreach(fun moveOneUserDetailsToItems/1, UserDetailsList).
    
moveOneUserDetailsToItems(UserDetails) ->
    NewInventoryIDList = copyItemListToItems(UserDetails#userDetails.inventory),
    NewEquipmentIDList = copyItemListToItems(UserDetails#userDetails.equipment),
    mnesia:write(UserDetails#userDetails{inventory=NewInventoryIDList, equipment=NewEquipmentIDList}).
    
copyItemListToItems(ItemList) ->
    lists:foldl(fun({ItemID, ItemClassID, Durability}, NewItemIDList) ->
        mnesia:write(#item{id=ItemID, itemsClassID=ItemClassID, durability=Durability*100, durabilityMax=100}),
        NewItemIDList ++ [ItemID]
    end, [], ItemList).

migrateItemClass() ->
    Fun = fun(OldItemClass_) ->
            OldItemClass = utils:changeRecordName(OldItemClass_, oldItemClass),
            #itemClass {
                id = OldItemClass#oldItemClass.id,
                name = OldItemClass#oldItemClass.name,
                category = OldItemClass#oldItemClass.category,
                description = OldItemClass#oldItemClass.description,
                price = OldItemClass#oldItemClass.price,
                targetCars = OldItemClass#oldItemClass.targetCars,
                usingType = OldItemClass#oldItemClass.usingType,
                slot = OldItemClass#oldItemClass.slot,
                usingCount = OldItemClass#oldItemClass.usingCount,
                minLevel = OldItemClass#oldItemClass.minLevel,
                maxLevel = OldItemClass#oldItemClass.maxLevel,
                power = OldItemClass#oldItemClass.power,
                speed = OldItemClass#oldItemClass.speed,
                braking = OldItemClass#oldItemClass.braking,
                controllability = OldItemClass#oldItemClass.controllability,
                targetType = OldItemClass#oldItemClass.targetType,
                realPrice = OldItemClass#oldItemClass.realPrice
            }
        end,
    
    {atomic, ok} = mnesia:transform_table(itemClass, Fun, record_info(fields, itemClass), itemClass).

migrate() ->
    mneser:createTables([
		{item, [{attributes, record_info(fields, item)}, {disc_copies,  [node()]}, {type, set}]}]),
    
    {atomic, ok} = mnesia:transaction(fun() ->
        ok = mnesia:write(#global{key=depreciationCoef, value=10}),
        ok = mnesia:write(#global{key=durabilityRedCoef, value=0.035}),
        ok = mnesia:write(#global{key=junkConversionCoef, value=0.2}),
        ok = mnesia:write(#global{key=durabilityJunkThreshold, value=3}),
        ok = mnesia:write(#global{key=sellItemRate, value=0.5})
    end),
    
    migrateItemClass(),
    
    {atomic, ok} = mnesia:transaction(fun() ->
        ok = moveAllUserDetailsToItems()
    end),
    
    {migrated, 38}.
