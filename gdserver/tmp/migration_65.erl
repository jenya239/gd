-module(migration_65).

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
    userDetails,
    {
        id,
        inventory = [],
        cars = [],
        carSlots = 3
    }
).

migrate() ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=maxCarSlots, value=6}),
        mnesia:write(#global{key=carSlotCost, value=30})
    end),
    
    {atomic, ok} = mnesia:transform_table(userDetails, fun(OldRecord) ->
        list_to_tuple(tuple_to_list(OldRecord) ++ [3])
    end, record_info(fields, userDetails), userDetails),
            
    {migrated, 65}.