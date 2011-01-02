-module(migration_59).
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
    scheduledTask,
    {
        id,
        dateTime
    }
).

-record(
    city,
    {
       id,
       score,
       minRating,
       minScore
    }).

migrate() ->
    mneser:createTables(
    [
        {scheduledTask, [{attributes, record_info(fields, scheduledTask)}, {disc_copies, [node()]}, {type, set}]}
    ]),    
    
    {atomic, ok} = mnesia:transform_table(city, fun(OldCity) ->
        list_to_tuple(tuple_to_list(OldCity) ++ [0])
    end, record_info(fields, city), city),
    
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=dailyBattleEndHour, value=1})
    end),
    
    mnesia:del_table_index(user, 11),
    {atomic, ok} = mnesia:del_table_index(tip, minLevel),
    {atomic, ok} = mnesia:del_table_index(tip, maxLevel),
    {atomic, ok} = mnesia:del_table_index(tip, available),
        
    {migrated, 59}.
    