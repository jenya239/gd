-module(migration_71).

-include_lib("stdlib/include/qlc.hrl").

-export([migrate/0]).

-record(
    levelOld,
    {
        id,
        number,
        experience,
        trigger,
        message
    }
).    

-record(
    level,
    {
        id,
        number,
        experience,
        money = 0,
        realMoney = 0,
        itemID,
        nitroID,
        message
    }
).    

migrate() ->
    Fun = fun(OldRecord) ->
            LevelOld = utils:changeRecordName(OldRecord, levelOld),
            #level{
                id = LevelOld#levelOld.id,
                number = LevelOld#levelOld.number,
                experience = LevelOld#levelOld.experience,
                message = LevelOld#levelOld.message
            }
    end,
    
    {atomic, ok} = mnesia:transform_table(level, Fun, record_info(fields, level), level),
    {migrated, 71}.