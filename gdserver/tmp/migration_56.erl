-module(migration_56).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    userProgressOld, 
    {
        userID,
        leveling = [{1, 0}],
        onlineTime = 0,
        kilometers = 0,
        worksCounter = 0,
        invites = 0,
        activeInvites = 0
    }
).

-record(
    userProgress, 
    {
        userID,
        leveling = [{1, 0}],
        onlineTime = 0,
        kilometers = 0,
        worksCounter = 0,
        invites = 0,
        activeInvites = 0,
        realPurchases = 0
    }
).

-record(
    global,
    {        
        key,
        value
    }
).

migrate() ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=addRating, value=0}),
        mnesia:write(#global{key=ratingText, value="Подарок от Города Дорог!"})
    end),
    
    {atomic,ok} = mnesia:transform_table(userProgress, fun(UserProgress) -> 
        Old = utils:changeRecordName(UserProgress, userProgressOld),
        #userProgress{
            userID = Old#userProgressOld.userID,
            leveling = Old#userProgressOld.leveling,
            onlineTime = Old#userProgressOld.onlineTime,
            kilometers = Old#userProgressOld.kilometers,
            worksCounter = Old#userProgressOld.worksCounter,
            invites = Old#userProgressOld.invites,
            activeInvites = Old#userProgressOld.activeInvites,
            realPurchases = 0
        }
    end, record_info(fields, userProgress), userProgress),
    
    {migrated, 56}.