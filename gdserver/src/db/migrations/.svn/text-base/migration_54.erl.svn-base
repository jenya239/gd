-module(migration_54).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    userOld, 
    {
    id,
    name,
    currentCarID,
    level=1,
    experience=0,
    roles=[],
    triggers=[],
    date = erlang:now(),
    vkontakteID,
    rating=0,
    city,
    money=0,
    realMoney=0
    }).

-record(
    user, 
    {
    id,
    name,
    currentCarID,
    level=1,
    experience=0,
    roles=[],
    triggers=[],
    date = erlang:now(),
    vkontakteID,
    rating=0,
    city,
    money=0,
    realMoney=0,
    referer=0
    }).
    
-record(
    userProgressOld, 
    {
        userID,
        leveling = [{1, 0}],
        onlineTime = 0,
        kilometers = 0,
        worksCounter = 0
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
        activeInvites = 0
    }
).
 
migrate() ->    
    FunUser = fun(UserOld) -> 
        Old = utils:changeRecordName(UserOld, userOld),
        #user{
            id = Old#userOld.id,
            name = Old#userOld.name,
            currentCarID = Old#userOld.currentCarID,
            level = Old#userOld.level,
            experience = Old#userOld.experience,
            roles = Old#userOld.roles,
            triggers = Old#userOld.triggers,
            date = Old#userOld.date,
            vkontakteID = Old#userOld.vkontakteID,
            rating = Old#userOld.rating,
            city = Old#userOld.city,
            money = Old#userOld.money,
            realMoney = Old#userOld.realMoney,
            referer=0
        }
    end,

    FunProgress = fun(ProgressOld) -> 
        OldProgress = utils:changeRecordName(ProgressOld, userProgressOld),
        #userProgress{
            userID = OldProgress#userProgressOld.userID,
            leveling = OldProgress#userProgressOld.leveling,
            onlineTime = OldProgress#userProgressOld.onlineTime,
            kilometers = OldProgress#userProgressOld.kilometers,
            worksCounter = OldProgress#userProgressOld.worksCounter,
            invites = 0,
            activeInvites = 0
        }
    end,
    
    {atomic,ok} = mnesia:transform_table(user, FunUser, record_info(fields, user), user),
    {atomic,ok} = mnesia:transform_table(userProgress, FunProgress, record_info(fields, userProgress), userProgress),
    
    {migrated, 54}.