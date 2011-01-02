-module(migration_19).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    userOld, 
    {
    id,
    login,
    password,
    salt,
    email,
    displayName,
    image = "",
    currentCarID = 1,
    defaultColor = "ff0000",
    level = 1,
    experience = 0,    
    roles = [],
    triggers = [],
    date = erlang:now(),
    vkontakteID = 0,
    nitroCount = 5,
    rating = 0
    }).

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
    city = undefined
    }).
 
migrate() ->    
    Fun = fun(UserOld) -> 
        Old = utils:changeRecordName(UserOld, userOld),
        #user{
                id = Old#userOld.id,
                login = Old#userOld.login,
                password = Old#userOld.password,
                salt = Old#userOld.salt,
                email = Old#userOld.email,
                displayName = Old#userOld.displayName,
                image = Old#userOld.image,
                currentCarID = random:uniform(3),
                defaultColor = random:uniform(8),
                level = 1,
                experience = 100,
                roles = Old#userOld.roles,
                triggers = Old#userOld.triggers,
                date = Old#userOld.date,
                vkontakteID = Old#userOld.vkontakteID,
                nitroCount = Old#userOld.nitroCount,
                rating = Old#userOld.rating,
                city = random:uniform(2)
        }
    end,
    
    {atomic,ok} = mnesia:transform_table(user, Fun, record_info(fields, user), user),
    {migrated, 19}.