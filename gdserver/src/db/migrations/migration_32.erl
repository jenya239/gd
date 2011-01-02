-module(migration_32).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

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
    city = undefined,
    money = 0,
    realMoney = 0,
    fuelVolume = 40
    }).

migrate() ->
        {atomic, ok} = mnesia:transaction(fun() ->
                Guest = lists:nth(1, mnesia:read({user, guest})),
                mnesia:write(Guest#user{level=1})
            end),
    {migrated, 32}.
