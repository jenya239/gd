-module(migration_22).
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
    money = 0
    }).

migrate() ->
    %% add one integer "money" field with value = 0
    Fun = fun(UserOld) -> list_to_tuple(tuple_to_list(UserOld) ++ [1000]) end,
    {atomic,ok} = mnesia:transform_table(user, Fun, record_info(fields, user), user),
    
    {migrated, 22}.