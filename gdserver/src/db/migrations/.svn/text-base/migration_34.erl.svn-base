-module(migration_34).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(advertisement,
    {
        id,
        date = erlang:localtime(),
        message
    }
).

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

-record(
    postMessage,
    {
        id,
        senderID,
        recepientID,
        timeStamp,
        item,
        money,
        comment
    }
).

-record(
    message,
    {
        id,
        from,
        subject = none,
        date,
        receiver ,
        body    
     }
).

migrate() ->
    FunAdv = fun(OldAdv) -> 
        OldAdv#advertisement{message=utils:unicode_to_utf8_escaped(OldAdv#advertisement.message)} 
    end,
    {atomic, ok} = mnesia:transform_table(advertisement, FunAdv, record_info(fields, advertisement)),
    
    FunUser = fun(OldUser) ->
        OldUser#user{displayName=utils:unicode_to_utf8_escaped(OldUser#user.displayName)} 
    end,
    {atomic, ok} = mnesia:transform_table(user, FunUser, record_info(fields, user)),                

    FunPostMessage = fun(OldPostMessage) ->
        OldPostMessage#postMessage{comment=utils:unicode_to_utf8_escaped(OldPostMessage#postMessage.comment)} 
    end,
    {atomic, ok} = mnesia:transform_table(postMessage, FunPostMessage, record_info(fields, postMessage)),                
        
    FunMessage = fun(OldMessage) ->
        OldMessage#message{body=utils:unicode_to_utf8_escaped(OldMessage#message.body)} 
    end,
    {atomic, ok} = mnesia:transform_table(message, FunMessage, record_info(fields, message)),
            
    {migrated, 34}.
