-module(migration_49).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    postMessageOld,
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
    postMessage,
    {
        id,
        senderID,
        recepientID,
        timeStamp,
        item,
        money,
        sellPrice,
        comment
    }
).
 
migrate() ->    
    Fun = fun(PostMessageOld) -> 
        Old = utils:changeRecordName(PostMessageOld, postMessageOld),
        #postMessage{
            id=Old#postMessageOld.id,
            senderID=Old#postMessageOld.senderID,
            recepientID=Old#postMessageOld.recepientID,
            timeStamp=Old#postMessageOld.timeStamp,
            item=Old#postMessageOld.item,
            money=Old#postMessageOld.money,
            sellPrice=0,
            comment=Old#postMessageOld.comment
        }
    end,
    
    {atomic,ok} = mnesia:transform_table(postMessage, Fun, record_info(fields, postMessage), postMessage),
    {migrated, 49}.