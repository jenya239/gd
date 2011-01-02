-module(migration_24).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(global,
    {        
        key,
        value
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
        comment
    }
).

migrate() ->
    Tables = [
	{global, [{attributes, record_info(fields, global)}, {disc_copies, [node()]}, {type, set}]},
        {postMessage, [{attributes, record_info(fields, postMessage)}, {disc_copies, [node()]}, {type, set}]}
    ],
	mneser:createTables(Tables), 
        {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#global{key=commissionRate, value=0}),
                mnesia:write(#global{key=exchangeRate, value=30}),
                mnesia:write(#global{key=sendItemRate, value=0.05}),
                mnesia:write(#global{key=sendMoneyRate, value=0.05})
            end),
        
    {migrated, 24}.