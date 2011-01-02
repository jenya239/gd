-module(migration_26).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(workOffer,
    {
        id,
        time,
        fuel,
        message
    }
).

migrate() ->
  Tables = [
	{workOffer, [{attributes, record_info(fields, workOffer)}, {disc_copies, [node()]}, {type, set}] }
           ],
	mneser:createTables(Tables),
        {atomic, ok} = mnesia:transaction(fun() ->
                mnesia:write(#workOffer{id=1,time = 600000, fuel = 1, message = "литр за 10 минут"}),
                mnesia:write(#workOffer{id=2,time = 3600000, fuel = 10, message = "10 литров за час"}),
                mnesia:write(#workOffer{id=3,time = 7200000, fuel = 25, message = "25 литров за два часа"}),
                mnesia:write(#workOffer{id=4,time = 10800000, fuel = 40, message = "40 литров за три часа"}),
                mnesia:write(#workOffer{id=5,time = 100, fuel = 0.00001, message = "0 литров мгновенно"})
        end),
 {migrated, 26}.
