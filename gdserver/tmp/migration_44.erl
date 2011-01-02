-module(migration_44).

-export([migrate/0]).

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
    {atomic, ok} = mnesia:add_table_index(message, #message.date),
	{migrated, 44}.