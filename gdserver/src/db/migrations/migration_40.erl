-module(migration_40).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    onlineStats,
    {
        timestamp = 0,
        min = 0,
        max = 0
    }
).

migrate() ->
	Tables = [
	    {onlineStats, [{attributes, record_info(fields, onlineStats)}, {disc_copies, [node()]}, {type, set}]}
	],
            
	mneser:createTables(Tables),
	{migrated, 40}.