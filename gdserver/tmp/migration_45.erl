-module(migration_45).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    systemInfo,
    {
        timestamp = 0, % in minutes
        cpuLoad, 
        memUsage,
        memTotal
    }
).

migrate() ->
	Tables = [
	    {systemInfo, [{attributes, record_info(fields, systemInfo)}, {disc_copies, [node()]}, {type, set}]}
	],
            
	mneser:createTables(Tables),
	{migrated, 45}.