-module(migration_53).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).
-record(
    trafficStat,
    {
       time,
       name,
       size
    }
).

migrate() ->
    mneser:createTables([
		{trafficStat,
      [{attributes, record_info(fields, trafficStat)},
       {disc_copies,  [node()]},
       {type, bag}]}]),
    {migrated, 53}.