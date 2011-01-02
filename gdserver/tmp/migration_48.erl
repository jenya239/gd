-module(migration_48).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    tip,
    {
        id,
        message,
        minLevel,
        maxLevel,
        available=true
    }
).

migrate() ->
    mneser:createTables([
		{tip,
      [{attributes, record_info(fields, tip)},
       {disc_copies,  [node()]},
       {type, set}]}]),
    {atomic, ok} = mnesia:add_table_index(tip, minLevel),
    {atomic, ok} = mnesia:add_table_index(tip, maxLevel),
    {atomic, ok} = mnesia:add_table_index(tip, available),
    {migrated, 48}.