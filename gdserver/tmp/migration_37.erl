-module(migration_37).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(vapiTest,
    {
        key,
        value
    }
).

migrate() ->
    mneser:createTables([
		{vapiTest, [{attributes, record_info(fields, vapiTest)}, {ram_copies,  [node()]}, {type, set}]}]),
    {migrated, 37}.
