-module(migration_21).
-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(userDetails,
   {
    id,
    inventory = [],
    equipment = []
   }).

migrate() ->
	Tables = [
	{userDetails,[{attributes,record_info(fields,userDetails)},{disc_copies,[node()]},{type,set}]}
            ] ,
	mneser:createTables(Tables),
	
  {migrated,21}.