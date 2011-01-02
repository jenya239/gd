-module(migration_20).
-export([migrate/0]).
-include_lib("stdlib/include/qlc.hrl").

-record(carUpgrade,
   {
    id,
    userID,
    classID,
    durability,
    onCar
   }).

migrate() ->
	Tables = [
	{carUpgrade,[{attributes,record_info(fields,carUpgrade)},{disc_copies,[node()]},{type,set}]}
            ] ,
	mneser:createTables(Tables),
  {migrated,20}.