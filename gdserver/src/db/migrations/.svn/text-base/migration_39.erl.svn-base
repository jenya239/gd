-module(migration_39).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    stopList,
    {
    id,      %  id забаненного пользователя
    time,    %  время когда его забанили
    period,  %
    admin    %  id админа который забанил
   }).

migrate() ->
    mneser:createTables([
		{stopList,
      [{attributes, record_info(fields, stopList)},
       {disc_copies,  [node()]},
       {type, bag}]}]),
    {migrated, 39}.