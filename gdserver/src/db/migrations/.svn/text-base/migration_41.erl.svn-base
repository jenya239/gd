-module(migration_41).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

migrate() ->
	{atomic, ok} = mnesia:add_table_index(user, rating),
    {atomic, ok} = mnesia:add_table_index(user, vkontakteID),
    {atomic, ok} = mnesia:add_table_index(user, displayName),
	{migrated, 41}.