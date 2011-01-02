-module(migration_47).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    uuid,
    {
        type,
        current
    }
).

migrate() ->
    fix(carClass),
    {migrated, 47}.

fix(Type) ->
	{atomic, ok} =
		mnesia:transaction(
		fun() ->
			N = lists:max([1]++mnesia:all_keys(Type)),
			mnesia:write( #uuid{type=Type,current=N} )
		end).