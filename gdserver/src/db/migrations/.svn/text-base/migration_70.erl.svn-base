-module(migration_70).

-include_lib("stdlib/include/qlc.hrl").

-export([migrate/0]).

migrate() ->
    mnesia:dirty_write({global, inventorySize, 12}),
        
    {migrated, 70}.
