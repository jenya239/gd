-module(migration_69).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    friendAction,
    {
        userID,
        friendID,
        washTime = 0
    }
 ).

migrate() ->
    mneser:createTables(
    [
        {friendAction, [{attributes, record_info(fields, friendAction)}, {disc_copies, [node()]}, {type, bag}]}
    ]),    
        
    mnesia:dirty_write({global, washCarMoney, 10}),
        
    {migrated, 69}.
    