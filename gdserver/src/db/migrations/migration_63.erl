-module(migration_63).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    userDailyScore,
    {
        userID,
        score,
        homeCity
    }
).

migrate() ->
    mnesia:clear_table(userDailyScore),
    {atomic, ok} = mnesia:transform_table(userDailyScore, fun(_) ->
        ignore
    end, record_info(fields, userDailyScore), userDailyScore),
            
    {migrated, 63}.
    