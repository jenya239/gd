-module(migration_42).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
    session,
    {
        id,
        userID,
        startTime,
        endTime,
        killometers = 0,
        worksCounter = 0
    }
).

-record(
    userProgress, 
    {
        userID,
        leveling = [{1, 0}],
        onlineTime = 0,
        kilometers = 0,
        worksCounter = 0
    }
).

-record(
    user, 
    {
        id,
        login,
        password,
        salt,
        email,
        displayName,
        image = "",
        currentCarID = 1,
        defaultColor = 1,
        level = 1,
        experience = 0,    
        roles = [],
        triggers = [],
        date = erlang:now(),
        vkontakteID = 0,
        nitroCount = 5,
        rating = 0,
        city = undefined,
        money = 1000,
        realMoney = 0,
        fuelVolume = 40
    }
).

migrate() ->
	Tables = [
	    {session, [{attributes, record_info(fields, session)}, {disc_copies, [node()]}, {type, set}]},
	    {userProgress, [{attributes, record_info(fields, userProgress)}, {disc_copies, [node()]}, {type, set}]}
	],
            
	mneser:createTables(Tables),
	
	{atomic, Users} = mnesia:transaction(fun() -> 
	    qlc:e(qlc:q([X || X <- mnesia:table(user)]))
	end),
	
	lists:foreach(fun(User) -> 
	    UserID = User#user.id,
	    Level = User#user.level,
	    {atomic, ok} = mnesia:transaction(fun() ->
	        mnesia:write(#userProgress{userID=UserID, leveling=[{Level, 0}]})
        end)
	end, Users),
	
	{migrated, 42}.