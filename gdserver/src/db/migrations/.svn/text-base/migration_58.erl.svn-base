-module(migration_58).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

-record(
    global,
    {        
        key,
        value
    }
).

-record(
    city,
    {
       id,
       score,
       minRating
    }
).

-record(
    scoreHistory,
    {
        date,
        table
    }
).

-record(
    userDailyScore,
    {
        userID,
        score
    }
).

-record(
    userOld,
    {
    id,
    name,
    currentCarID,
    level=1,
    experience=0,
    roles=[],
    triggers=[],
    date = erlang:now(),
    vkontakteID,
    rating=0,
    city,
    money=0,
    realMoney=0,
    referer=0,
    duelWin=0,
    duelCount=0
    }).

-record(
    user, 
    {
    id,
    name,
    currentCarID,
    level=1,
    experience=0,
    roles=[],
    triggers=[],
    date = erlang:now(),
    vkontakteID,
    rating=0,
    homeCity,
    currentCity,
    money=0,
    realMoney=0,
    referer=0,
    duelWin=0,
    duelCount=0    
    }).

-record(
    userState, 
    {
        userID,
        state
    }
).

migrate() ->
    mneser:createTables([{city, [{attributes, record_info(fields, city)}, {disc_copies, [node()]}, {type, set}]},
                         {userDailyScore, [{attributes, record_info(fields, userDailyScore)}, {disc_copies, [node()]}, {type, set}]},
                         {scoreHistory, [{attributes, record_info(fields, scoreHistory)}, {disc_copies, [node()]}, {type, set}]},
                         {userState, [{attributes, record_info(fields, userState)}, {disc_copies, [node()]}, {type, set}]}]),
    
    FunUser = fun(UserOld) -> 
        Old = utils:changeRecordName(UserOld, userOld),
        #user{
            id = Old#userOld.id,
            name = Old#userOld.name,
            currentCarID = Old#userOld.currentCarID,
            level = Old#userOld.level,
            experience = Old#userOld.experience,
            roles = Old#userOld.roles,
            triggers = Old#userOld.triggers,
            date = Old#userOld.date,
            vkontakteID = Old#userOld.vkontakteID,
            rating = Old#userOld.rating,
            homeCity = Old#userOld.city,
            currentCity = Old#userOld.city,
            money = Old#userOld.money,
            realMoney = Old#userOld.realMoney,
            referer=Old#userOld.referer,
            duelWin=Old#userOld.duelWin,
            duelCount=Old#userOld.duelCount            
        }
    end,

    {atomic, ok} = mnesia:transform_table(user, FunUser, record_info(fields, user), user),
    {atomic, ok} = mnesia:transaction(fun() ->
        MinCityRating1 = case mnesia:read({global, minCityRating1}) of
            [MinCityRating1_] -> MinCityRating1_#global.value;
            [] -> 0
        end,
        MinCityRating2 = case mnesia:read({global, minCityRating2}) of
            [MinCityRating2_] -> MinCityRating2_#global.value;
            [] -> 0
        end,            
        mnesia:write(#city{id=1, score=0, minRating=MinCityRating1}),
        mnesia:write(#city{id=2, score=0, minRating=MinCityRating2}),
        mnesia:write(#city{id=3, score=0, minRating=0})
    end),
        
    {migrated, 58}.
    