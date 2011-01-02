-module(usersTest).
-include("lib/eunit/include/eunit.hrl").
-include("data.hrl").

trigger_delete_test() ->  
    mnesia:clear_table(users),
    {ok, User} = dbUser:add(#user{ id=dbUuid:get(user), triggers = [test], name="Monster"}),
    users:deleteTrigger(User#user.id,test),    
    ?assertMatch([], (dbUser:getRecord(id,User#user.id))#user.triggers),
    mnesia:clear_table(users).
    

checkTrigger_test() ->

    User = #user{triggers=[test1, test2]},
    ?assertMatch(false, users:checkTrigger(User, test)),
    ?assertMatch(false, users:checkTrigger(User, notrigger)).
    
ratingTable_test() ->
    ThisResult  = #lobbyResult{userID=1, oldRating=2500,position=2},
    OtherResult = #lobbyResult{userID=2, oldRating=1000,position=1},
    ?assertEqual(-50,users:ratingTable(ThisResult, OtherResult,4)),
    ?assertEqual(50,users:ratingTable(OtherResult,ThisResult, 4)),
    ?assertEqual(0,users:ratingTable(ThisResult,ThisResult, 4)),
    ?assertEqual(0,users:ratingTable(OtherResult,OtherResult, 4)),
    ThisResult1  = #lobbyResult{userID=1, oldRating=2500,position=1},
    OtherResult1 = #lobbyResult{userID=2, oldRating=1000,position=2},
    ?assertEqual(0,users:ratingTable(ThisResult1, OtherResult1,4)) .

userExp_test() ->
    Route = #route{
        id=-1,
        displayName=0,
        fileName=0,
        length=100000,
        minLevel = 1,
        maxLevel = 100,
        moneyPrize = 5000,
        isHomeCity = true,
        isBattleCity = true,
        difficulty = 1},
    {aborted, {200,200} } =
        mnesia:transaction(
          fun() ->
              %MinAvgSpeed = dbGlobal:get_nt(minAvgSpeed),
              %MaxAvgSpeed = dbGlobal:get_nt(maxAvgSpeed),
             % ExpMin = dbGlobal:get_nt(expMin),
             % ExpMax = dbGlobal:get_nt(expMax),
              mnesia:write(Route),
              mnesia:write(#global{key=minAvgSpeed,value=50}),
              mnesia:write(#global{key=maxAvgSpeed,value=150}),
              mnesia:write(#global{key=expMin,value=100}),
              mnesia:write(#global{key=expMax,value=300}),
              A = users:calcExp_nt(-1, 1,3600*1000), % ?assertEqual(200,),
              B = users:calcExp_nt(-1, 3,3*3600*1000), % ?assertEqual(20,),
              mnesia:abort({A,B})
          end).


