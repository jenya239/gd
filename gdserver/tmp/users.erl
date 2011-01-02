-module(users).

-export([addExperience/2,
         checkTrigger/2,
         checkRole/2,
         addTrigger_nt/2,
         addTrigger/2,
         deleteTrigger/2,
         changeTrigger/3,
         getUserInfo/1,
         getUserDetails/1,
         calculateRatings/2,
         considerLobbyResults/4,
         calculateMoney/3,
		 sendLevelUpMessages/2,
         calculateScores/3,
         setTrigger/3,
         hasGift/1,
         canCheat/1,
         ratingTable/3, % export for test
         getTriggerValue/3,
         setTriggerValue_nt/3,
         last5clock/0,
         calcExp_nt/3, 
         giveLevelUpPrize/3
   ]).

-include("data.hrl").
-include("lib/eunit/include/eunit.hrl").

addExperience(UserID, Experience) ->
	{atomic, Res} = mnesia:transaction(fun() -> addExperience_nt(UserID,Experience) end),
	Res.

addExperience_nt(UserID, Experience) ->
    if is_number(UserID) -> % for test. userid can be undefined only in tests.
    	User = dbUser:getRecord_nt(id, UserID),
    	NewExperience = User#user.experience + Experience,
    	NextLevel = dbGame:getLevel_nt(number, User#user.level + 1),
    	if
    		(NextLevel /= undefined) and (NewExperience >= NextLevel#level.experience) ->
    			NewLevel = NextLevel#level.number,
    			mnesia:write(User#user{level=NewLevel, experience = NewExperience}),
    			{_, ExpNextLevel} = getExpLevels(NextLevel#level.number-1),
				dbActivity:register_nt(UserID, {newLevel, NewLevel}, ok),
				serverSocket:sendMessageToClientByUserID(UserID, {newLevel, NewLevel}),
				users:addTrigger_nt(UserID, {levelUp, NewLevel}),
    			ExpLeft = ExpNextLevel - Experience,
                
                if NewLevel =:= 3 andalso User#user.referer > 0 ->
                    RefererUser = dbUser:getRecord_nt(vkontakteID, User#user.referer),
                    if erlang:is_record(RefererUser, user) ->
                        RefererProgress = dbUserProgress:getOrCreate_nt(RefererUser#user.id),
                        OldActiveInvites = RefererProgress#userProgress.activeInvites,
                        mnesia:write(RefererProgress#userProgress{activeInvites=OldActiveInvites+1}),
                        if OldActiveInvites + 1 =:= 5 ->
                            dbUser:addRealMoney_nt(RefererUser#user.id, 10),
                            users:addTrigger_nt(RefererUser#user.id, showInvitesReward),
                            dbActivity:register_nt(RefererUser#user.id, {invitesReward, 10}, ok);
                        true -> ok end;
                    true -> ok end;
                true -> ok end,
            
    			{ok, newLevel, NewLevel, ExpLeft, NextLevel#level.message};
    		true ->
    			mnesia:write(User#user{experience = NewExperience}),
    			ok
    	end;
	true ->
	   ok
   end.
   
calculateRatings(LobbyResults,LobbyInfo) ->
	lists:map(
			fun(LobbyResult) ->
					NewRating = LobbyResult#lobbyResult.oldRating + lists:foldl(
                        fun(OtherLobbyResult, DeltaRating) ->
                            DeltaRating + ratingTable(LobbyResult, OtherLobbyResult, LobbyInfo#lobbyInfo.league)
						end, 0, LobbyResults),
					NewRating2 = if	(NewRating < 0) -> 0;
                        true -> NewRating
					end,
					LobbyResult#lobbyResult{newRating=NewRating2}
			end, LobbyResults).

ratingTable(ThisResult, OtherResult,League) ->
    if League < 4
         -> 0;
       ThisResult#lobbyResult.userID =:= OtherResult#lobbyResult.userID
         -> 0;
       true ->
           C1 = 50,
           C2 = 400,
           DeltaRating = OtherResult#lobbyResult.oldRating - ThisResult#lobbyResult.oldRating,
           Probability = 1 / ( 1 + math:pow(10,DeltaRating / C2)  ),
           % 1 / ( 1 + math:pow(10,(-1500) / 400)  )
           W = if 
                 ThisResult#lobbyResult.position < OtherResult#lobbyResult.position -> 1.0;
                 true -> 0.0
           end,
           round(C1 * (W - Probability) )
    end.

calcPrizeFund(LobbyInfo,Players) ->
    if is_number(LobbyInfo#lobbyInfo.routeID) -> % for tests. routeid can be undefined only in tests.
    	League = mneser:getRecord(league, LobbyInfo#lobbyInfo.league),
      Route = mneser:getRecord(route, LobbyInfo#lobbyInfo.routeID),
    	BaseSum = Route#route.moneyPrize * League#league.moneyCoef,
    	%Players = LobbyInfo#lobbyInfo.playerCountBlue + LobbyInfo#lobbyInfo.playerCountRed,
    	Laps = LobbyInfo#lobbyInfo.lapNumber,
    	PlayerMinLevel = 1, %где его узнать???,
    	Coef = case dbGlobal:exists(prizeFundCoef) of
    					 true -> dbGlobal:get(prizeFundCoef);
    					 false -> 0.1
    				 end,
      if (LobbyInfo#lobbyInfo.league == 4)
    	       -> LobbyInfo#lobbyInfo.stake;
        true -> Coef * BaseSum * Players * Laps * PlayerMinLevel
      end;
	true ->
	   100
   end.

generatePrizeRatios(PlayerCount,LobbyInfo) ->
	if is_integer(PlayerCount) ->
		if
			PlayerCount == 1 -> [0];
			PlayerCount > 1 ->
				K0 = 0.5 - 0.02 * ( PlayerCount - 2 ),
				K = 0.48 + 0.02 * ( PlayerCount - 2 ),
				if 
            (LobbyInfo#lobbyInfo.league == 4) ->
                Rate = dbGlobal:get(duelRate),
                [1-Rate,-1];
            true -> [ K0 * math:pow( K, N-1 ) || N <- lists:seq( 1, PlayerCount ) ]
        end

		end
	end.

calculateMoney(LobbyInfo, LobbyResults,CountTotal) ->
	PrizeFund = calcPrizeFund(LobbyInfo,CountTotal),
	lists:map(fun({LobbyRes, Ratio}) ->
    	LobbyRes#lobbyResult{money = PrizeFund * Ratio}
	end, lists:zip(LobbyResults, generatePrizeRatios(CountTotal,LobbyInfo))).

calculateScores(_LobbyInfo, LobbyResults,PlayerCount) ->
	MaxScore = PlayerCount * 2,
	{NewLobbyResults, _} = lists:foldl(fun(LobbyRes, {NewResults, CurScore}) ->
        {NewResults ++ [LobbyRes#lobbyResult{score = CurScore}], CurScore - 2}
    end, {[], MaxScore}, LobbyResults),
    NewLobbyResults.

updateMoney_nt(LobbyResults,LobbyInfo) ->
	lists:foreach(
			fun(LobbyRes) ->
			    if is_number(LobbyRes#lobbyResult.userID) -> % for test. userid can be undefined only in tests.
    				User = dbUser:getRecord_nt(id, LobbyRes#lobbyResult.userID),
            {Dcount,Dwin} = if
                   ( LobbyInfo#lobbyInfo.league == 4 andalso LobbyRes#lobbyResult.money > 0  ) -> {1,1};
                   ( LobbyInfo#lobbyInfo.league == 4 andalso LobbyRes#lobbyResult.money < 0  ) -> {1,0};
                   true -> {0,0}
            end,
    				mnesia:write(
    					User#user{
                money     = User#user.money + LobbyRes#lobbyResult.money,
                duelCount = Dcount + User#user.duelCount,
                duelWin   = Dwin +  User#user.duelWin   }
    				),
            dbActivity:register_nt(User#user.id, {finishLobby, User#user.money, LobbyRes#lobbyResult.money ,User#user.money + LobbyRes#lobbyResult.money}, ok);
           true ->
				    ok
			    end
			end, LobbyResults).

updateRatings_nt(LobbyResults) ->
	lists:foreach(
			fun(LobbyResult) ->
			    if is_number(LobbyResult#lobbyResult.userID) -> % for test. userid can be undefined only in tests.
    				User = dbUser:getRecord_nt(id, LobbyResult#lobbyResult.userID),
					vkontakte:setStatus(User#user.vkontakteID, "Рейтинг: " ++ integer_to_list(LobbyResult#lobbyResult.newRating) ),
    				mneser:writeRecord(
    					User#user{rating = LobbyResult#lobbyResult.newRating}
    				);
				true ->
				    ok
			    end
			end, LobbyResults).

updateScores_nt(LobbyResults, BlueCount, RedCount) ->
	{BlueScore, RedScore} = lists:foldl(fun(LobbyResult, {BlueSum, RedSum}) ->
        if is_number(LobbyResult#lobbyResult.userID) -> % for test. userid can be undefined only in tests.
            Score = LobbyResult#lobbyResult.score,
            dbUserDailyScore:addScore_nt(LobbyResult#lobbyResult.userID, Score),
            case LobbyResult#lobbyResult.city of
                1 -> {BlueSum+Score, RedSum};
                2 -> {BlueSum, RedSum+Score};
                _ -> {BlueSum, RedSum}
            end;
        true ->
            {BlueSum, RedSum}
        end
    end, {0, 0}, LobbyResults),
    
    Coeff = BlueCount / RedCount,
    if Coeff > 1.0 ->
        {utils:floor(BlueScore / Coeff), RedScore};
    true ->
        {BlueScore, utils:floor(RedScore * Coeff)}
    end.

calcExp_nt(RouteID, LapNumber,Time) ->
	Route = mneser:getRecord_nt(route,RouteID),
  MinAvgSpeed = dbGlobal:get_nt(minAvgSpeed), % 70,
  MaxAvgSpeed = dbGlobal:get_nt(maxAvgSpeed), % 180,
  ExpMin = dbGlobal:get_nt(expMin), % 50,
  ExpMax = dbGlobal:get_nt(expMax), % 900,
  RouteLength = Route#route.length * LapNumber / 1000,
  %io:format("~n RouteLength ~w~n",[RouteLength ]),
  AvgSpeed = ((RouteLength  * 3600 * 1000 ) /  Time ) * Route#route.difficulty, % km / h
  %io:format("AvgSpeed ~w~n",[AvgSpeed]),
  %io:format("Time ~w~n",[Time]),
  ExpRaw = (ExpMin-ExpMax)/(MinAvgSpeed - MaxAvgSpeed)*AvgSpeed +
            (
               (ExpMax*MinAvgSpeed - ExpMin*MaxAvgSpeed) / (MinAvgSpeed - MaxAvgSpeed)
            ),
  %io:format("ExpRaw ~w~n",[ExpRaw]),
  utils:max(20,round(ExpRaw*(LapNumber/4 + 1/4)*(Route#route.length/700 + 1/14))).
  %utils:max(20,round(ExpRaw)).
	%EtalonTime = LapNumber * Route#route.time,
	%Exp = LapNumber * Route#route.exp,
	%Lst = [ {math:pow(1.2, N) * EtalonTime, math:pow(0.5, N) * Exp }
	%			 || N <- lists:seq(0, 3)],
	%l%ists:foldl(fun({T,E},Acc) ->
	%								if (Time < T) and (E > Acc) ->
	%										E;
	%									true -> Acc
	%								end
	%						end,20,Lst).

%должна возвращать {LobbyResultsWithExp, LevelupMessages}
updateExp_nt(LobbyResults, LobbyInfo) ->
	LobbyResultsWithExp = lists:map(
		fun(LobbyResult) ->
			case LobbyResult#lobbyResult.finished of
				true ->
				    if is_number(LobbyResult#lobbyResult.userID) -> % for tests. userid can be undefined only in tests.    					
    					DExp = if LobbyInfo#lobbyInfo.league =:=4 -> 0;
                    true ->
                        calcExp_nt(
                            LobbyInfo#lobbyInfo.routeID,
                            LobbyInfo#lobbyInfo.lapNumber,
                            LobbyResult#lobbyResult.time
                          )
                    end,
    					LobbyResult#lobbyResult{experience = DExp};
					true ->
					    LobbyResult
				    end;
				false ->
					LobbyResult
			end
		end,
		LobbyResults
	),
	
	LevelupMessages = lists:foldl(
		fun(LobbyResult, List) ->
			case LobbyResult#lobbyResult.finished of
				true ->
					case addExperience_nt(LobbyResult#lobbyResult.userID, LobbyResult#lobbyResult.experience) of
						{ok, newLevel, NewLevel, _ExpLeft, _Message} ->
							[{LobbyResult#lobbyResult.clientID, "[[levelUp, " ++ integer_to_list(NewLevel) ++ "]]" } | List];
						ok -> List
					end;
				false ->
					List
			end
		end,
		[],
		LobbyResultsWithExp
	),
	{LobbyResultsWithExp, LevelupMessages}.

 considerLobbyResults_nt(LobbyResults, LobbyInfo, BlueCount, RedCount)->
	updateRatings_nt(LobbyResults),
	updateMoney_nt(LobbyResults, LobbyInfo),
	
	{BlueScore, RedScore} = case LobbyInfo#lobbyInfo.type of
	    team ->
            {Blue, Red} = updateScores_nt(LobbyResults, BlueCount, RedCount),
            EP = dbGlobal:get_nt(blueScoreExtraPercent),
            Blue2 = Blue * (1+EP),
            Red2 = Red * (1-EP),
            dbCity:addScore_nt(1, Blue2),
            dbCity:addScore_nt(2, Red2),
            {Blue, Red};
        _Other ->
            {0, 0}
    end,
    
    FuelLobbyResult = fuel:writeOffFuel(LobbyResults, LobbyInfo),
	{LobbyResultsWithExp, LevelupMessages} = updateExp_nt(FuelLobbyResult, LobbyInfo),
    dbCar:wearOutCars_nt(LobbyResultsWithExp, LobbyInfo),
	{LobbyResultsWithExp, carUpgrades:wearOutAllUpgrades_nt(LobbyResultsWithExp, LobbyInfo), LevelupMessages, BlueScore, RedScore}.

sendLevelUpMessages(List, Clients) ->
	lists:foreach(fun({ClientID, MessageText }) ->
        Message = {otherChatMessage, city, MessageText, -1, 0, "[[systemMessage]]", utils:now()},
        utils:sendMessageToClient(ClientID, {get,tipInfo}, Clients),
        utils:sendMessageToClient(ClientID, Message, Clients)
    end, List).

considerLobbyResults(LobbyResults, LobbyInfo, BlueCount, RedCount) ->
	{atomic, ResultOfFun} = mnesia:transaction(fun() -> considerLobbyResults_nt(LobbyResults, LobbyInfo, BlueCount, RedCount) end),
	ResultOfFun.

getExpLevels(Level) ->
	L1 = dbGame:getLevel(number, Level),
	L2 = dbGame:getLevel(number, Level+1),
	{L1#level.experience, L2#level.experience}.

getUserInfo(User) ->
	{ExpPrevLevel, ExpNextLevel} = getExpLevels(User#user.level),
    Car = mneser:getRecord(car, User#user.currentCarID),
    UserDetails = dbUser:getDetails(User#user.id),
    
    CarSlots = if erlang:is_record(UserDetails, userDetails) ->
        UserDetails#userDetails.carSlots;
    true ->
        3
    end,
    
    IsWashed = dbUser:isFriendCarClean(User#user.id, User#user.id),
    
	UserInfo = #userInfo{user=User, car = Car, expPrevLevel=ExpPrevLevel, expNextLevel=ExpNextLevel, carSlots=CarSlots, isWashed=IsWashed},
    getUserDetails(UserInfo).

getUserDetails(UserInfo) ->
    UserID = (UserInfo#userInfo.user)#user.id,
	case dbItem:getUserInventoryAndEquipment(UserID) of
		{Inventory, Equipment} ->
			UserInfo#userInfo{inventory=Inventory, equipment=Equipment, cars=dbCar:getUserCars(UserID)};
		_Other ->
			UserInfo
	end.

checkTrigger(User, Trigger) ->
    lists:any(fun (Element) ->
        Element == Trigger
	end, User#user.triggers).

checkRole(User, Role) ->
    lists:any(fun (Element) ->
        Element == Role
	end, User#user.roles).

deleteTrigger_nt(UserID, Trigger) ->
    case mnesia:wread({user, UserID}) of
		 [DBUser] ->
			 mnesia:write(DBUser#user{triggers = lists:subtract(DBUser#user.triggers,[Trigger])});
		 _ ->
			 error
	 end.

deleteTrigger(UserID, Trigger) ->
	mnesia:transaction(fun() ->
	    deleteTrigger_nt(UserID, Trigger)
	end).
    
addTrigger_nt(UserID, Trigger) ->
    case mnesia:wread({user, UserID}) of
         [DBUser] ->
             mnesia:write(DBUser#user{triggers = DBUser#user.triggers ++ [Trigger]});
         _ ->
             error
    end.
     
addTrigger(UserID, Trigger) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        addTrigger_nt(UserID, Trigger)
    end),
    
    Result.

changeTrigger(UserID, Trigger, Delta) ->    
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:wread({user, UserID}) of
            [DBUser] ->
                case lists:keysearch(Trigger, 1, DBUser#user.triggers) of
                    {value, {TName, TValue}} ->
                        {NewTriggers, NewValue} = if TValue + Delta > 0 ->
                            NewValue_ = TValue + Delta,
                            {lists:keyreplace(Trigger, 1, DBUser#user.triggers, {TName, NewValue_}), NewValue_};
                        true ->
                            {lists:keydelete(Trigger, 1, DBUser#user.triggers), undefined}
                        end,
                        
                        mnesia:write(DBUser#user{triggers=NewTriggers}),
                        NewValue;
                    false ->                        
                        case lists:member(Trigger, DBUser#user.triggers) andalso Delta < 0 of
                            true ->                                
                                NewTriggers = lists:delete(Trigger, DBUser#user.triggers),
                                mnesia:write(DBUser#user{triggers=NewTriggers}),
                                0;
                            false ->                                
                                error
                        end
                end;
            _ ->
                error
        end
    end),
    Result.

setTrigger(UserID, Trigger, Value) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:wread({user, UserID}) of
            [DBUser] ->
                case lists:keysearch(Trigger, 1, DBUser#user.triggers) of
                    {value, {_TName, _TValue}} ->
                        NewTriggers = lists:keyreplace(Trigger, 1, DBUser#user.triggers, {Trigger, Value}),
                        mnesia:write(DBUser#user{triggers=NewTriggers});
                    false ->
                        NewTriggers = [{Trigger, Value}|DBUser#user.triggers],
                        mnesia:write(DBUser#user{triggers=NewTriggers})
                end;
            _ ->
                error
        end
    end),
    Result.
    
setTriggerValue_nt(User, Trigger, Value) ->
    case lists:keysearch(Trigger, 1, User#user.triggers) of
        {value, {_TName, _TValue}} ->
            NewTriggers = lists:keyreplace(Trigger, 1, User#user.triggers, {Trigger, Value}),
            User#user{triggers=NewTriggers};
        false ->
            NewTriggers = [{Trigger, Value} | User#user.triggers],
            User#user{triggers=NewTriggers}
    end.

last5clock() ->
    Now = utils:now(),
    Day = 24*60*60*1000,
    Hours = 5*60*60*1000,
    MS = Now rem Day,
    if ( MS < Hours ) ->
        Now - MS - Day + Hours;
    true ->
        Now - MS + Hours
    end.

getTriggerValue(User, TriggerName, DefaultValue) ->
    Lst = lists:filter(fun({Trigger, _}) ->
        Trigger =:= TriggerName;
    (_Trigger) ->
        false
    end, User#user.triggers),
    
    case Lst of
        [{_, V}] -> V;
        _ -> DefaultValue
    end.

hasGift(User) ->
    getTriggerValue(User, lastGiftTime, 0) < last5clock().

canCheat(User) ->
    getTriggerValue(User, lastCheatTime, 0) < last5clock().
    
giveLevelUpPrize(UserID, Level, Type) ->
    Result = try mnesia:transaction(fun() -> 
        User = dbUser:getRecord_nt(id, UserID),
        if erlang:is_record(User, user) ->
            case checkTrigger(User, {levelUp, Level}) of
                true ->
                    ok;
                _ ->
                    mnesia:abort(error, "[[databaseError]]")
            end,
            
            LevelRec = dbGame:getLevel_nt(number, Level),
            
            if not erlang:is_record(LevelRec, level) ->
                mnesia:abort(error, "[[databaseError]]");
            true -> ok end,
            
            case Type of
                money ->
                    dbUser:addMoney_nt(UserID, LevelRec#level.money),
                    dbUser:addRealMoney_nt(UserID, LevelRec#level.realMoney);
                _Other ->
                    ItemClassID = case Type of
                        item -> LevelRec#level.itemID;
                        nitro -> LevelRec#level.nitroID;
                        _ -> mnesia:abort(error, "Unknown reward type")
                    end,
                        
                    ItemClass = dbItem:getClass_nt(ItemClassID),
                    UserDetails = dbUser:getDetails_nt(UserID),
                    
                    NewItem = #item{id=dbUuid:get_nt(item), itemClassID=ItemClassID, durability=ItemClass#itemClass.durabilityMax, durabilityMax=ItemClass#itemClass.durabilityMax},
                    NewUserDetails = UserDetails#userDetails{inventory=UserDetails#userDetails.inventory ++ [NewItem#item.id]},
                    mnesia:write(NewItem),
                    mnesia:write(NewUserDetails)
            end,
            
            deleteTrigger_nt(UserID, {levelUp, Level}),
            dbActivity:register_nt(UserID, {levelUpPrize, Level, Type}, ok);
        true -> 
            mnesia:abort(error, "[[databaseError]]") 
        end
    end)
    
    catch throw: Throw ->
         Throw
    end,
    
    mneser:transactionResult(Result, UserID, {levelUpPrize, Level, Type}).
