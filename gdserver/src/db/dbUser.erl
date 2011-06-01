-module(dbUser).
-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([ 
	getAllRecords/0,
	getAll/0,
	getRegUsersByDate/1,
	add/1,
	add_nt/1,
	getRecord_nt/2,
	getRecord/2,
	getRecords/2,
	getDetails/1,
	getDetails_nt/1,
	getInventoryAndEquipment_nt/1,
	getInventoryAndEquipment/1,
	getInfoWithDetailsAndUpgrades/2,
	exchangeMoney/2,
	addNitroCount/2,
	addRole/2,
	addRole_nt/2,
	addAdminRole/1,
	addDevRole/1,
    removeAdminRole/1,
    removeDevRole/1,
	getRatings/1,
	delete/1,
	getVkontakteId/1,
	canChat/1,
	getAllIds/0, 
	setMoney/2,
    addMoney/2,
    addMoney_nt/2,
    setRealMoney/2,
	addRealMoney/2,
    addRealMoney_nt/2,
	changeNickname/2, 
	startTransferToCity/4, 
	getUserState/1,
    getTopUserDailyScores/0,
    getTopUserDailyScores/1, 
    switchHomeCity/1, 
    restartGame/1,
    cleanCar/2,
    isFriendCarClean/2, 
	getRegisteredUserIDs/2
]).

getAllRecords() ->
	getAllRecords(3).

getAllRecords(Column) ->
	mneser:do(
			%TODO заменить индекс на имя поля
        qlc:keysort(Column, qlc:q([X || X <- mnesia:table(user)]))
	).

getAll() ->
	mneser:do
			(
			qlc:q([X#user.name || X <- mnesia:table(user)])
			).


getRegUsersByDate(DayCount) ->
	Pairs = mneser:do
			(
			qlc:q([ {Y#user.name, Y#user.vkontakteID}
						 || Y <- mnesia:table(user),
						 mneser:checkDate(DayCount, Y#user.date)
						])),
	lists:sort(Pairs).

add(User) ->
    {atomic, Executed} = mnesia:transaction(fun() -> 
        add_nt(User)
    end),
    Executed.

add_nt(User) ->
    case getRecord_nt(nickname, User#user.name) of
        {error, noNickname, _Nick} ->
            UserID =
                if
                  User#user.id == undefined ->
                      dbUuid:get_nt(user);
                  true ->
                      User#user.id
                end,
            UserRecord2 = User#user{id=UserID},
            mnesia:write(UserRecord2),
            
            if User#user.referer > 0 ->
                Referer = dbUser:getRecord_nt(vkontakteID, User#user.referer),
                if erlang:is_record(Referer, user) ->
                    RefererProgress = dbUserProgress:getOrCreate_nt(Referer#user.id),
                    OldInvites = RefererProgress#userProgress.invites,
                    mnesia:write(RefererProgress#userProgress{invites=OldInvites+1});
                true -> ok end;
            true -> ok end,
            
            {ok, UserRecord2};
        _Other ->
            {userAlreadyExsists, User#user.name}
    end.

getRecord_nt(id, ID) ->
	Result = qlc:e(qlc:q([X || X <- mnesia:table(user), X#user.id =:= ID])),
	case Result of
		[Element] ->
			Element;
		[] ->
			{error, noUserID, ID}
	end;

getRecord_nt(nicknameIgnoreCase, Nickname) ->
    LowerNickname = utils:toLower(Nickname),
    Result = qlc:e(qlc:q([X || X <- mnesia:table(user), utils:toLower(X#user.name) =:= LowerNickname])),
    case Result of
        [] ->
            {error, noNickname, Nickname};
        List when is_list(List) ->
            lists:nth(1, List)
    end;

getRecord_nt(nickname, Nickname) ->
    Result = qlc:e(qlc:q([X || X <- mnesia:table(user), X#user.name =:= Nickname])),
    case Result of
        [] ->
            {error, noNickname, Nickname};
        List when is_list(List) ->
            lists:nth(1, List)
    end;

getRecord_nt(vkontakteID, VkontakteID) ->
    Result = qlc:e(qlc:q([X || X <- mnesia:table(user), X#user.vkontakteID =:= VkontakteID])),
    case Result of
        [] ->
            {error, {noVkontakteID, VkontakteID}};
        List when is_list(List) ->
            lists:nth(1, List)
    end.
		
getRecord(id, ID) ->
    case mnesia:dirty_read({user, ID}) of
		[Element] ->
			Element;
		[] ->
			{error, noUserID, ID}
	end;   


getRecord(nickname, Nickname) ->
    case mnesia:dirty_index_read(user, Nickname, #user.name) of
		[Element | _Tail] ->
			Element;
		[] ->
			{error, noNickname, Nickname}
	end;

getRecord(vkontakteID, VkontakteID) ->
    case mnesia:dirty_index_read(user, VkontakteID, #user.vkontakteID) of
		[Element | _Tail] ->
			Element;
		[] ->
			{error, {noVkontakteID, VkontakteID}}
	end;
	
getRecord(nicknameIgnoreCase, Nickname) ->
    {atomic, User} = mnesia:transaction(fun() -> 
        getRecord_nt(nicknameIgnoreCase, Nickname)
    end), 
    User.

getRecords(vkontakteId, VkontakteId) ->
	mneser:do(qlc:q([X || X <- mnesia:table(user), X#user.vkontakteID =:= VkontakteId])).

getDetails(UserID) ->
    {atomic, UserDetails} = mnesia:transaction(fun() ->
        getDetails_nt(UserID)
    end),
    UserDetails.
    
getDetails_nt(UserID) ->
	Result = qlc:e(qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.id =:= UserID])),
	case Result of
		[UserDetails] ->
			UserDetails;
		[] ->
			Details = #userDetails{id=UserID},
			mnesia:write(Details),
			Details
	end.

getInventoryAndEquipment_nt(UserID) ->
	Result = qlc:e(qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.id =:= UserID])),
	case Result of
		[UserDetails] ->
      User = getRecord(id, UserID),
      Car = mneser:getRecord_nt(car, User#user.currentCarID),
			Fun =
					fun({ID, ItemClassID, D}) ->
							case qlc:e(qlc:q([I || I <- mnesia:table(itemClass), I#itemClass.id =:= ItemClassID])) of
								[ItemClass] ->
									{ID, ItemClass, D};
								_Other ->
									log:write("ItemClass with id ~b has not been found", [ItemClassID]),
									notFound
							end
					end,
			Inventory = lists:map(Fun, UserDetails#userDetails.inventory),
			Equipment = lists:map(Fun, Car#car.upgrades),
			{Inventory, Equipment};
		[] ->
			{[], []}
	end.

getInventoryAndEquipment(UserID) ->
	{atomic, Result} = mnesia:transaction(fun() ->
        dbUser:getInventoryAndEquipment_nt(UserID)
	end),
	Result.

getInfoWithDetailsAndUpgrades(UserID, ClientID) ->
	User = dbUser:getRecord(id, UserID),
	UserInfo = users:getUserInfo(User),
	UserInfoWithDetails = users:getUserDetails(UserInfo),
	UpgradeInfo = carUpgrades:getUpgradeInfo(UserID, ClientID),
	UserInfoWithDetails#userInfo{upgradeInfo=UpgradeInfo}.

exchangeMoney(UserID, RealMoneyToExchange) ->
	Result = try
    mnesia:transaction(
    fun() ->
        User = dbUser:getRecord_nt(id, UserID),
        OldRealMoney = User#user.realMoney,
        OldMoney = User#user.money,
        Rate = dbGlobal:get_nt(exchangeRate),
        Commission = dbGlobal:get_nt(commissionRate),
        if RealMoneyToExchange > OldRealMoney ->
            mnesia:abort({error, "[[notEnoughMoney]]"});
        true ->
            ok
        end,
        AdditionalMoney = utils:trunc(RealMoneyToExchange * Rate * (1-Commission), 2),
        NewMoney = OldMoney + AdditionalMoney,
        NewRealMoney = OldRealMoney - RealMoneyToExchange,
        mnesia:write(User#user{realMoney=NewRealMoney, money=NewMoney}),
        dbActivity:register_nt(User#user.id, {exchangeMoney, RealMoneyToExchange, Rate, Commission, OldMoney, NewMoney, OldRealMoney, NewRealMoney}, ok)
    end
                                         )
    catch throw: Throw ->
        Throw
    end,
	mneser:transactionResult(Result, UserID, exchangeMoney).

addNitroCount(UserID, ValueToAdd) ->
	{atomic, ok} = mnesia:transaction(fun() ->
		case dbUser:getRecord(id, UserID) of
			CurrentUser when is_record(CurrentUser, user) ->
                Car = mneser:getRecord_nt(car, CurrentUser#user.currentCarID),
				mnesia:write(Car#car{nitroCount = Car#car.nitroCount + ValueToAdd});
			[] -> error
		end,
		ok
	end).

addAdminRole(Id) ->
    addRole(Id, admin).
	
addDevRole(Id) ->
    addRole(Id, dev).
	
addRole(Id, Role) ->
	{atomic, ok} = mnesia:transaction(fun() ->
		addRole_nt(Id, Role)
	end).

addRole_nt(Id, Role) ->
	CurrentUser = getRecord_nt(id, Id),
	if erlang:is_record(CurrentUser, user) ->
		%ToDo что роль не была установлена ранее
		UpdatedUser = CurrentUser#user{roles = utils:nub([Role] ++ CurrentUser#user.roles)},
		mnesia:write(UpdatedUser);
	true -> ok end.

removeAdminRole(Id) ->
    removeRole(Id, admin).
    
removeDevRole(Id) ->
    removeRole(Id, dev).
	
removeRole(Id, Role) ->
	{atomic, ok} = mnesia:transaction(fun() ->
		CurrentUser = getRecord_nt(id, Id),
		if erlang:is_record(CurrentUser, user) ->
    		%ToDo что роль не была установлена ранее
            UpdatedUser = CurrentUser#user{roles = utils:nub(CurrentUser#user.roles -- [Role]) },
    		mnesia:write(UpdatedUser);
		true ->
		  ok
	    end
	end).
	
switchHomeCity(User) ->
    NewCity = case User#user.homeCity of
        1 ->
            2;
        2 ->
            1
    end,
    
    NewUser = User#user{homeCity=NewCity, currentCity=NewCity},
    ok = mnesia:dirty_write(NewUser),
    NewUser.
    
restartGame(UserID) ->
	{atomic, Result} = mnesia:transaction(fun() ->
		User = getRecord_nt(id, UserID),
    if erlang:is_record(User, user) ->
			Price = dbGlobal:get_nt(restartGameCost),
			OldRealMoney = User#user.realMoney,	
			if OldRealMoney < Price ->
				mnesia:abort({noMoney, "[[notEnoughMoney]]"});
			true ->	ok end,
      resetUser_nt(User#user{realMoney=OldRealMoney-Price});			
    true -> mnesia:abort({noUser, "User not found"}) end
  end),
  Result.

getTopUserDailyScores() ->
    getTopUserDailyScores(1) ++ getTopUserDailyScores(2).

getTopUserDailyScores(CityID) ->
	{atomic, Scores} = mnesia:transaction (
		fun() ->
            City = dbCity:get_nt(CityID),
            MinScore = City#city.minScore,
            N = 100,
			SortedHandle = qlc:keysort(#userDailyScore.score, 
                qlc:q([ UDS || 
                        UDS <- mnesia:table(userDailyScore),
                        UDS#userDailyScore.score >= MinScore, 
                        UDS#userDailyScore.homeCity =:= CityID
                        ]), {order, descending}),
			Cursor = qlc:cursor(SortedHandle),
			TopScores = qlc:next_answers(Cursor, N),
			qlc:delete_cursor(Cursor),
										 			
            NewMinScore = if length(TopScores) >= N -> 
                (lists:last(TopScores))#userDailyScore.score;
            true ->
                0
            end,
            
            TopScoresWithName = lists:reverse(lists:foldl(
                fun(UDS, NewList) ->
                    UserID = UDS#userDailyScore.userID,
                    User = dbUser:getRecord_nt(id, UserID),
                    if erlang:is_record(User, user) ->
                        [#userDailyScoreInfo{userID=UserID, score=UDS#userDailyScore.score, homeCity=UDS#userDailyScore.homeCity, displayName=User#user.name} | NewList];
                    true ->
                        NewList
                    end
                end, [], TopScores)),
            mnesia:write(City#city{minScore=NewMinScore}),
            TopScoresWithName
        end
    ),
    Scores.

getRatings(CityID) ->
	{atomic, Ratings} = mnesia:transaction (
		fun() ->
            City = dbCity:get_nt(CityID),
            MinRating = City#city.minRating,
			SortedHandle = qlc:keysort(#user.rating, qlc:q([X || X <- mnesia:table(user), X#user.rating >= MinRating, X#user.homeCity =:= CityID, is_integer(X#user.id)]), {order, descending}),
			Cursor = qlc:cursor(SortedHandle),
            N = 100,
			PagedHandle = qlc:next_answers(Cursor, N),
			qlc:delete_cursor(Cursor),
			RatingsList = [#ratingInfo{userID=X#user.id,
							 displayName=X#user.name,
							 rating=X#user.rating} || X <- PagedHandle],
							 
			{RatingsList2, _} = lists:foldl(
					fun(Rating, {List, Index}) ->
							{[Rating#ratingInfo{position=Index} | List], Index + 1}
					end, {[], 1}, RatingsList),
			RatingsList3 = lists:reverse(RatingsList2),
            LastRating = if length(RatingsList3) >= N -> 
                (lists:last(RatingsList3))#ratingInfo.rating;
            true ->
                0
            end,
            mnesia:write(City#city{minRating=LastRating}),
            RatingsList3
		end
	 ),
	Ratings.

deleteCar_nt(CarID)->
  Car = mneser:getRecord_nt(car, CarID),
  mneser:deleteRecordsList_nt(item,2,Car#car.upgrades),
  mneser:deleteRecords_nt(car, 2, CarID).

delete(Id) ->
    {atomic, ok} = mnesia:transaction(fun() -> 
        delete_nt(Id)
    end).
    
delete_nt(UserID) ->
    UserDetails = mneser:getRecord_nt(userDetails, UserID),
    mneser:deleteRecordsList_nt(item, 2, UserDetails#userDetails.inventory),
    lists:foreach(fun deleteCar_nt/1, UserDetails#userDetails.cars),
    mneser:deleteRecords_nt(user, 2, UserID),
    mneser:deleteRecords_nt(message, 3, UserID),
    mneser:deleteRecords_nt(message, 6, UserID),
    mneser:deleteRecords_nt(postMessage, 3, UserID),
    mneser:deleteRecords_nt(postMessage, 4, UserID),
    mneser:deleteRecords_nt(userDetails, 2, UserID),
    mneser:deleteRecords_nt(userProgress, 2, UserID).
    
resetUser_nt(User) ->
    UserID = User#user.id,
    UserDetails = mneser:getRecord_nt(userDetails, UserID),
    mneser:deleteRecordsList_nt(item, 2, UserDetails#userDetails.inventory),
    lists:foreach(fun deleteCar_nt/1, UserDetails#userDetails.cars),
    mneser:deleteRecords_nt(message, 3, UserID),
    mneser:deleteRecords_nt(message, 6, UserID),
    mneser:deleteRecords_nt(postMessage, 3, UserID),
    mneser:deleteRecords_nt(postMessage, 4, UserID),
    
    Car = registration:createCar_nt(random:uniform(3), [], random:uniform(8), 5),
    mnesia:write(Car),
    mnesia:write(#userDetails{id=UserID,
                              inventory = [],
                              cars = [Car#car.id]}),
    NewUser = User#user{currentCarID = 
                        Car#car.id, 
                        level=1, experience=0, roles=[], 
                        triggers=[register, {showClickOnCarTip, 3}, {showHowToDriveTip, 3}, {showNitroTip, 3}, {tutorialStage, 1}],
                        rating=1000, money=1000, duelWin=0, duelCount=0},
                        
    UserProgress = dbUserProgress:getOrCreate_nt(UserID),    
    mnesia:write(#userProgress{userID=UserID, invites=UserProgress#userProgress.invites, activeInvites=UserProgress#userProgress.activeInvites, onlineTime=UserProgress#userProgress.onlineTime, kilometers=UserProgress#userProgress.kilometers, worksCounter=UserProgress#userProgress.worksCounter}),
	dbActivity:register_nt(UserID, {restartGame}, ok),
    mnesia:write(NewUser).

getVkontakteId(Id) ->
	(mneser:getRecord(user, Id))#user.vkontakteID.

canChat(UserId)->
    Now = utils:now(),
    Result = mneser:do
			(
			qlc:q([X ||
                 X <- mnesia:table(stopList),
                 X#stopList.id =:= UserId,
                 X#stopList.time + X#stopList.period > Now
                ])
       ),
     Result == [].

getAllIds() ->
	mneser:do(qlc:keysort(1, qlc:q([
		{ U#user.id, U#user.vkontakteID }
		||
		U <- mnesia:table(user)
	]), {order ,descending})).
	
setMoney(Id, Value) ->
    mnesia:transaction(fun() -> 
        User = getRecord_nt(id, Id),
        case erlang:is_record(User, user) of 
            true ->
                mnesia:write(User#user{money=Value});
            false ->
                ok
        end
    end).

addMoney(UserID, Value) ->
    mnesia:transaction(fun() -> 
        addMoney_nt(UserID, Value)
    end).

addMoney_nt(UserID, Value) ->
    case mnesia:wread({user, UserID}) of 
        [User] ->
            mnesia:write(User#user{money=User#user.money + Value});
        [] ->
            ok
    end.

setRealMoney(Id, Value) ->
    mnesia:transaction(fun() -> 
        User = getRecord_nt(id, Id),
        case erlang:is_record(User, user) of 
            true ->
                mnesia:write(User#user{realMoney=Value});
            false ->
                ok
        end
    end).
    

addRealMoney_nt(Id, Value) ->
    case mnesia:wread({user, Id}) of 
        [User] ->
            mnesia:write(User#user{realMoney=User#user.realMoney + Value});
        [] ->
            ok
    end.

addRealMoney(Id, Value) ->
    mnesia:transaction(fun() -> 
        addRealMoney_nt(Id, Value)
    end).
    
changeNickname(Id, Nickname) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        UserOld = getRecord_nt(id, Id),
        if erlang:is_record(UserOld, user) ->
            UserNew = getRecord_nt(nicknameIgnoreCase, Nickname),
            if erlang:is_record(UserNew, user) ->
                {error, userExists};
            true ->
                mnesia:write(UserOld#user{name=Nickname})
            end;
        true ->
            {error, notFound}
        end
    end),
    
    Result.
    
spendUserMoney_nt(User, Price, PriceGold, NotEnoughMoneyMessage) ->
    NewUserMoney = User#user.money - Price,
    NewUserRealMoney = User#user.realMoney - PriceGold,
    if NewUserMoney < 0 orelse NewUserRealMoney < 0 ->
        mnesia:abort({error, NotEnoughMoneyMessage});
    true -> ok end,
    mnesia:write(User#user{money=NewUserMoney, realMoney=NewUserRealMoney}).

startTransferToCity(UserID, TransitionType, CitySrcID, CityDstID) ->    
    Result = mnesia:transaction(fun() -> 
        User = getRecord_nt(id, UserID),

        TransferMinLevel = dbGlobal:get_nt(transferMinLevel),
        if User#user.level < TransferMinLevel ->
            mnesia:abort({error, "[[youNeedLevelXForWar, " ++ integer_to_list(TransferMinLevel) ++ "]]"});
        true -> ok end,

        if erlang:is_record(User, user) ->
            if CityDstID =:= User#user.homeCity orelse CityDstID =:= 3 ->
                {Arrival, Price, PriceGold} = case TransitionType of
                    train ->
                        {utils:now() + dbGlobal:get_nt(transferTimeTrain)*1000, dbGlobal:get_nt(transferPriceTrain), 0};
                    plane ->
                        {utils:now() + dbGlobal:get_nt(transferTimePlane)*1000, 0, dbGlobal:get_nt(transferPricePlane)}
                end,

                UserTransferState = #userTransferState{type=TransitionType, citySrc=CityDstID, cityDst=CityDstID, arrivalTime=Arrival},
                UserState = #userState{userID=UserID, state=UserTransferState},
                spendUserMoney_nt(User, Price, PriceGold, "[[notEnoughMoney]]"),
                mnesia:write(UserState),
                dbActivity:register(UserID, {startTransfer, TransitionType, CitySrcID, CityDstID}, ok),
                UserState;
            true ->
                {error, {wrongDestination, "wrongDestination"}}
            end;
        true ->
            {error, {userNotFound, "User not found"}}
        end
    end),    
    
    mneser:transactionResult(Result, UserID, {startTransfer, TransitionType, CitySrcID, CityDstID}).

getUserState(UserID) ->
    {atomic, UserState_} = mnesia:transaction(fun() -> 
        case mnesia:read({userState, UserID}) of
            [UserState] ->
                if erlang:is_record(UserState#userState.state, userTransferState) ->
                    UserTransferState = UserState#userState.state,
                    Left = UserTransferState#userTransferState.arrivalTime - utils:now(),
                    if Left > 0 ->
                        UserState;
                    true ->
                        User = getRecord_nt(id, UserID),
                        if erlang:is_record(User, user) ->
                            mnesia:write(User#user{currentCity=UserTransferState#userTransferState.cityDst});
                        true -> ok end,
                        
                        mnesia:delete({userState, UserID}),                        
                        #userState{}
                    end;
                true -> 
                    UserState 
                end;
            _Other ->
                #userState{}
        end
    end),
    UserState_.

canWash(Time) ->
    Time < users:last5clock().

cleanCar(UserID, FriendID) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        WashReward = dbGlobal:get_nt(washCarMoney),
        case qlc:e(qlc:q([X || X <- mnesia:table(friendAction), X#friendAction.userID =:= UserID, X#friendAction.friendID =:= FriendID])) of
            [] ->
                mnesia:write(#friendAction{userID=UserID, friendID=FriendID, washTime=utils:now()}),
                addMoney_nt(UserID, WashReward),
                {ok, WashReward};
            [OldFriendAction] ->
                case canWash(OldFriendAction#friendAction.washTime) of true ->
                    mnesia:delete_object(OldFriendAction),
                    mnesia:write(OldFriendAction#friendAction{washTime=utils:now()}),
                    addMoney_nt(UserID, WashReward),
                    {ok, WashReward};
                false ->
                    {error, {aleadyWashed, "[[Машина уже чистая]]"}}
                end                     
        end        
    end),
    Result.
    
isFriendCarClean(UserID, FriendID) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
        case qlc:e(qlc:q([X || X <- mnesia:table(friendAction), X#friendAction.userID =:= UserID, X#friendAction.friendID =:= FriendID])) of
            [FriendAction] ->
                not canWash(FriendAction#friendAction.washTime);            
            _Other ->
                false
        end
    end),
    Result.

getRegisteredUserIDs(From, To) ->
    {atomic, UserIDs} = mnesia:transaction(fun() -> 
        qlc:e(qlc:q([U#user.id || U <- mnesia:table(user), utils:erlangTime(U#user.date) >= From, utils:erlangTime(U#user.date) < To]))
    end),
    UserIDs.
