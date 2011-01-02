-module(fuel).

-include("data.hrl").

-export([calcFuelConsumption/3,
         writeOffFuel/2,
         checkFuel/2,
         getGasInfo/1,
         buyFuel/2,
         startWork/1,
         finishWork/2,
         cancelWork/2]).

-include("lib/eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

getGasInfo(UserID) ->
    User = mneser:getRecord(user, UserID),
    FuelCost = dbGlobal:get({fuelCost, User#user.currentCity}),
    CarID = User#user.currentCarID,
    Car =  mneser:getRecord(car, CarID),
    CarClass =  mneser:getRecord(carClass, Car#car.carClassID),
    MaxFuelCount = getMaxFuelCount(User,Car, CarClass, FuelCost,0),
    #gasInfo{fuelPrice = FuelCost, maxFuelCount = MaxFuelCount, jobOffers=mneser:getWorkOffers()}.

getMaxFuelCount(User,Car, CarClass, FuelCost, N) ->
    MoneyMaxFuelCount = User#user.money/FuelCost,
    CarMaxFuelCount = CarClass#carClass.fuelCapacity + N - Car#car.fuel,
    utils:max(0,utils:min(CarMaxFuelCount,MoneyMaxFuelCount)).

buyFuel(UserID, FuelCount) ->
    {atomic, Val} = mnesia:transaction(fun() -> buyFuel_nt(UserID,FuelCount) end),
    Val.

buyFuel_nt(UserID, FuelCount) ->
    User = mneser:getRecord_nt(user, UserID),
    CarID = User#user.currentCarID,
    Car =  mneser:getRecord_nt(car, CarID),
    CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
    FuelCost = dbGlobal:get_nt({fuelCost,User#user.currentCity}),
    case (FuelCount =<  getMaxFuelCount(User,Car, CarClass, FuelCost, 1) )
         and (FuelCount >= 0) of
        true ->
          FuelCount2 = utils:min(CarClass#carClass.fuelCapacity - Car#car.fuel, FuelCount),
          Price = FuelCost * FuelCount2, %(FuelCapacity - Fuel),
          OldMoney = User#user.money,
          NewMoney = OldMoney - Price,
          OldFuel = Car#car.fuel,
          NewFuel = OldFuel + FuelCount2,
          NewUser = User#user{money = NewMoney},
          mnesia:write(NewUser),
          mnesia:write(Car#car{fuel = NewFuel}),
          dbActivity:register_nt(UserID, {buyFuel, Price, OldFuel, NewFuel, OldMoney, NewMoney}, ok),
          ok;
        _False ->
          error
    end.

calcFuelConsumption(RouteID, LapCount, CarID) ->    
    Length = (mneser:getRecord(route, RouteID))#route.length / 1000,
    Car = mneser:getRecord(car, CarID),
    ConsumptionRate = (mneser:getRecord(carClass, Car#car.carClassID))#carClass.fuelConsumption,
    (Length * LapCount * ConsumptionRate).

writeOffFuel(LobbyResults, LobbyInfo) ->
    RouteID = LobbyInfo#lobbyInfo.routeID,
    LapCount = LobbyInfo#lobbyInfo.lapNumber,
    Fun =
     fun(LobbyResult) ->
         UserID = LobbyResult#lobbyResult.userID,
         if is_number(UserID) -> % for test. userid can be undefined only in tests.
             User = mneser:getRecord_nt(user, UserID),
             CarID = User#user.currentCarID,
             Car = mneser:getRecord_nt(car, CarID),
             FuelConsumption = calcFuelConsumption(RouteID,LapCount,CarID),
             OldFuel = Car#car.fuel,
             NewFuel = utils:max(0, OldFuel - FuelConsumption),
             mnesia:write(Car#car{fuel = NewFuel}),
             dbActivity:register_nt(UserID, {consumeFuel, OldFuel, NewFuel}, ok),
             LobbyResult#lobbyResult{fuel = NewFuel};
         true ->
            LobbyResult#lobbyResult{fuel = 40}
        end
     end,
    lists:map(Fun,LobbyResults).

checkFuel(LobbyInfo, ClientInfo) ->
    RouteID = LobbyInfo#lobbyInfo.routeID,
    LapCount = LobbyInfo#lobbyInfo.lapNumber,
    UserID = ClientInfo#clientInfo.userID,
    if UserID =/= undefined -> % for tests. userid can be undefined only in tests.
        User = mneser:getRecord(user, UserID),
        CarID = User#user.currentCarID,
        Car = mneser:getRecord(car, User#user.currentCarID),
        FuelConsumption = calcFuelConsumption(RouteID,LapCount,CarID),
        Car#car.fuel > FuelConsumption;
    true ->
        true
    end.

startWork(WorkID) ->
    WorkOffer = mneser:getRecord(workOffer, WorkID),
    case timer:send_after(WorkOffer#workOffer.time, {finishWork,timerEvent}) of
        {ok, TRef} ->
            StartTime = utils:now(),
            WorkInfo = #workInfo{offerID = WorkOffer#workOffer.id, startTime = StartTime ,timerRef=TRef},
            {ok, WorkOffer#workOffer.time, WorkOffer#workOffer.message, WorkInfo};
        {error,_Reason} ->
            {error, error, "You were unable to start work"}
    end.

% always return {ok,Message}
finishWork(UserID, WorkInfo) ->
    WorkOffer = mneser:getRecord(workOffer, WorkInfo#workInfo.offerID),
    {atomic, Result} = mnesia:transaction(
        fun() ->
           {ok,considerWorkResult_nt(UserID, WorkOffer, 1.0)}
        end
    ),
    Result.

% always return {ok,Message}
cancelWork(UserID, WorkInfo) ->
    WorkOffer = mneser:getRecord(workOffer, WorkInfo#workInfo.offerID),
    timer:cancel(WorkInfo#workInfo.timerRef),
    Now = utils:now(),
    case (Now - WorkInfo#workInfo.startTime)*2 < WorkOffer#workOffer.time of
        true ->
            {ok, "[[youHaveNotGainedFuel]]"};
        _Other ->
            {atomic, Result} = mnesia:transaction(
            fun() ->
               {ok,considerWorkResult_nt(UserID, WorkOffer, 0.2)}
            end
            ),
            Result
    end.

considerWorkResult_nt(UserID, WorkOffer, Coef) ->
    User = dbUser:getRecord_nt(id,UserID),
    CarID = User#user.currentCarID,
    Car = mneser:getRecord_nt(car, CarID),
    CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
    FuelCapacity = CarClass#carClass.fuelCapacity,
    NewFuel = utils:min(Car#car.fuel + Coef * WorkOffer#workOffer.fuel,
                        FuelCapacity),
    OldFuel = Car#car.fuel,
    %log:write("delta fuel is ~w~n",[NewFuel - OldFuel]),
    mnesia:write(Car#car{fuel = NewFuel}),
    serverSocket:sendMessageToClientByUserID(UserID, incWorksCounter),
    dbActivity:register_nt(UserID, {workFuel, OldFuel, NewFuel}, ok),
    lists:flatten(io_lib:format("[[youHaveGainedXFuel, ~w]]", [NewFuel - OldFuel])).


