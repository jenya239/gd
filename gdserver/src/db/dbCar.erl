-module(dbCar).

-compile(export_all).

-include("data.hrl").
-include("config.hrl").

-include_lib("stdlib/include/qlc.hrl").
-include("lib/eunit/include/eunit.hrl").

-export([
	getCarNameByID/1,
	getCars/1,
	getShopInfo/0,
	getUserCars/1,
	setCar/2,
	buy/3,
	give_nt/3,
	give/3,
	sell/2,
	getCurrentUserCarUpgrade/1,
	wearOutCars_nt/2,
	checkCarCondition/1,
	repairCar_nt/1,
	capitalRepairCar/2,
	getClass/1,
	getCar/1,
	getRecolorInfo/2,
	buySlot/1,
	getCarsWithMinimumLevel/1,
	removeBrokenCars/1,
	giveFuel/0
]).

getCurrentUserCarUpgrade(UserID) ->
  User = dbUser:getRecord(id,UserID),
  Car = mneser:getRecord(car, User#user.currentCarID),
  CarClass = mneser:getRecord(carClass, Car#car.carClassID),
  #upgradeInfo{speed = CarClass#carClass.speed,
               power = CarClass#carClass.power,
               controllability = CarClass#carClass.controllability,
               braking = CarClass#carClass.breaking}.

getCarNameByID(CarID) ->
   try
        Car = mneser:getRecord(car, CarID),
        CarClass = mneser:getRecord(carClass, Car#car.carClassID),
        CarClass#carClass.fileName
    catch error:_ ->
        "focus"
    end.

getClass(ClassID) ->
	mneser:getRecord(carClass, ClassID).

getCars(Level) ->
	mneser:do(qlc:sort(qlc:q([genCarbyCarClass(CC) ||
                             CC <- mnesia:table(carClass),
                             CC#carClass.minLevel =< Level]))).

genCarbyCarClass(CarClass) ->
    Car = #car{ id=-1,
          carClassID=CarClass#carClass.id,
          durability=CarClass#carClass.durabilityMax,
          durabilityMax=CarClass#carClass.durabilityMax,
          upgrades = [],
          fuel = CarClass#carClass.fuelCapacity,
          color = random:uniform(8),
          nitroCount = 5
         },
     SellPrice = 0,     
     #carInfo{car = Car,
              carClass = CarClass,
              sellPrice = SellPrice,
              carUpgrade =  #upgradeInfo{speed = CarClass#carClass.speedView,
			               				 power = CarClass#carClass.powerView,
			               				 controllability = CarClass#carClass.controllabilityView,
			               				 braking = CarClass#carClass.breakingView}}. % calcCarParams(Car,CarClass)}.

getShopInfo() ->
	{atomic, ShopInfo} =
        mnesia:transaction(
        fun() ->
            Q = qlc:keysort(#carClass.minLevel,
                qlc:q([ CC ||CC <- mnesia:table(carClass),
                       CC#carClass.available]),
                {order, ascending}
            ),
            qlc:e(qlc:q([ genCarbyCarClass(CC) || CC <- Q]))
        end),
	ShopInfo.

give_nt(UserID, CarClassID, Color) ->
  if (is_integer(Color)) -> ok;
      true -> mnesia:abort("[[invalidColor]]")
  end,
  UserDetailsOld = dbUser:getDetails_nt(UserID),
  UserDetails =
  if length(UserDetailsOld#userDetails.cars) >= UserDetailsOld#userDetails.carSlots ->
            UserDetailsOld#userDetails{carSlots = length(UserDetailsOld#userDetails.cars)+1};
        true -> UserDetailsOld
  end,
  User = dbUser:getRecord_nt(id, UserID),
  _CarClass = mneser:getRecord_nt(carClass,CarClassID),
  NewCar = registration:createCar_nt(CarClassID, [], Color, 5),
  NewUserDetails = UserDetails#userDetails{cars=UserDetails#userDetails.cars ++ [NewCar#car.id]},
  mnesia:write(NewCar),
  mnesia:write(User#user{currentCarID=NewCar#car.id }),
  mnesia:write(NewUserDetails),
  dbActivity:register_nt(UserID, {giveCar, CarClassID}, ok).

give(UserID, CarClassID, Color) ->
  mnesia:transaction(fun() ->
        give_nt(UserID, CarClassID, Color)
    end).

buy(UserID, CarClassID, Color) ->
	Result = try
    mnesia:transaction(fun() ->
        UserDetails = dbUser:getDetails_nt(UserID),

        if length(UserDetails#userDetails.cars) >= UserDetails#userDetails.carSlots ->
            mnesia:abort({noCarSlot, "[[noCarSlot]]"});
        true -> ok end,

        User = dbUser:getRecord_nt(id, UserID),
        CarClass = mneser:getRecord_nt(carClass,CarClassID),

        if CarClass#carClass.count < 1 ->
            mnesia:abort( { error, "[[notEnoughCars]]" } );
        true -> ok end,

        if User#user.level < CarClass#carClass.minLevel ->
            mnesia:abort({error, "[[levelToLow]]"});
        true -> ok end,

        CarPrice = CarClass#carClass.price,
        CarRealPrice = CarClass#carClass.realPrice,
        OldUserRealMoney = User#user.realMoney,
        OldUserMoney = User#user.money,
        {NewUserMoney, NewUserRealMoney} =
            if CarRealPrice > 0 ->
                {OldUserMoney, OldUserRealMoney - CarRealPrice};
            true ->
                {OldUserMoney-CarPrice, OldUserRealMoney}
            end,

        if NewUserMoney < 0 orelse NewUserRealMoney < 0 ->
            mnesia:abort({error, "[[notEnoughMoney]]"});
        true ->
            ok
        end,


        NewCar = registration:createCar_nt(CarClassID, [], Color, 5),
        NewUserDetails = UserDetails#userDetails{cars=UserDetails#userDetails.cars ++ [NewCar#car.id]},
        mnesia:write( CarClass#carClass{ count=CarClass#carClass.count-1 } ),
        mnesia:write(NewCar),
        mnesia:write(User#user{money=NewUserMoney, realMoney=NewUserRealMoney, currentCarID=NewCar#car.id }),
        mnesia:write(NewUserDetails),
        dbActivity:register_nt(UserID, {buyCar, CarClassID, CarPrice, CarRealPrice, OldUserMoney, OldUserRealMoney, NewUserMoney, NewUserRealMoney}, ok)
    end)

    catch throw: Throw ->
         Throw
    end,

	mneser:transactionResult(Result, UserID, {buyCar, CarClassID}).
	
buySlot(UserID) ->
	Result = try
    mnesia:transaction(fun() ->
        UserDetails = dbUser:getDetails_nt(UserID),

        MaxSlots = dbGlobal:get_nt(maxCarSlots),
        if UserDetails#userDetails.carSlots >= MaxSlots ->
            mnesia:abort({noCarSlot, "[[cantBuyMoreSlots]]"});
        true -> ok end,

        User = dbUser:getRecord_nt(id, UserID),

				NSlots = UserDetails#userDetails.carSlots,
				Mult = if NSlots =< 7 -> NSlots - 2; true -> 5 end,
        SlotPrice = dbGlobal:get_nt(carSlotCost) * Mult,
        OldUserRealMoney = User#user.realMoney,
        
        NewUserRealMoney = OldUserRealMoney - SlotPrice,

        if NewUserRealMoney < 0 ->
            mnesia:abort({error, "[[notEnoughMoney]]"});
        true ->
            ok
        end,

        NewUserDetails = UserDetails#userDetails{carSlots=NSlots + 1},
        mnesia:write(User#user{realMoney=NewUserRealMoney}),
        mnesia:write(NewUserDetails),
        dbActivity:register_nt(UserID, {buyCarSlot, SlotPrice, OldUserRealMoney, NewUserRealMoney}, ok)
    end)

    catch throw: Throw ->
         Throw
    end,

	mneser:transactionResult(Result, UserID, {buyCarSlot}).

getCar(CarID)->
  Car = mneser:getRecord(car, CarID),
  Fun = fun (ItemID) ->
          dbItem:getItem(ItemID)
        end,
  Car#car{upgrades = lists:filter( fun (A) -> not ( A =:= notFound ) end,
                                   lists:map(Fun, Car#car.upgrades))}.

calcCarParams(Car2,CarClass) -> %видимые!! реальные считаются в carUpgrades.getUpgradeInfo
    %да, вот бля такая жизнь... кто писал этот код???
  UpgradeInfo2 = lists:foldl(
        fun(Item, UpgradeInfo) ->
          ItemClass = dbItem:getClass(Item#item.itemClassID),

          #upgradeInfo{
            speed = UpgradeInfo#upgradeInfo.speed + ItemClass#itemClass.speed, %А ЕСЛИ ДЕТАЛЬ СЛОМАНА???? (первоначальный отсев таких деталей не проводится (см getCar (dbItem:getItem вызывается для всех существующих(!)))
            power = UpgradeInfo#upgradeInfo.power + ItemClass#itemClass.power,
            controllability = UpgradeInfo#upgradeInfo.controllability + ItemClass#itemClass.controllability,
            braking = UpgradeInfo#upgradeInfo.braking + ItemClass#itemClass.braking}
        end,
        #upgradeInfo{userID = 0}, Car2#car.upgrades),
        #upgradeInfo{speed = CarClass#carClass.speedView + UpgradeInfo2#upgradeInfo.speed / 5,
           power = CarClass#carClass.powerView + UpgradeInfo2#upgradeInfo.power / 5,
           controllability = CarClass#carClass.controllabilityView + UpgradeInfo2#upgradeInfo.controllability / 10,
           braking = CarClass#carClass.breakingView + UpgradeInfo2#upgradeInfo.braking / 2}.

getUserCars(UserID) ->
  UserDetails = mneser:getRecord(userDetails,UserID),
  Cars = UserDetails#userDetails.cars,
  FunFilter =
      fun(CarID) ->
          Car = mneser:getRecord(car, CarID),
          case mnesia:transaction(fun() -> mneser:getRecord_nt(carClass, Car#car.carClassID) end) of
            {atomic, CarClass} -> CarClass#carClass.minLevel =:= 1;
            {aborted, _} -> false
          end
      end,
  FirstLevelCars = lists:filter(FunFilter, Cars),
  FirstLevelCarsCount = length(FirstLevelCars),
  Fun = fun(CarID) ->
    Car = getCar(CarID),
    case mnesia:transaction(fun() -> mneser:getRecord_nt(carClass, Car#car.carClassID) end) of
      {atomic, CarClass} -> 
        SellPrice2 = 
          case (FirstLevelCarsCount =:= 1 andalso CarClass#carClass.minLevel =:= 1) of
            true -> -1;
            _Other -> calcSellPrice(Car,CarClass)
          end,
        Info = #carInfo{
          car=Car,
          carClass=CarClass,
          sellPrice=SellPrice2,
          repairPrice = calcFullRepairPrice(CarClass,Car),
          capitalRepairPrice = calcCapitalRepairPrice(CarClass,Car),
          carUpgrade = calcCarParams(Car,CarClass),
          recolor=getRecolor(CarClass#carClass.id)
        },
        {true, Info};
      {aborted, _} -> false
    end
  end,
  lists:foldr( fun(Elem, Acc) -> %filtermap
    case Fun(Elem) of
      false -> Acc;
      true -> [Elem|Acc];
      {true,Value} -> [Value|Acc]
    end
  end, [], Cars ).

getRecolor(CarClassID) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        qlc:e( qlc:q( [ X || X <- mnesia:table(recolorPrice),
                       X#recolorPrice.carClassID =:= CarClassID
                      ] ) )
    end),
    Result.

checkHasCar(UserID,CarID)->
    {atomic, Result} =
        mnesia:transaction(
        fun() ->
             checkHasCar_nt(UserID,CarID)
        end),
    Result.

checkHasCar_nt(UserID,CarID)->
      UserDetails = mneser:getRecord_nt(userDetails,UserID),
      Cars = UserDetails#userDetails.cars,
      lists:any(
          fun (Element) ->
              Element == CarID
	        end,
      Cars).


setCar(UserID,CarID) ->
    User = mneser:getRecord(user,UserID),
    mneser:writeRecord(User#user{currentCarID = CarID}),
    case checkHasCar(UserID,CarID) of
        true ->            
            mneser:writeRecord(User#user{currentCarID = CarID});
        false ->
            ok
    end.

delete_nt(CarID) ->
    mnesia:delete(car, CarID, write).

delete(CarID) ->
    mnesia:transaction(fun() -> delete_nt(CarID) end).

upgradesSellPrice_nt(UpgradeIDList) ->
  Fun = fun (Item,Acc) ->
          case mnesia:read({itemClass, Item#item.itemClassID}) of
            [ItemClass] -> Acc + dbItem:sellPrice_nt(Item,ItemClass);
            [] -> Acc
          end
        end,
  lists:foldl(Fun, 0,
    lists:filter(fun (A) -> not ( A =:= notFound ) end,
      lists:map(fun dbItem:getItem/1 , UpgradeIDList) ) ).


calcSellPrice_nt(Car,CarClass) ->
    Price = if CarClass#carClass.price > 0 ->
                CarClass#carClass.price;
            true ->
                CarClass#carClass.realPrice * dbGlobal:get_nt(exchangeRate)
            end,
    Coef = (0.2 + (0.3 * (Car#car.durabilityMax / CarClass#carClass.durabilityMax))),
    % do not remove next line!!!
    % Car in param is not record in the db.
    CarClear = mneser:getRecord_nt(car, Car#car.id),
    Price * Coef + upgradesSellPrice_nt(CarClear#car.upgrades).

calcSellPrice(Car,CarClass) ->
   {atomic, Result} = mnesia:transaction(fun() ->
        calcSellPrice_nt(Car,CarClass)
    end),
    Result.


sell(UserID,CarID)->
    Result = try
        mnesia:transaction(
            fun() ->
                  User = mneser:getRecord_nt(user,UserID),
                  UserDetails = mneser:getRecord_nt(userDetails,UserID),
                  Car = mneser:getRecord_nt(car, CarID),
                  CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
                  Price = calcSellPrice_nt(Car,CarClass),
                  NewMoney = User#user.money + Price,
                  Cars = UserDetails#userDetails.cars,
                  NewCars = lists:delete(CarID, Cars),
                  CurrentCarID =
                      case (CarID =:= User#user.currentCarID) of
                          true ->
                             lists:nth(1,NewCars);
                          false ->
                             User#user.currentCarID
                      end,
                  NewUser = User#user{money = NewMoney,currentCarID = CurrentCarID},
                  mnesia:write(NewUser),
                  mnesia:write(UserDetails#userDetails{cars = NewCars}),
                  lists:foreach(fun dbItem:deleteItem_nt/1, Car#car.upgrades),
                  delete_nt(CarID),
                  dbActivity:register_nt(UserID, {sellCar, Car#car.carClassID, Price, User#user.money, NewUser#user.money}, ok)
          end)
        catch throw: Throw ->
            Throw
        end,

	mneser:transactionResult(Result, UserID, {sellCar, CarID}).

wearOutCars_nt(LobbyResults, LobbyInfo) ->
    Fun = fun(DepreciationCoef, TotalRaceLength) ->
        DepreciationCoef * TotalRaceLength / 1000.0
    end,
    TotalRaceLength = LobbyInfo#lobbyInfo.lapNumber * (mneser:getRecord(route, LobbyInfo#lobbyInfo.routeID))#route.length,
    wearOutCars_nt(LobbyResults, TotalRaceLength, Fun).

wearOutCars_nt(LobbyResults, TotalRaceLength, Fun) ->
    lists:foreach(
        fun(LobbyResult) ->
           UserID = LobbyResult#lobbyResult.userID,
           User = mneser:getRecord_nt(user,UserID),
           Car = mneser:getRecord_nt(car,User#user.currentCarID),
           CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
           DeltaDurability =  Fun(CarClass#carClass.durabilityCoef, TotalRaceLength), %
           NewDurability = utils:max(0, (Car#car.durability - DeltaDurability)),
           mnesia:write(Car#car{durability=NewDurability})
        end, LobbyResults).

checkCarCondition(UserID) ->
    User = mneser:getRecord(user,UserID),
    Car = mneser:getRecord(car,User#user.currentCarID),
    not (Car#car.durability =< 0 orelse
         Car#car.durabilityMax < dbGlobal:get(durabilityJunkThreshold) ).

% CarID -> RepairPrice
repairCar_nt(_CarID) ->
    0.
    %Car = mneser:getRecord_nt(car,CarID),
    %CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
    %NewDurabilityMax = Car#car.durabilityMax -
    %    (Car#car.durabilityMax - Car#car.durability)*dbGlobal:get_nt(durabilityRedCoef),
    %mnesia:write(Car#car{durability=NewDurabilityMax, durabilityMax=NewDurabilityMax}),
    %calcRepairPrice(CarClass, Car).

calcFullRepairPrice(CarClass,Car) ->
    lists:foldl(
       fun(Item,PriceIn) ->
               ItemClass = mneser:getRecord(itemClass, Item#item.itemClassID),
               PriceIn + dbItem:calcRepairPrice(ItemClass,Item)
       end,
       calcRepairPrice(CarClass, Car), Car#car.upgrades).

calcCapitalRepairPrice(CarClass, Car) ->
   Price = if CarClass#carClass.price > 0 ->
                CarClass#carClass.price/dbGlobal:get(exchangeRate);
            true ->
                CarClass#carClass.realPrice
            end,
    Coef  = 0.7 * (CarClass#carClass.durabilityMax - Car#car.durability)/CarClass#carClass.durabilityMax,
    Price*Coef.

calcCapitalRepairPrice_nt(CarClass, Car) ->
   Price = if CarClass#carClass.price > 0 ->
                CarClass#carClass.price/dbGlobal:get_nt(exchangeRate);
            true ->
                CarClass#carClass.realPrice
            end,
    Coef  = 0.7 * (CarClass#carClass.durabilityMax - Car#car.durability)/CarClass#carClass.durabilityMax,
    Price*Coef.

capitalRepairCar(UserID,CarID) ->
    Result = mnesia:transaction(
        fun() ->
            capitalRepairCar_nt(UserID, CarID)
        end),
	mneser:transactionResult(Result, UserID, {repairCar, CarID}).

capitalRepairCar_nt(UserID,CarID) ->
    User = mneser:getRecord_nt(user, UserID),
    Car = mneser:getRecord_nt(car, CarID),
    CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
    Price = calcCapitalRepairPrice_nt(CarClass, Car),
    RealMoney = User#user.realMoney,
    case checkHasCar_nt(UserID,CarID) of 
        false ->
            mnesia:abort({error, "[[cantRepairCarYouDontOwn]]"});
        true -> ok
    end,
    if RealMoney < Price ->
        mnesia:abort({error, "[[notEnoughMoney]]"});
      true ->
        ok
    end,
    NewRealMoney = RealMoney - Price,
    mnesia:write(User#user{realMoney = NewRealMoney}),
    mnesia:write(Car#car{durabilityMax = CarClass#carClass.durabilityMax,
                         durability =  CarClass#carClass.durabilityMax}),
    dbActivity:register_nt(User#user.id, {repairCar, CarID, RealMoney, NewRealMoney}, ok).

calcRepairPrice(_CarClass, _Car) ->
	0. %CarClass#carClass.repairPrice * (Car#car.durabilityMax - Car#car.durability).

repaint(UserID,CarID,Color) ->
    Result = mnesia:transaction(
        fun() ->
            repaint_nt(UserID,CarID,Color)
        end),
	mneser:transactionResult(Result, UserID, {repairCar, CarID}).

repaint_nt(UserID,CarID,Color) ->
  case checkHasCar_nt(UserID,CarID) of
    false ->
        mnesia:abort({error, "[[cantRecolorCarYouDontOwn]]"});
    true -> ok
  end,
  User = mneser:getRecord_nt(user, UserID),
  Car = mneser:getRecord_nt(car, CarID),
  CarClass = mneser:getRecord_nt(carClass, Car#car.carClassID),
  RecolorPrice = getRecolorPrice_nt(CarClass#carClass.id,Color),
  NewUserMoney = User#user.money - RecolorPrice#recolorPrice.price,
  NewUserRealMoney = User#user.realMoney - RecolorPrice#recolorPrice.realPrice,
  if NewUserMoney < 0 orelse NewUserRealMoney < 0 ->
      mnesia:abort({error, "[[notEnoughMoney]]"});
     true ->
        ok
  end,
  mnesia:write(Car#car{color = Color}),
  mnesia:write(User#user{money = NewUserMoney, realMoney = NewUserRealMoney}),
  dbActivity:register_nt(User#user.id, {repaintCar, CarID, User#user.realMoney, NewUserRealMoney ,User#user.money, NewUserMoney}, ok).

getRecolorPrice_nt(CarClassID, Color) ->
  Res = qlc:e(qlc:q( [R || R <- mnesia:table(recolorPrice),
                R#recolorPrice.carClassID =:= CarClassID,
                R#recolorPrice.color =:= Color]  )),
  case Res of
     [Rep] -> Rep;
     _Other -> mnesia:abort({error, "[[databaseError]]"})
  end.

getRecolorPrice(CarClassID, Color) ->
	{atomic, Result} = mnesia:transaction(fun() -> getRecolorPrice_nt(CarClassID, Color) end),
	Result.

getRecolorInfo(CarClassID, Color) ->
	CarClass = getClass(CarClassID),
	RecolorPrice = getRecolorPrice(CarClassID, Color),
	#recolorInfo{carDisplayName=CarClass#carClass.displayName, colorID=Color, price=RecolorPrice#recolorPrice.price, realPrice=RecolorPrice#recolorPrice.realPrice}.
	
getCarsWithMinimumLevel(Level) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
        qlc:e(qlc:q([genCarbyCarClass(CC) || CC <- mnesia:table(carClass), CC#carClass.minLevel =:= Level andalso CC#carClass.available =:= true]))
    end),
    
    Result.

giveFuel() ->    
    {atomic,ArrayFuel} = mnesia:transaction(fun() ->
         mnesia:foldl( fun(Record,Arr) ->
        array:set(Record#carClass.id, Record#carClass.fuelCapacity, Arr ) end,
        array:new([{size,50},{fixed,true}]),
        carClass)
    end),
    Fun = fun(Car) -> Car#car{ fuel = array:get(Car#car.id, ArrayFuel) } end,
    {atomic,ok} = mnesia:transform_table(car, Fun, record_info(fields, car), car).

removeBrokenCars(UserID) ->
	UserDetails = mneser:getRecord(userDetails, UserID),
	Cars = UserDetails#userDetails.cars,
	FunFilter =
	    fun(CarID) ->
	        Car = mnesia:dirty_read({car, CarID}),
			case Car of
				[_Element] ->
					true;
				_ ->
					false
			end
	    end,
	FilteredCars = lists:filter(FunFilter, Cars),
	NewUserDetails = UserDetails#userDetails{cars=FilteredCars},
	mnesia:dirty_write(NewUserDetails).	