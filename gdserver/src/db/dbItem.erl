-module(dbItem).
-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([
        getItem_nt/1,
        getItem/1,
        updateItem_nt/1,
        updateItem/1,
        deleteItem_nt/1,
        deleteItem/1,
        getUserInventoryAndEquipment_nt/1,
        getUserInventoryAndEquipment/1,
		getShopInfo/1,
		findAndRemove/2,
		findAndUpdate/3,
		findAndReplaceInTheSameSlot_nt/2,
		updateEquipment_nt/2,
		repairEquipment/2,
		useCarUpgrade/2,
		removeCarUpgrade/2,
        getClass/1,
		getClass_nt/1,
		buy/2,
		sell/2,
		delete/2,
		replaceClassWithID/1,
		send/6,
        calcRepairPrice/2,
        sellPrice_nt/2, 
        getUpgradesWithMinimumLevel/1
]).

getItem_nt(ItemID) ->
    Result = qlc:e(qlc:q([I || I <- mnesia:table(item), I#item.id =:= ItemID])),
    case Result of
        [Item] ->
            Item;
        _notFound ->
            notFound
    end.
    
getItem(ItemID) ->
    {atomic, Item} = mnesia:transaction(fun() -> getItem_nt(ItemID) end),
	Item.

updateItem_nt(Item) ->
    mnesia:write(Item).

updateItem(Item) ->
    mnesia:transaction(fun() -> updateItem_nt(Item) end).

deleteItem_nt(ItemID) ->
    mnesia:delete(item, ItemID, write).

deleteItem(Item) ->
    mnesia:transaction(fun() -> deleteItem_nt(Item) end).

getUserInventoryAndEquipment_nt(UserID) ->
	Result = qlc:e(qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.id =:= UserID])),

	case Result of
		[UserDetails] ->
			Fun =
    			fun(ItemID, ListIn) ->
                    case getItem_nt(ItemID) of
                        notFound -> ListIn;
                        Item -> [Item | ListIn]
                    end
				end,

            User = mneser:getRecord_nt(user, UserID),
            Car = mneser:getRecord_nt(car, User#user.currentCarID),
            Equipment = lists:foldl(Fun, [], Car#car.upgrades),
            Inventory = lists:foldl(Fun, [], UserDetails#userDetails.inventory),
			{Inventory, Equipment};
		[] ->
			{[], []}
	end.

getUserInventoryAndEquipment(UserID) ->
	{atomic, Result} = mnesia:transaction(
        fun() ->
            getUserInventoryAndEquipment_nt(UserID)
        end),
	Result.

getShopInfo(UserID) ->
	{atomic, ShopInfo} =
        mnesia:transaction(
        fun() ->
            UserLevel = (dbUser:getRecord_nt(id, UserID))#user.level,
            Q = qlc:keysort(#itemClass.id,
                qlc:q([ IC ||IC <- mnesia:table(itemClass), (IC#itemClass.maxLevel =:= 0 orelse IC#itemClass.maxLevel >= UserLevel) andalso IC#itemClass.available]), {order, ascending}
            ),
            qlc:e(qlc:q([#item{id=-1, itemClassID=IC#itemClass.id, durability=IC#itemClass.durabilityMax, durabilityMax=IC#itemClass.durabilityMax} || IC <- Q]))
        end),
	ShopInfo.

findAndRemove(ItemID, ItemList) ->
	case lists:keytake(ItemID, 1, ItemList) of
		{value, Item, NewItemList} ->
			{NewItemList, [Item]};
		false ->
			{ItemList, []}
	end.

findAndUpdate(ItemID, Fun, ItemList) ->
	case lists:keysearch(ItemID, #item.id, ItemList) of
		{value, Item} ->
			{NewItem, AnyValue} = Fun(Item),
			{ok, lists:keyreplace(ItemID, #item.id, ItemList, NewItem), AnyValue};
		false ->
			false
	end.
    
findAndReplaceInTheSameSlot_nt(Upg, EquipList) ->
	UpgItemClass = getClass_nt(Upg#item.itemClassID),

	{NewEquipList, ReplacedItem} = lists:foldl(
        fun(ID, {ListIn, Item}) ->
            E = dbItem:getItem_nt(ID),
            ItemClass = dbItem:getClass_nt(E#item.itemClassID),
            if ItemClass#itemClass.slot =:= UpgItemClass#itemClass.slot ->
                {ListIn, E};
            true ->
                {ListIn ++ [ID], Item}
            end
        end, {[], undefined}, EquipList),

	{NewEquipList ++ [Upg#item.id], ReplacedItem}.

repairEquipment_nt(UserID, ItemID) ->
	User = dbUser:getRecord_nt(id, UserID),

    RepairSingleItemFun = fun(Item) ->
        ItemClass = dbItem:getClass_nt(Item#item.itemClassID),
        NewDurabilityMax = Item#item.durabilityMax - (Item#item.durabilityMax - Item#item.durability)*dbGlobal:get_nt(durabilityRedCoef),
        mnesia:write(Item#item{durability=NewDurabilityMax, durabilityMax=NewDurabilityMax}),
        calcRepairPrice(ItemClass, Item)
    end,

	{OldInventoryExt, OldEquipmentExt} = dbItem:getUserInventoryAndEquipment_nt(UserID),
	Price =
        if ItemID =:= -1 ->
            lists:foldl(
                fun(Item, PriceIn) ->
                    RepairSingleItemFun(Item) + PriceIn
            end, dbCar:repairCar_nt(User#user.currentCarID), OldEquipmentExt);
        true ->
            case lists:keysearch(ItemID, #item.id, OldInventoryExt) of
                false ->
                    case lists:keysearch(ItemID, #item.id, OldEquipmentExt) of
                        false ->
                            0;
                        {value, Item} ->
                            RepairSingleItemFun(Item)
                    end;
                {value, Item} ->
                    RepairSingleItemFun(Item)
            end                    
        end,

	if Price > User#user.money ->
			mnesia:abort({error, "[[notEnoughMoney]]"});
		true ->
			ok
	end,
    
	OldMoney = User#user.money,
	NewMoney = OldMoney - Price,
	mnesia:write(User#user{money=NewMoney}),
	dbActivity:register_nt(User#user.id, {repair, ItemID, OldMoney, NewMoney}, ok).

repairEquipment(UserID, ItemID) ->
	Result = mnesia:transaction(
        fun() ->
            repairEquipment_nt(UserID, ItemID)
        end),

	mneser:transactionResult(Result, UserID, {repair, ItemID}).

updateEquipment_nt(UserID, Equipment) ->
  User = mneser:getRecord_nt(user, UserID),
  Car = mneser:getRecord_nt(car, User#user.currentCarID),
  mnesia:write(Car#car{upgrades = Equipment}).

switchCarUpgrade(UserID, UpgID, Action) ->
	Result = mnesia:transaction(fun() ->
			switchCarUpgrade_nt(UserID, UpgID, Action)
	end),
		
	case Result of
		{atomic, ok} ->
			ok;
		{aborted, {error, Message}} ->
			{error, {database, Message}};
		{error, {_Reason, _Message}} = Error ->
			Error;
		OtherResult ->
			log:write(error, ?MODULE_STRING, "~p~n", [OtherResult]),
			{error, {database, "[[databaseError]]"}}
	end.
	
switchCarUpgrade_nt(UserID, UpgID, Action) ->
	User = dbUser:getRecord_nt(id, UserID),
	Result = qlc:e(qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.id =:= UserID])),
	Car = mneser:getRecord_nt(car, User#user.currentCarID),
	case Result of
	    [UserDetails] ->
	        List =
	            case Action of
	                use ->
	                    UserDetails#userDetails.inventory;
	                _Else ->
	                    Car#car.upgrades
	            end,

	        Upg = dbItem:getItem_nt(UpgID),
	        NewList = lists:delete(UpgID, List),                            

	        ItemClass = dbItem:getClass_nt(Upg#item.itemClassID),

	        if User#user.level < ItemClass#itemClass.minLevel ->
	            mnesia:abort({error, "[[levelTooLow]]"});
	        true -> ok end,
    
	        {Inventory,NewCar} =
	            case Action of
	                use ->
	                    case Upg#item.durabilityMax < dbGlobal:get_nt(durabilityJunkThreshold) of
	                        true ->
	                           mnesia:abort({error, "[[cantUseBrokenItem]]"});
	                        false -> ok
	                    end,
                    
	                    if (ItemClass#itemClass.usingType =:= "inventory") ->
	                        NewCar1 = Car#car{nitroCount = Car#car.nitroCount + ItemClass#itemClass.usingCount},
	                        {NewList,NewCar1};
	                    true ->
	                        {NewEquipList, ReplacedItem} = findAndReplaceInTheSameSlot_nt(Upg, Car#car.upgrades),
	                        NewInventory = if ReplacedItem =/= undefined ->
	                            NewList ++ [ReplacedItem#item.id];
	                        true ->
	                            NewList
	                        end,
	                        {NewInventory, Car#car{upgrades=NewEquipList}}
	                    end;
	                remove ->
	                    {UserDetails#userDetails.inventory ++ [Upg#item.id], Car#car{upgrades=NewList} };
	                _Other ->
	                    UserDetails
	            end,
	        mnesia:write(UserDetails#userDetails{inventory = Inventory}),
	        mnesia:write(NewCar);
	    _OtherAgain -> ok
	end.
	
useCarUpgrade(UserID, UpgID) ->
	switchCarUpgrade(UserID, UpgID, use).

removeCarUpgrade(UserID, UpgID) ->
	switchCarUpgrade(UserID, UpgID, remove).

calcRepairPrice(ItemClass, Item) ->
	ItemClass#itemClass.repairPrice * (Item#item.durabilityMax - Item#item.durability).

replaceClassWithID(List) ->
	lists:map(
        fun({ID, ItemClass, Durability}) ->
            {ID, ItemClass#itemClass.id, Durability}
        end, List).

getClass(ItemClassID) ->
    {atomic, Result} = mnesia:transaction(fun() -> getClass_nt(ItemClassID) end),
    Result.

getClass_nt(ItemClassID) ->
	Result = qlc:e(qlc:q([I || I <- mnesia:table(itemClass), I#itemClass.id =:= ItemClassID])),
	case Result of
		[ItemClass] ->
			ItemClass;
		_Other ->
			throw({error, {noItemClass, "[[itemDoesNotExist]]"}})
	end.

buy(UserID, ItemClassID) ->
	Result = try
    mnesia:transaction(fun() ->
        UserDetails = dbUser:getDetails_nt(UserID),
        InventorySize = dbGlobal:get_nt(inventorySize),
        
        if length(UserDetails#userDetails.inventory) >= InventorySize ->
            mnesia:abort({error, "[[inventoryIsFull]]"});
        true -> ok end,

        User = dbUser:getRecord_nt(id, UserID),
        ItemClass = getClass_nt(ItemClassID),

        if User#user.level < ItemClass#itemClass.minLevel ->
            mnesia:abort({error, "[[levelTooLow]]"});
        true -> ok end,

        ItemPrice = ItemClass#itemClass.price,
        ItemRealPrice = ItemClass#itemClass.realPrice,
        OldUserRealMoney = User#user.realMoney,
        OldUserMoney = User#user.money,
        {NewUserMoney, NewUserRealMoney} =
            if ItemRealPrice > 0 ->
                {OldUserMoney, OldUserRealMoney-ItemRealPrice};
            true ->
                {OldUserMoney-ItemPrice, OldUserRealMoney}
            end,

        if NewUserMoney < 0 orelse NewUserRealMoney < 0 ->
            mnesia:abort({error, "[[notEnoughMoney]]"});
        true ->
            ok
        end,

        mnesia:write(User#user{money=NewUserMoney, realMoney=NewUserRealMoney}),
        NewItem = #item{id=dbUuid:get_nt(item), itemClassID=ItemClassID, durability=ItemClass#itemClass.durabilityMax, durabilityMax=ItemClass#itemClass.durabilityMax},
        NewUserDetails = UserDetails#userDetails{inventory=UserDetails#userDetails.inventory ++ [NewItem#item.id]},
        mnesia:write(NewItem),
        mnesia:write(NewUserDetails),

		switchCarUpgrade_nt(UserID, NewItem#item.id, use),
		
        dbActivity:register_nt(UserID, {buyItem, ItemClassID, ItemPrice, ItemRealPrice, OldUserMoney, OldUserRealMoney, NewUserMoney, NewUserRealMoney}, ok)
    end)

    catch throw: Throw ->
         Throw
    end,
	
	mneser:transactionResult(Result, UserID, {buyItem, ItemClassID}).

sellPrice_nt(Item,ItemClass) ->
    Price = if ItemClass#itemClass.price > 0 ->
        ItemClass#itemClass.price;
    true ->
        ItemClass#itemClass.realPrice * dbGlobal:get_nt(exchangeRate)
    end,

    SellRateMax = dbGlobal:get_nt(sellItemRate),
    SellRateMin = dbGlobal:get_nt(junkConversionCoef),

    DurabilityMin = dbGlobal:get_nt(durabilityJunkThreshold),
    DurabilityMax = ItemClass#itemClass.durabilityMax,
    DurabilityCur = if Item#item.durabilityMax<DurabilityMin -> DurabilityMin; true->Item#item.durabilityMax end,

    SellRate = SellRateMin+(SellRateMax-SellRateMin)*(DurabilityCur-DurabilityMin)/(DurabilityMax-DurabilityMin),
    Price * SellRate.

trySell_nt(ItemID, List) ->
	case lists:member(ItemID, List) of
        true ->
            Item = dbItem:getItem_nt(ItemID),
            NewList = lists:delete(ItemID, List),
            ItemClass = getClass_nt(Item#item.itemClassID),
            dbItem:deleteItem_nt(ItemID),

           
            {NewList, sellPrice_nt(Item,ItemClass) , ItemClass#itemClass.id};
        false ->
            notFound
	end.

sell(UserID, ItemID) ->
	Result = try
        mnesia:transaction(
             fun() ->
                 UserDetails = dbUser:getDetails_nt(UserID),
                 User = dbUser:getRecord_nt(id, UserID),
                 Car = mneser:getRecord_nt(car, User#user.currentCarID),
                 OldUserMoney = User#user.money,

                 {NewInventory, NewEquipment, ItemPrice, ItemClassID} =
                     case trySell_nt(ItemID, UserDetails#userDetails.inventory) of
                         {NewInventory_, ItemPrice_, ItemClassID_} ->
                             {NewInventory_, Car#car.upgrades, ItemPrice_, ItemClassID_};
                         notFound ->
                             case trySell_nt(ItemID, Car#car.upgrades) of
                                 {NewEquipment_, ItemPrice_, ItemClassID_} ->
                                     {UserDetails#userDetails.inventory, NewEquipment_, ItemPrice_, ItemClassID_};
                                 notFound ->
                                     mnesia:abort({error, "[[databaseError]]"})
                             end
                     end,

                 NewUserMoney = User#user.money + ItemPrice,
                 mnesia:write(User#user{money=NewUserMoney}),
                 mnesia:write(UserDetails#userDetails{inventory=NewInventory}),
                 mnesia:write(Car#car{upgrades=NewEquipment}),
                 dbActivity:register_nt(UserID, {sellItem, ItemClassID, ItemPrice, OldUserMoney, NewUserMoney}, ok)
             end)
        catch throw: Throw ->
            Throw
        end,

	mneser:transactionResult(Result, UserID, {sellItem, ItemID}).

tryDelete_nt(ItemID, List) ->
	case lists:member(ItemID, List) of
        true ->
            Item = dbItem:getItem_nt(ItemID),            
            ItemClass = getClass_nt(Item#item.itemClassID),
            NewList = lists:delete(ItemID, List),
            dbItem:deleteItem_nt(ItemID),
            {NewList, ItemClass#itemClass.id};
        false ->
            notFound
	end.

delete(UserID, ItemID) ->
	Result = try
         mnesia:transaction(
             fun() ->
                 UserDetails = dbUser:getDetails_nt(UserID),
                 User = dbUser:getRecord_nt(id, UserID),
                 Car = mneser:getRecord_nt(car, User#user.currentCarID),
                 {NewInventory, NewEquipment, ItemClassID} =
                     case tryDelete_nt(ItemID, UserDetails#userDetails.inventory) of
                         {NewInventory_, ItemClassID_} ->
                             {NewInventory_, Car#car.upgrades, ItemClassID_};
                         notFound ->
                             case tryDelete_nt(ItemID, Car#car.upgrades) of
                                 {NewEquipment_, ItemClassID_} ->
                                     {UserDetails#userDetails.inventory, NewEquipment_, ItemClassID_};
                                 notFound ->
                                     mnesia:abort({error, "[[databaseError]]"})
                             end
                     end,

                 mnesia:write(UserDetails#userDetails{inventory=NewInventory}),
                 mnesia:write(Car#car{upgrades = NewEquipment}),
                 dbActivity:register_nt(UserID, {deleteItem, ItemID, ItemClassID}, ok)
             end
                                             )
     catch throw: Throw ->
             Throw
     end,

	mneser:transactionResult(Result, UserID, {deleteItem, ItemID}).

send(UserID, ItemID, Money, SellPrice, Comment, RecepientNick) ->
	Result = try
        mnesia:transaction(
             fun() ->
                 User = dbUser:getRecord_nt(id, UserID),
                 if User#user.level < 3 ->
                     mnesia:abort({error, "[[cantSendMailBeforeLevelX, 3]]"});
                 true -> ok end,
             
                 UserDetails = dbUser:getDetails_nt(UserID),
                 Item = dbItem:getItem_nt(ItemID),
                 NewInventory = lists:delete(ItemID, UserDetails#userDetails.inventory),                 

                 if Item =:= notFound andalso Money =< 0 ->
                     mnesia:abort({error, "[[cantSendEmptyMail]]"});
                 true ->
                     ok
                 end,

                 ItemCommission =
                     if Item =/= notFound ->
                         (getClass_nt(Item#item.itemClassID))#itemClass.price * dbGlobal:get_nt(sendItemRate);
                     true ->
                         0.0
                     end,

                 MoneyCommission = Money * dbGlobal:get_nt(sendMoneyRate),

                 OldUserMoney = User#user.money,
                 NewUserMoney =
                     if Money > 0 ->
                         utils:trunc(OldUserMoney - Money - MoneyCommission - ItemCommission, 2);
                     true ->
                         utils:trunc(OldUserMoney - ItemCommission, 2)
                     end,

                 if NewUserMoney < 0 ->
                     mnesia:abort({error, "[[notEnoughMoney]]"});
                 true -> ok end,

                 mnesia:write(User#user{money=NewUserMoney}),
                 mnesia:write(UserDetails#userDetails{inventory=NewInventory}),

                 Recepient = case dbUser:getRecord_nt(nickname, RecepientNick) of
                     {error, noNickname, _} ->
                         mnesia:abort({error, "[[cantFindUserByNickname]]"});
                     RecepientResult ->
                         RecepientResult
                 end,

                 ItemIDList = if ItemID > 0 -> [ItemID]; true -> [] end,                     

                 mnesia:write(#postMessage{id=dbUuid:get_nt(postMessage), senderID=User#user.id, recepientID=Recepient#user.id, timeStamp=utils:now(), item=ItemIDList, money=Money, sellPrice=SellPrice, comment=Comment}),
                 dbActivity:register_nt(User#user.id, {sendItem, Recepient#user.id, Money, Comment, OldUserMoney, NewUserMoney, MoneyCommission, ItemID}, ok)
            end)         
    catch throw: Throw ->
         Throw
    end,

	mneser:transactionResult(Result, UserID, {sendItem, ItemID, Money, Comment, RecepientNick}).
	
getUpgradesWithMinimumLevel(Level) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
        qlc:e(qlc:q([I || I <- mnesia:table(itemClass), I#itemClass.minLevel =:= Level andalso I#itemClass.available =:= true]))
    end),
    
    Result.
