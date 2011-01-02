-module(carUpgrades).
-include("data.hrl").
-include("config.hrl").
-export([getUpgradeInfo/2,
         getUpgradesList/1,
         wearOutAllUpgrades_nt/2,
         transformMessage/2]).

getUpgradeInfo(UserID, ClientID) ->
    UpgradeInfo2 = case dbItem:getUserInventoryAndEquipment(UserID) of
        {_Inventory, Equipment} ->
            lists:foldl(
            fun(Item, UpgradeInfo) ->
              ItemClass = dbItem:getClass(Item#item.itemClassID),
              D = case (Item#item.durability == 0 orelse Item#item.durabilityMax < dbGlobal:get(durabilityJunkThreshold)) of true -> 0; false -> 1 end,
              #upgradeInfo{
                userID = ClientID,                
                speed = UpgradeInfo#upgradeInfo.speed + ItemClass#itemClass.speed * D,
                power = UpgradeInfo#upgradeInfo.power + ItemClass#itemClass.power * D,
                controllability = UpgradeInfo#upgradeInfo.controllability + ItemClass#itemClass.controllability * D,
                braking = UpgradeInfo#upgradeInfo.braking + ItemClass#itemClass.braking * D}
            end,
            (dbCar:getCurrentUserCarUpgrade(UserID))#upgradeInfo{userID = ClientID}
                ,Equipment);
        _Other ->
            #upgradeInfo{userID = ClientID}
    end,

    MaxValue = (mneser:getRecord(global,upgradeMaxValue))#global.value,
    MaxTimes = (mneser:getRecord(global,upgradeMaxTimes))#global.value,
    UpgradeInfo2#upgradeInfo{
                speed = normalizeParameter( UpgradeInfo2#upgradeInfo.speed,MaxValue, MaxTimes) ,
                power = normalizeParameter( UpgradeInfo2#upgradeInfo.power,MaxValue, MaxTimes) ,
                controllability = normalizeParameter( UpgradeInfo2#upgradeInfo.controllability,MaxValue, MaxTimes) ,
                braking = normalizeParameter(UpgradeInfo2#upgradeInfo.braking,MaxValue, MaxTimes)}.

normalizeParameter(Param, MaxValue, MaxTimes) ->
    if (Param < 0 ) -> 0;
       (Param > MaxValue) -> MaxTimes;
       true -> 1 + ( (MaxTimes * Param) / MaxValue) end.

%% todo
%% it may be faster, if we would make only one request to data base
%% now we create one request per client
getUpgradesList(Users) ->
   UsersLst = dict:to_list(Users),
   lists:map(
		fun({_ClientID,{_Pid, ClientInfo}}) ->
			getUpgradeInfo(ClientInfo#clientInfo.userID, ClientInfo#clientInfo.clientID)
		end,
		UsersLst).

%% this function should be wrapped by transaction
%% inside function we do not use transaction
wearOutAllUpgrades_nt(LobbyResults, LobbyInfo) ->
    Fun = fun(DepreciationCoef, TotalRaceLength) ->
        DepreciationCoef * TotalRaceLength / 1000.0
    end,
    TotalRaceLength = LobbyInfo#lobbyInfo.lapNumber * (mneser:getRecord(route, LobbyInfo#lobbyInfo.routeID))#route.length,
    wearOutAllUpgrades_nt(LobbyResults, TotalRaceLength, Fun).

transformMessage( {lobbyRaceResults, LobbyResultsWithRating, Upg, WearOut, BlueScore, RedScore}, UserID) ->
    Fun = fun (WearInfo) -> UserID =:= WearInfo#wearInfo.userID end,
    {lobbyRaceResults, LobbyResultsWithRating, Upg, lists:filter(Fun,WearOut), BlueScore, RedScore}.

%% decrease durability
%% Users - lists of int, each atom is ID in user table
%% Fun(DepreciationCoef, TotalLength) -> float()
wearOutAllUpgrades_nt(LobbyResults, TotalRaceLength, Fun) ->
    lists:foldl(
        fun(LobbyResult,Lst1) ->
           UserID = LobbyResult#lobbyResult.userID,
           case dbItem:getUserInventoryAndEquipment_nt(UserID) of
              {_Inventory, Equipment} ->
                 {Wears, NewEquipmentList} = lists:foldl(
                 fun(Item, {WearsList, EqList}) ->
                     ItemClass = dbItem:getClass_nt(Item#item.itemClassID),
                     DeltaDurability =  Fun(dbGlobal:get_nt(depreciationCoef), TotalRaceLength),
                     NewDurability = utils:max(0, (Item#item.durability - DeltaDurability)),
                     WearInfo = #wearInfo{
                                  id = Item#item.id,
                                  userID = UserID,
                                  displayName = ItemClass#itemClass.name,
                                  delta = DeltaDurability,
                                  durability = NewDurability,
                                  durabilityMax = Item#item.durabilityMax},
                     dbItem:updateItem_nt(Item#item{durability=NewDurability}),
                     {WearsList ++ [WearInfo], EqList ++ [Item#item.id]}
                 end, {[],[]}, Equipment),
                 dbItem:updateEquipment_nt(UserID, NewEquipmentList),
                 Lst1++Wears;
               _other ->
                   Lst1
           end
         % {[WearInfo],[newEquipmentList] }
        end,[], LobbyResults).
