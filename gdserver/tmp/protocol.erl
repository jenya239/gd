%% Author: demid
%% Created: 29.08.2008
%% Description: Модуль занимается разбором (десериализацией) входящих сообщений, полученных
%%              в виде набора байт из входящего соединения, и кодированием (сериализацией) 
%%              сообщений в обратно в последовательнось байт для отправки. Все в соответствии 
%%              с нашим протоколом

-module(protocol).

-export([serialize/1, 
        deserialize/1,
        deserializeXml/1,
        generateXml/1,
        item_to_xml/1,
        triggerInfo_to_xml/1,
        roleInfo_to_xml/1] ).
        
-include_lib("xmerl/include/xmerl.hrl").
-include("data.hrl").
-include("lib/eunit/include/eunit.hrl").

% первый уровень разбора: 0x80000000 - xml, 0x40000000 - binary
deserialize([0, 0, 0, 128 | Xml])->
    deserializeXml(Xml);
    
deserialize([0, 0, 0, 64 | Data])->
    {carState, Data}.

% второй уровень разбора, парсим xml
deserializeXml(Xml) ->
    ObjectModel = xmerl_scan:string(Xml, [{xmlbase, ""}]),
    parse(parseXmerl(ObjectModel)).

parseXmerl({{xmlElement,event,event,[],
             {xmlNamespace,[],[]},
             [],1,
             AttributesList,
             _TagsList,[],[],undeclared}, []}) ->

    lists:map(fun(XmerlAttribute) ->
                {element(2, XmerlAttribute), utils:unicode_to_utf8_escaped(element(9, XmerlAttribute))}
              end, AttributesList).

list_to_atom2(Str) ->
    case catch erlang:list_to_existing_atom(Str) of
        Atom when is_atom(Atom) ->
            Atom;
        _NotAtom ->
            list_to_atom(Str)
    end.

parse([{name, Name}]) ->
    {list_to_atom2(Name)};

parse([{name, "get"}, {property, PropertyName}]) ->
    {get, list_to_atom2(PropertyName)};

parse([{name, "readiness"}, {value, Value}]) ->
    {readiness, list_to_atom2(Value)};

parse([{name, "send_pass"}, {login, Login}]) ->
    {send_pass, Login};
    
parse([{name, "quitLobby"}, {reason, Reason}]) ->
    {quitLobby, Reason};

parse([{name,"useCarUpgrade"}, {id, ID}]) ->
    {useCarUpgrade, list_to_integer(ID)};
    
parse([{name,"removeCarUpgrade"}, {id, ID}]) ->
    {removeCarUpgrade, list_to_integer(ID)};
    
parse([{name, "repair"}, {itemID, ItemID}]) ->
    {repair, list_to_integer(ItemID)};

parse([{name, "repairCar"}, {carID, CarID}]) ->
    {repairCar, list_to_integer(CarID)};

parse([{name,"buyItem"}, {itemClassID, ItemClassID}]) ->
    {buyItem, list_to_integer(ItemClassID)};

parse([{name,"setCar"}, {carID, CarID}]) ->
    {setCar, list_to_integer(CarID)};

parse([{name,"buyCar"}, {carClassID, CarClassID}, {color, Color}, {needSlot, NeedSlot}]) ->
    {buyCar, list_to_integer(CarClassID), list_to_integer(Color), list_to_atom2(NeedSlot)};

parse([{name,"sellItem"}, {itemID, ItemID}]) ->
    {sellItem, list_to_integer(ItemID)};

parse([{name,"sellCar"}, {carID, CarID}]) ->
    {sellCar, list_to_integer(CarID)};

parse([{name,"repaintCar"}, {carID, CarID}, {color, Color}]) ->
    {repaintCar, list_to_integer(CarID), list_to_integer(Color)};

parse([{name,"exchangeMoney"}, {realMoney, RealMoney}]) ->
    {exchangeMoney, list_to_integer(RealMoney)};

parse([{name,"exchangeVkontakteVotes"}, {votes, VkontakteVotes}]) ->
    {exchangeVkontakteVotes, list_to_integer(VkontakteVotes)};

parse([{name, "joinLobby"}, {lobbyID, LobbyID}]) ->
    {joinLobby, list_to_integer(LobbyID)};

parse([{name, "startWork"}, {workID, WorkID}]) ->
    {startWork, list_to_integer(WorkID)};

parse([{name, "endTransfer"}]) ->
    {endTransfer};
    
parse([{name, "startTransfer"}, {transitionType, TransitionType}, {citySrcID, CitySrcID}, {cityDstID, CityDstID}]) ->
    {startTransfer, list_to_atom2(TransitionType), list_to_integer(CitySrcID), list_to_integer(CityDstID)};
    
parse([{name, "levelUpPrize"}, {level, Level}, {type, Type}]) ->
    {levelUpPrize, list_to_integer(Level), list_to_atom2(Type)};
    
parse([{name, "levelUpRewards"}, {level, Level}]) ->
    {levelUpRewards, list_to_integer(Level)};

parse([{name, "levelUpInfo"}, {level, Level}]) ->
    {levelUpInfo, list_to_integer(Level)};

parse([{name, "buyFuel"}, {fuelCount, FuelCount}]) ->
    {buyFuel, list_to_integer(FuelCount)};
    
parse([{name, "deleteItem"}, {itemID, ItemID}]) ->
    {deleteItem, list_to_integer(ItemID)};

parse([{name,"processPostMessage"}, {id, PostMessageID}, {action, Action}]) ->
    {processPostMessage, list_to_integer(PostMessageID), list_to_atom2(Action)};

parse([{name,"sendItem"}, {itemID, ItemID}, {money, Money}, {sellPrice, SellPrice}, {comment, Comment}, {recepientNick, RecepientNick}]) ->
    {sendItem, list_to_integer(ItemID), list_to_integer(Money), list_to_integer(SellPrice), Comment, RecepientNick};

parse([{name, "authorizeVkontakte"}, {vkontakteID, VkontakteID}, {authKey, AuthKey}, {firstLastName, FirstLastName}, {vkontakteOwnerID, VkontakteOwnerID}]) -> 
    {authorizeVkontakte, list_to_integer(VkontakteID), AuthKey, FirstLastName, list_to_integer(VkontakteOwnerID)};

parse([{name, "register"}, {nickname, Nickname}, {car, Car}, {color, Color}, {city, City}]) ->
    {register, Nickname, list_to_integer(Car), list_to_integer(Color), list_to_integer(City)};

parse([{name, "get"}, {property, "lobby"}, {lobbyID, LobbyID}]) ->
    {get, lobby, list_to_integer(LobbyID)};
    
parse([{name, "get"}, {property, "secureUserInfo"}, {userID, UserID}]) ->
    {get, secureUserInfo, list_to_integer(UserID)};

parse([{name, "get"}, {property, "friendInfos"}, {friendIDs, FriendIDs}]) ->
    IDStrings = string:tokens(FriendIDs, ","),
    IDs = lists:map(fun(IDString) -> 
        list_to_integer(IDString)
    end, IDStrings),
    {get, friendInfos, IDs};

parse([{name, "ban"}, {id, ID}, {time, Time}]) ->
    {ban, list_to_integer(ID), list_to_integer(Time)};
    
parse([{name, "changeTrigger"}, {trigger, Trigger}, {delta, Delta}]) ->
    {changeTrigger, list_to_atom2(Trigger), list_to_integer(Delta)};

parse([{name, "set"}, {property, "currentCarID"}, {value, Value}]) ->
    {set, currentCarID, list_to_integer(Value)};

parse([{name, "lapTime"}, {channel, Channel}, {type, Type}, {time, Time}]) ->
    {lapTime, list_to_atom2(Channel), Type, list_to_integer(Time)};

parse([{name, "chatMessage"}, {channel, Channel}, {text, Text}, {nick, Nick}]) ->
    {chatMessage, list_to_atom2(Channel), Text, Nick};
    
parse([{name, "chatSystemMessage"}, {text, Text}]) ->
    {chatSystemMessage, Text};

parse([{name, "createLobby"}, {routeID, RouteID}, {lapNumber, LapNumber}, {direction, Direction}, {playerMax, PlayerMax}, {allowedCarID, AllowedCarID}, {timerLength, TimerLength}, {league,League}, {stake,Stake}]) ->
    {createLobby, list_to_integer(RouteID), list_to_integer(LapNumber), list_to_atom2(Direction), list_to_integer(PlayerMax), list_to_integer(AllowedCarID), list_to_integer(TimerLength), list_to_integer(League), list_to_integer(Stake) };
    
parse([{name, "cleanCar"}, {friendID, FriendID}]) ->
    {cleanCar, list_to_integer(FriendID)};    

parse([{name, "loadingProgress"}, {channel, Channel}, {progress, Progress}]) ->
    {loadingProgress, list_to_atom2(Channel), list_to_integer(Progress)}.

% кодирование бинарных сообщений
serialize({otherCarState, OtherCarState, _Time, OtherClientID}) ->
    [0, 0, 0, 64 | binary_to_list(<<OtherClientID:32/integer>>)] ++ OtherCarState;

% кодирование xml сообщений
serialize(Event) ->
    XmlString = generateXml(Event),
    [0, 0, 0, 128 | XmlString].

generateReasonXml(Name, Result, {Reason, Message}) ->
    "<event name='" ++ atom_to_list(Name) ++ "'" 
    ++  " result='" ++ atom_to_list(Result) ++ "'" 
    ++  " reason='" ++ atom_to_list(Reason) ++ "'" 
    ++ " message='" ++ Message ++ "' />".

generateReasonXml(Name, Property, Result, {Reason, Message}, {Attribute, Value}) ->
    "<event name='" ++ atom_to_list(Name) ++ "'" 
    ++  " property='" ++ atom_to_list(Property) ++ "'" 
    ++  " result='" ++ atom_to_list(Result) ++ "'" 
    ++  " reason='" ++ atom_to_list(Reason) ++ "'" 
    ++  " message='" ++ Message ++ "'"
    ++  " " ++ Attribute ++ "=" ++ Value ++ "' />";

generateReasonXml(Name, Property, Result, {Reason, Message}, Value) ->
    "<event name='" ++ atom_to_list(Name) ++ "'" 
    ++  " property='" ++ atom_to_list(Property) ++ "'" 
    ++  " result='" ++ atom_to_list(Result) ++ "'" 
    ++  " reason='" ++ atom_to_list(Reason) ++ "'" 
    ++  " message='" ++ Message ++ "'"
    ++  " result='" ++ Value ++ "' />".

generateReasonXml(Name, Property, Result, {Reason, Message}) ->
    "<event name='" ++ atom_to_list(Name) ++ "'" 
    ++  " property='" ++ atom_to_list(Property) ++ "'" 
    ++  " result='" ++ atom_to_list(Result) ++ "'" 
    ++  " reason='" ++ atom_to_list(Reason) ++ "'" 
    ++  " message='" ++ Message ++ "' />".

generateXml({serverTime, TimeInMiliseconds, Adv}) ->
    lists:flatten(io_lib:format("<event name='serverTime' time='~w' adv='~s'/>", [TimeInMiliseconds, Adv]));

generateXml({Name, error, {Reason, Message}}) ->
    MessageEscaped = utils:escapeText(Message),
    generateReasonXml(Name, error, {Reason, MessageEscaped});

generateXml({startWork, ok, Time, Message}) ->
     lists:flatten(io_lib:format("<event name='startWork' result='ok' time='~w' message='~s' />", [Time,Message]));


generateXml({finishWork, ok, GasInfo, UserInfo}) ->
    lists:flatten(io_lib:format("<event name='finishWork'>~s~s</event>",
                                [gasInfo_to_xml(GasInfo),
                                 userInfo_to_xml(UserInfo)]));

generateXml({cancelWork, ok, GasInfo, UserInfo}) ->
     lists:flatten(io_lib:format("<event name='cancelWork' result='ok'>~s~s</event>",
                                 [gasInfo_to_xml(GasInfo),
                                 userInfo_to_xml(UserInfo)]));

generateXml({skipTutorial, ok, UserInfo}) ->
     lists:flatten(io_lib:format("<event name='skipTutorial' result='ok'>~s</event>",
                                 [userInfo_to_xml(UserInfo)]));

generateXml({join, ok, RouteID, Direction, LapNumber, CarID}) ->
    lists:flatten(io_lib:format("<event name='join' result='ok' routeID='~b' direction='~w' lapNumber='~b' carID='~b' />", [RouteID, Direction, LapNumber, CarID]));

generateXml({quickConnect, ok, RouteID, Direction, LapNumber, UserInfo}) ->
    UserInfoXml = userInfo_to_xml(UserInfo),
    lists:flatten(io_lib:format("<event name='quickConnect' result='ok' routeID='~b' direction='~w' lapNumber='~b'>", [RouteID, Direction, LapNumber]))
    ++ UserInfoXml
    ++ "</event>";

generateXml({startTransfer, ok, UserState, UserInfoWithDetails}) ->
    lists:flatten(io_lib:format("<event name='startTransfer' result='ok'>~s~s</event>", [userState_to_xml(UserState), userInfo_to_xml(UserInfoWithDetails)]));
    
generateXml({endTransfer, ok, CityDst}) ->
    lists:flatten(io_lib:format("<event name='endTransfer' result='ok' cityDstID='~w' />", [CityDst]));
    
generateXml({endTransfer, error, TransitionType, ArrivalTime}) ->
    lists:flatten(io_lib:format("<event name='endTransfer' result='error' transitionType='~w' arrivalTime='~w' />", [TransitionType, ArrivalTime]));
    
generateXml({disconnect, ok}) ->
    "<event name='disconnect' result='ok'/>";

generateXml({restartGame, ok}) ->
    "<event name='restartGame' result='ok'/>";
    
generateXml({levelUpInfo, ok, Message, NewUpgrades, NewRoutes, NewCars}) ->
    lists:flatten(io_lib:format("<event name='levelUpInfo' result='ok' message='~s'>~s~s~s</event>", 
        [Message,
         createXmlList("newUpgrades", fun itemClass_to_xml/1, NewUpgrades),
         createXmlList("newCars", fun car_to_xml/1, NewCars),
         createXmlList("newRoutes", fun route_to_string/1, NewRoutes)]));

generateXml({levelUpRewards, ok, RealMoney, Money, ItemName, NitroCount}) ->
    lists:flatten(io_lib:format("<event name='levelUpRewards' result='ok' realMoney='~w' money='~w' itemName='~s' nitroCount='~w' />", 
        [RealMoney, Money, ItemName, NitroCount]));

generateXml({addClient, Channel, ClientInfo}) ->
    ClientInfoXml = clientInfo_to_xml(ClientInfo),
    lists:flatten(io_lib:format("<event name='addClient' channel='~w'>~s</event>", [Channel, ClientInfoXml]));

%% todo: isDuringLoad - may be something better?
generateXml({addClientInitial, Channel, ClientInfo}) ->
    ClientInfoXml = clientInfo_to_xml(ClientInfo),
    lists:flatten(io_lib:format("<event name='addClient' channel='~w' initial='true'>~s</event>", [Channel, ClientInfoXml]));

generateXml({removeClient, Channel, ClientID}) ->
    lists:flatten(io_lib:format("<event name='removeClient' channel='~w' clientID='~b' />", [Channel, ClientID]));

generateXml({get, userInfo, ok, User}) ->
    "<event name='get' property='userInfo' result='ok'><property name='property'>"
    ++ userInfo_to_xml(User)
    ++ "</property></event>";
    
generateXml({get, secureUserInfo, ok, UserInfo}) ->
    "<event name='get' property='secureUserInfo' result='ok'><property name='property'>"
    ++ secure_userInfo_to_xml(UserInfo)
    ++ "</property></event>";    

generateXml({get, gasInfo, ok, Gas}) ->
    "<event name='get' property='gasInfo' result='ok'><property name='property'>"
    ++ gasInfo_to_xml(Gas)
    ++ "</property></event>";
    
generateXml({get, userState, ok, UserState, CurrentCity}) ->
    lists:flatten(io_lib:format("<event name='get' property='userState' result='ok' currentCity='~w'>~s</event>", [CurrentCity, userState_to_xml(UserState)]));
    
generateXml({set, currentCarID, ok, CarID}) ->
    "<event name='set' property='currentCarID' value='" ++ integer_to_list(CarID) ++ "' result='ok' />";

generateXml({get, invitesInfo, ok, ActiveInvites}) ->
    "<event name='get' property='invitesInfo' result='ok'><property name='property'>"
    ++ invitesInfo_to_xml(ActiveInvites)
    ++ "</property></event>";    

generateXml({set, currentCarID, error, {Reason, Message}, Value}) ->
    generateReasonXml(set, currentCarID, error, {Reason, Message}, Value);

generateXml({set, carName, CarName, _CarID, ClientID}) ->
    "<event name='set' property='carName' value='" ++ CarName ++ "' clientID='" ++ integer_to_list(ClientID) ++ "' />";

generateXml({otherLapTime, Channel, Type, Time, ClientID}) ->
    "<event name='lapTime' channel='" ++ atom_to_list(Channel) ++ "' type='" ++ Type ++ "' time='" ++ integer_to_list(Time) ++ "' clientID='" ++ integer_to_list(ClientID) ++ "' />";

generateXml({otherChatMessage, Channel, Text, UserID, HomeCity, Nick, TimeStamp, Admin}) ->
    lists:flatten(io_lib:format("<event name='chatMessage' channel='~w' text='~s' userId='~w' homeCity='~w' nick='~s' timeStamp='~w' admin='~w'></event>",
                                 [Channel, Text, UserID, HomeCity, Nick, TimeStamp,Admin]));

generateXml({get, cars, ok, Cars}) ->
    "<event name='get' property='cars' result='ok'>" ++ 
        createXmlList("property", fun car_to_xml/1, Cars) ++
    "</event>";

generateXml({get, inbox, ok, PostMessageInfos}) ->
    "<event name='get' property='inbox' result='ok'>" ++ 
        createXmlList("property", fun postMessage_to_xml/1, PostMessageInfos) ++ 
    "</event>";

generateXml({get, tipInfo, ok, Tips}) ->
    "<event name='get' property='tipInfo' result='ok'>" ++
        createXmlList("property", fun tip_to_xml/1, Tips) ++
    "</event>";

generateXml({get, cities, ok, Cities}) ->
    "<event name='get' property='cities' result='ok'>" ++
        createXmlList("property", fun city_to_xml/1, Cities) ++
    "</event>";

generateXml({get, leagueInfo, ok, Leagues}) ->
    "<event name='get' property='leagueInfo' result='ok'>" ++
        createXmlList("property", fun league_to_xml/1, Leagues) ++
    "</event>";

generateXml({processPostMessage, Action, ok, UserInfo, PostMessageInfos}) ->
    "<event name='processPostMessage' result='ok' action='" ++ atom_to_list(Action) ++ "'>" ++         
        userInfo_to_xml(UserInfo) ++
        createXmlList("inbox", fun postMessage_to_xml/1, PostMessageInfos) ++ 
    "</event>";
    
generateXml({processPostMessage, _Action, error, {Reason, Message}} ) ->
    generateReasonXml(processPostMessage, error, {Reason, Message});

generateXml({get, globalInfo, ok, Globals}) ->
    "<event name='get' property='globalInfo' result='ok'><property name='property'><globalInfo "
        ++ createXmlAttributesList(#global.key, #global.value, Globals)
    ++ " /></property></event>";    

generateXml({get, routes, ok, Routes}) ->
    "<event name='get' property='routes' result='ok'>" ++ 
        createXmlList("property", fun route_to_string/1, Routes) ++ 
    "</event>";

generateXml({get, vkontakteInfo, ok, VkontakteInfo}) ->
    "<event name='get' property='vkontakteInfo' result='ok'><property name='property'>" ++ 
        vkontakteInfo_to_xml(VkontakteInfo) ++
    "</property></event>";

generateXml({reward, Type, Experience, Time}) ->
    lists:flatten(io_lib:format("<event name='reward' type='~w' experience='~b' time='~b' />", [Type, Experience, Time]));

generateXml({level, Level, ExpLeft, Message}) ->
    lists:flatten(io_lib:format("<event name='level' level='~b' expLeft='~b' message='~s' />", [Level, ExpLeft, Message]));

generateXml({resetCars}) ->
    "<event name='resetCars' />";
    
generateXml({showIntro, Type} ) ->
    lists:flatten(io_lib:format("<event name='showIntro' type='~w' />", [Type]));

generateXml({get, lobbies, ok, Lobbies}) ->
    "<event name='get' property='lobbies' result='ok' channel='lobby'>" ++ 
        createXmlList("property", fun lobbyInfo_to_xml/1, Lobbies) ++ 
    "</event>";

generateXml({get, ratings, ok, Ratings}) ->
    "<event name='get' property='ratings' result='ok'>" ++ 
        createXmlList("property", fun ratingInfo_to_xml/1, Ratings) ++ 
    "</event>";

generateXml({buyFuel,ok, UserInfo, GasInfo}) ->
    "<event name='buyFuel' result='ok'>"
        ++ userInfo_to_xml(UserInfo)
    ++ gasInfo_to_xml(GasInfo)
    ++ "</event>";

generateXml({createLobby, ok, LobbyInfo, UpgradeInfo} ) ->
    "<event name='createLobby' result='ok'>" ++ lobbyInfo_to_xml(LobbyInfo) ++ upgradeInfo_to_xml(UpgradeInfo) ++ "</event>";

generateXml({joinLobby, ok, LobbyInfo, UpgradeInfo} ) ->
    "<event name='joinLobby' result='ok'>" ++ lobbyInfo_to_xml(LobbyInfo) ++ upgradeInfo_to_xml(UpgradeInfo)  ++ "</event>";

generateXml({loadingProgress, Channel, Progress, ClientID} ) ->
    lists:flatten(io_lib:format("<event name='loadingProgress' channel='~w' progress='~b' clientID='~b' />", [Channel, Progress, ClientID]));

generateXml({get, lobby, ok, LobbyInfo} ) ->
    "<event name='get' property='lobby' result='ok' channel='lobby'><property name='property'>" ++ lobbyInfo_to_xml(LobbyInfo) ++ "</property></event>";

generateXml({get, Name, error, {Reason, Message}} ) ->
    generateReasonXml(get, Name, error, {Reason, Message});

generateXml({quitLobby, Reason}) ->
    "<event name='quitLobby' reason='" ++ Reason ++ "'/>";
       
generateXml({loaded, ClientID}) ->
    lists:flatten(io_lib:format("<event name='loaded' clientID='~b' />", [ClientID]));

generateXml({lobbyRaceResults, LobbyResults, LobbyUpgrades, WearOut, BlueScore, RedScore}) ->
    lists:flatten(io_lib:format("<event name='lobbyRaceResults' blueScore='~w' redScore='~w'>~s~s~s</event>", 
                [BlueScore, RedScore, 
                createXmlList("results", fun lobbyRaceResult_to_xml/1, LobbyResults),
                createXmlList("upgrades", fun upgradeInfo_to_xml/1, LobbyUpgrades),
                createXmlList("wearOutUpgrades", fun wearInfo_to_xml/1, WearOut)]));

generateXml({lobbyRaceEvent, Type, Param, ClientID}) ->
    if 
        is_integer(Param) ->
            P = integer_to_list(Param);
        true ->
            P = Param
    end,
    
    lists:flatten(io_lib:format("<event name='lobbyRaceEvent' type='~w' param='~s' clientID='~w' />", [Type, P, ClientID]));

generateXml({exchangeMoney, ok, UserInfoWithDetails, RealMoney}) ->
   lists:flatten
       (io_lib:format("<event name='exchangeMoney' result='ok' realMoney='~w'>~s</event>",
         [RealMoney, userInfo_to_xml(UserInfoWithDetails)]));
    
generateXml({exchangeMoney, error, {Reason, Message}}) ->
    generateReasonXml(exchangeMoney, error, {Reason, Message});

generateXml({exchangeVkontakteVotes, ok, UserInfoWithDetails, Votes}) ->
    lists:flatten
        (io_lib:format("<event name='exchangeVkontakteVotes' result='ok' votes='~w'>~s</event>",
          [Votes, userInfo_to_xml(UserInfoWithDetails)]));

generateXml({repaintCar, ok, UserInfoWithDetails, RecolorInfo}) ->
    lists:flatten
        (io_lib:format("<event name='repaintCar' result='ok'>~s~s</event>",
          [userInfo_to_xml(UserInfoWithDetails), recolorInfo_to_xml(RecolorInfo)]));

generateXml({buyCar, ok, UserInfoWithDetails, CarClass, Slot}) ->
    lists:flatten
        (io_lib:format("<event name='buyCar' result='ok' slot='~s'>~s~s</event>",
          [atom_to_list(Slot), userInfo_to_xml(UserInfoWithDetails), carClass_to_xml(CarClass)]));

generateXml({sellCar, ok, UserInfoWithDetails, CarClass}) ->
    lists:flatten
        (io_lib:format("<event name='sellCar' result='ok'>~s~s</event>",
          [userInfo_to_xml(UserInfoWithDetails), carClass_to_xml(CarClass)]));

generateXml({buyItem, ok, UserInfoWithDetails, ItemClass}) ->
    lists:flatten
        (io_lib:format("<event name='buyItem' result='ok'>~s~s</event>",
          [userInfo_to_xml(UserInfoWithDetails), itemClass_to_xml(ItemClass)]));

generateXml({sellItem, ok, UserInfoWithDetails, ItemClass}) ->
    lists:flatten
        (io_lib:format("<event name='sellItem' result='ok'>~s~s</event>",
          [userInfo_to_xml(UserInfoWithDetails), itemClass_to_xml(ItemClass)]));

generateXml({cleanCar, ok, Money}) ->
    utils:fmt("<event name='cleanCar' result='ok' money='~w' />", [Money]);

generateXml({Name, ok, UserInfoWithDetails}) ->
    lists:flatten
        (io_lib:format("<event name='~w' result='ok'>~s</event>",
          [Name, userInfo_to_xml(UserInfoWithDetails)]));

generateXml({get, shopInfo, ok, ShopInfo}) ->    
    "<event name='get' property='shopInfo' result='ok'>" ++ 
        createXmlList("property", fun item_to_xml/1, ShopInfo) ++ 
    "</event>";

generateXml({get, carShopInfo, ok, CarShopInfo}) ->
    "<event name='get' property='carShopInfo' result='ok'>" ++
        createXmlList("property", fun car_to_xml/1, CarShopInfo) ++
    "</event>";

generateXml({get, friendInfos, ok, FriendInfos}) ->
    "<event name='get' property='friendInfos' result='ok'>" ++
        createXmlList("property", fun friendInfo_to_xml/1, FriendInfos) ++
    "</event>";

generateXml({get, topUserDailyScores, ok, TopUserDailyScores}) ->
    "<event name='get' property='topUserDailyScores' result='ok'>" ++
        createXmlList("property", fun userDailyScoreInfo_to_xml/1, TopUserDailyScores) ++
    "</event>";

% universal error 
generateXml({get, Property, error, {Reason, Message}} ) ->
    generateReasonXml(get, Property, error, {Reason, Message}).


userInfo_to_xml(UserInfo) ->
    User = UserInfo#userInfo.user,
    Car = UserInfo#userInfo.car,
    lists:flatten(io_lib:format("<userInfo id='~w' displayName='~s' carID='~w' currentCarID='~w' color='~w' homeCity='~w' currentCity='~w' level='~w' experience='~w' expPrevLevel='~w' expNextLevel='~w' rating='~w' duelWin='~w' duelCount='~w' money='~w' realMoney='~w' fuel='~w' nitroCount='~w' hasGift='~w' carFileName='~s' carSlots='~w' isWashed='~w'>~s~s~s~s~s~s</userInfo>",
         [  User#user.id,
            User#user.name,
            Car#car.id,
            Car#car.carClassID,
            Car#car.color,
            User#user.homeCity,
            User#user.currentCity,
            User#user.level,
            User#user.experience,
            UserInfo#userInfo.expPrevLevel,
            UserInfo#userInfo.expNextLevel,
            User#user.rating,
            User#user.duelWin,
            User#user.duelCount,
            User#user.money,
            User#user.realMoney,
            Car#car.fuel,
            Car#car.nitroCount,
            users:hasGift(User),
            dbCar:getCarNameByID(Car#car.id),
            UserInfo#userInfo.carSlots,
            UserInfo#userInfo.isWashed,
            createXmlList("inventory", fun protocol:item_to_xml/1, UserInfo#userInfo.inventory),
            createXmlList("equipment", fun protocol:item_to_xml/1, UserInfo#userInfo.equipment),
            createXmlList("triggers", fun protocol:triggerInfo_to_xml/1, User#user.triggers),
            createXmlList("roles", fun protocol:roleInfo_to_xml/1, User#user.roles),
            createXmlList("cars", fun car_to_xml/1, UserInfo#userInfo.cars),
            upgradeInfo_to_xml(UserInfo#userInfo.upgradeInfo)])).

secure_userInfo_to_xml(UserInfo) ->
    User = UserInfo#userInfo.user,
    Car = UserInfo#userInfo.car,
    lists:flatten(io_lib:format("<userInfo id='~w' displayName='~s' currentCarID='~w' color='~w' homeCity='~w' level='~w' experience='~w' expPrevLevel='~w' expNextLevel='~w' rating='~w' duelWin='~w' duelCount='~w' carFileName='~s'>~s</userInfo>",
         [  User#user.id,
            User#user.name,
            Car#car.carClassID,
            Car#car.color,
            User#user.homeCity,
            User#user.level,
            User#user.experience,
            UserInfo#userInfo.expPrevLevel,
            UserInfo#userInfo.expNextLevel,
            User#user.rating,
            User#user.duelWin,
            User#user.duelCount,
            dbCar:getCarNameByID(Car#car.id),
            upgradeInfo_to_xml(UserInfo#userInfo.upgradeInfo)])).
        
invitesInfo_to_xml(ActiveInvites) ->
    lists:flatten(io_lib:format("<invitesInfo active='~w' />", [ActiveInvites])).

gasInfo_to_xml(GasInfo) ->
    lists:flatten(io_lib:format("<gasInfo fuelPrice='~w' maxFuelCount='~w'>~s</gasInfo>",
         [ GasInfo#gasInfo.fuelPrice,
           GasInfo#gasInfo.maxFuelCount,
           createXmlList("jobOffers", fun workOffer_to_xml/1, GasInfo#gasInfo.jobOffers)])).
       
userState_to_xml(UserState) ->
    if erlang:is_record(UserState#userState.state, userTransferState) ->
        lists:flatten(io_lib:format("<userState name='~w'>~s</userState>",
             ["userTransferState", userTransferState_to_xml(UserState#userState.state)]));
    true ->
        "<userState name=''/>"
    end.
    
userTransferState_to_xml(UserTransferState) ->
    lists:flatten(io_lib:format("<userTransferState citySrc='~w' cityDst='~w' type='~w' arrivalTime='~w' />",
         [UserTransferState#userTransferState.citySrc,
          UserTransferState#userTransferState.cityDst, 
          UserTransferState#userTransferState.type,
          UserTransferState#userTransferState.arrivalTime])).

workOffer_to_xml(WorkOffer) ->
    lists:flatten(io_lib:format("<workOfferInfo id='~b' time='~w' fuel = '~w' message='~s'/>",
        [
            WorkOffer#workOffer.id,
            WorkOffer#workOffer.time,
            WorkOffer#workOffer.fuel,
            WorkOffer#workOffer.message
        ])).

clientInfo_to_xml(ClientInfo) ->
	lists:flatten(io_lib:format("<clientInfo id='~w' userID='~w' displayName='~s' carID='~w' carColor='~w' carName='~s' loadingProgress='~w' homeCity='~w' currentCity='~w' level='~w'>~s~s</clientInfo>", [
		ClientInfo#clientInfo.clientID,
        ClientInfo#clientInfo.userID,
		ClientInfo#clientInfo.displayName,
		ClientInfo#clientInfo.carID,
		ClientInfo#clientInfo.carColor,
		ClientInfo#clientInfo.carFileName,
		ClientInfo#clientInfo.loadingProgress,
		ClientInfo#clientInfo.homeCity,
		ClientInfo#clientInfo.currentCity,
		ClientInfo#clientInfo.level,
		secure_userInfo_to_xml(ClientInfo#clientInfo.userInfo),
        upgradeInfo_to_xml(ClientInfo#clientInfo.upgrades)
	])).

upgradeInfo_to_xml(UpgradeInfo) ->
    lists:flatten(io_lib:format("<upgradeInfo userID='~b' speed='~w' power='~w' controllability='~w' braking='~w'/>",
                                [UpgradeInfo#upgradeInfo.userID,
								 UpgradeInfo#upgradeInfo.speed,
                                 UpgradeInfo#upgradeInfo.power,
                                 UpgradeInfo#upgradeInfo.controllability,
                                 UpgradeInfo#upgradeInfo.braking ] ) ).

wearInfo_to_xml(WearInfo) ->
     lists:flatten(io_lib:format("<wearOutUpgrade id='~w' displayName='~s' delta='~w' durability='~w' durabilityMax='~w'/>",
            [ WearInfo#wearInfo.id,
              WearInfo#wearInfo.displayName,
              WearInfo#wearInfo.delta,
              WearInfo#wearInfo.durability,
              WearInfo#wearInfo.durabilityMax
             ])).

car_to_xml(CarInfo) ->
    Car = CarInfo#carInfo.car,
    CarClass = CarInfo#carInfo.carClass,
    UpgradeInfo = CarInfo#carInfo.carUpgrade,
    lists:flatten(io_lib:format(
    "<carInfo id='~w' classID='~w' displayName='~s' fileName='~s' description='~s' price='~b' sellPrice='~w' durabilityMax='~w'	power='~w'	speed='~w' breaking='~w' controllability='~w' fuelCapacity='~w' fuelConsumption='~w' fuel='~w' color='~w' nitroCount='~w' currentDurability='~w' minLevel='~w' realPrice='~w' repairPrice='~w' capitalRepairPrice='~w' junkName='~s'>~s~s</carInfo>",
                                [Car#car.id,
                                 CarClass#carClass.id,
                                 CarClass#carClass.displayName,
                                 CarClass#carClass.fileName,
                                 CarClass#carClass.description,
                                 CarClass#carClass.price,
                                 CarInfo#carInfo.sellPrice,
                                 Car#car.durabilityMax,
                                 UpgradeInfo#upgradeInfo.power,
                                 UpgradeInfo#upgradeInfo.speed,
                                 UpgradeInfo#upgradeInfo.braking,
                                 UpgradeInfo#upgradeInfo.controllability,
                                 CarClass#carClass.fuelCapacity,
                                 CarClass#carClass.fuelConsumption,
                                 Car#car.fuel,
                                 Car#car.color,
                                 Car#car.nitroCount,
                                 Car#car.durability,
                                 CarClass#carClass.minLevel,
                                 CarClass#carClass.realPrice,
                                 CarInfo#carInfo.repairPrice,
                                 CarInfo#carInfo.capitalRepairPrice,
                                 CarClass#carClass.junkName,
                                 createXmlList("upgrades", fun item_to_xml/1, Car#car.upgrades),
                                 createXmlList("recolorInfo", fun recolor_to_xml/1, CarInfo#carInfo.recolor)
                                ])).

carClass_to_xml(CarClass) ->
    lists:flatten(io_lib:format(
    "<carClass id='~w' displayName='~s' fileName='~s' description='~s' price='~b' power='~w' speed='~w' breaking='~w' controllability='~w' fuelCapacity='~w' fuelConsumption='~w' minLevel='~w' realPrice='~w' junkName='~s' />",
                                [CarClass#carClass.id,
                                 CarClass#carClass.displayName,
                                 CarClass#carClass.fileName,
                                 CarClass#carClass.description,
                                 CarClass#carClass.price,
                                 CarClass#carClass.power,
                                 CarClass#carClass.speed,
                                 CarClass#carClass.breaking,
                                 CarClass#carClass.controllability,
                                 CarClass#carClass.fuelCapacity,
                                 CarClass#carClass.fuelConsumption,
                                 CarClass#carClass.minLevel,
                                 CarClass#carClass.realPrice,
                                 CarClass#carClass.junkName
                                ])).

recolorInfo_to_xml(RecolorInfo) ->
    lists:flatten(io_lib:format(
    "<recolorInfo carDisplayName='~s' color='~w' price='~w' realPrice='~w' />",
                                [RecolorInfo#recolorInfo.carDisplayName,
								 RecolorInfo#recolorInfo.colorID,
                                 RecolorInfo#recolorInfo.price,
                                 RecolorInfo#recolorInfo.realPrice
                                ])).


route_to_string(Route) ->
    % io:format("~w",[Route]),
    "<routeInfo"
        ++  " id='" ++ integer_to_list(Route#route.id)
        ++ "' displayName='" ++ Route#route.displayName
        ++ "' fileNameBase='" ++ Route#route.fileName
        ++ "' length='" ++ integer_to_list(Route#route.length)
        ++ "' minLevel='" ++ integer_to_list(Route#route.minLevel)
        ++ "' isHomeCity='" ++ atom_to_list(Route#route.isHomeCity)
        ++ "' isBattleCity='" ++ atom_to_list(Route#route.isBattleCity)
        ++ "' />".
  
lobbyInfo_to_xml(Lobby) ->
    "<lobbyInfo"
        ++  " id='" ++ integer_to_list(Lobby#lobbyInfo.id)
        ++ "' creatorName='" ++ Lobby#lobbyInfo.creatorName
        ++ "' routeID='" ++ integer_to_list(Lobby#lobbyInfo.routeID)
        ++ "' lapNumber='" ++ integer_to_list(Lobby#lobbyInfo.lapNumber)
        ++ "' direction='" ++ atom_to_list(Lobby#lobbyInfo.direction)
        ++ "' playerCountBlue='" ++ integer_to_list(Lobby#lobbyInfo.playerCountBlue)
        ++ "' playerCountRed='" ++ integer_to_list(Lobby#lobbyInfo.playerCountRed)
        ++ "' playerMax='" ++ integer_to_list(Lobby#lobbyInfo.playerMax)
        ++ "' status='" ++ atom_to_list(Lobby#lobbyInfo.status)       
        ++ "' timerEnd='" ++ integer_to_list(Lobby#lobbyInfo.timerEnd)
        ++ "' allowedCarID='" ++ integer_to_list(Lobby#lobbyInfo.allowedCarID)
        ++ "' league='" ++ integer_to_list(Lobby#lobbyInfo.league)
        ++ "' creatorCarName='" ++ Lobby#lobbyInfo.creatorCarName
        ++ "' creatorRating='" ++ integer_to_list(Lobby#lobbyInfo.creatorRating)
        ++ "' stake='" ++ integer_to_list(Lobby#lobbyInfo.stake)
        ++ "' type='" ++ atom_to_list(Lobby#lobbyInfo.type)
        ++ "' />".

ratingInfo_to_xml(Rating) ->
    lists:flatten(io_lib:format("<ratingInfo userID='~w' displayName='~s' rating='~s' position='~s'></ratingInfo>",
       [
           Rating#ratingInfo.userID,
           Rating#ratingInfo.displayName,
           integer_to_list(Rating#ratingInfo.rating),
           integer_to_list(Rating#ratingInfo.position)
       ])).

lobbyRaceResult_to_xml(LobbyResult) ->
    lists:flatten(io_lib:format(
        "<lobbyRaceResult userID='~w' clientID='~w' displayName='~s' time='~b' position='~b'"
        ++ " oldRating='~b' newRating='~b' fuel='~w' money='~w' experience='~w' score='~w' />", [
        LobbyResult#lobbyResult.userID,
        LobbyResult#lobbyResult.clientID,
        LobbyResult#lobbyResult.displayName,
        LobbyResult#lobbyResult.time,
        LobbyResult#lobbyResult.position,
        LobbyResult#lobbyResult.oldRating,
        LobbyResult#lobbyResult.newRating,
        LobbyResult#lobbyResult.fuel,
        LobbyResult#lobbyResult.money,
        LobbyResult#lobbyResult.experience,
        LobbyResult#lobbyResult.score
    ])).
                                 
item_to_xml(Item) ->
    ItemClass = dbItem:getClass(Item#item.itemClassID),    
    lists:flatten(io_lib:format("<itemInfo id='~w' classID='~w' name='~s' category='~s' description='~s' price='~b' usingType='~s' usingCount='~w' slot='~s' durabilityMax='~w'	power='~w'	speed='~w' braking='~w' controllability='~w' currentDurability='~w' minLevel='~w' maxLevel='~w' realPrice='~w' repairPrice='~w' junkName='~s' newDurabilityMax='~w' />",
                                [Item#item.id,
                                 ItemClass#itemClass.id,
                                 ItemClass#itemClass.name, 
                                 ItemClass#itemClass.category,
                                 ItemClass#itemClass.description,
                                 ItemClass#itemClass.price,
                                 ItemClass#itemClass.usingType,
                                 ItemClass#itemClass.usingCount,
                                 ItemClass#itemClass.slot,
                                 Item#item.durabilityMax,
                                 ItemClass#itemClass.power,
                                 ItemClass#itemClass.speed,
                                 ItemClass#itemClass.braking,
                                 ItemClass#itemClass.controllability,
                                 Item#item.durability,
                                 ItemClass#itemClass.minLevel,
                                 ItemClass#itemClass.maxLevel,
                                 ItemClass#itemClass.realPrice,
                                 ItemClass#itemClass.repairPrice,
                                 ItemClass#itemClass.junkName,
                                 ItemClass#itemClass.durabilityMax
                                ])).

itemClass_to_xml(ItemClass) ->
    lists:flatten(io_lib:format("<itemClass id='~w' name='~s' category='~s' description='~s' price='~b' usingType='~s' usingCount='~w' slot='~s' power='~w'	speed='~w' braking='~w' controllability='~w' minLevel='~w' maxLevel='~w' realPrice='~w' repairPrice='~w' junkName='~s' durabilityMax='~w' />",
                                [ItemClass#itemClass.id,
                                 ItemClass#itemClass.name, 
                                 ItemClass#itemClass.category,
                                 ItemClass#itemClass.description,
                                 ItemClass#itemClass.price,
                                 ItemClass#itemClass.usingType,
                                 ItemClass#itemClass.usingCount,
                                 ItemClass#itemClass.slot,
                                 ItemClass#itemClass.power,
                                 ItemClass#itemClass.speed,
                                 ItemClass#itemClass.braking,
                                 ItemClass#itemClass.controllability,
                                 ItemClass#itemClass.minLevel,
                                 ItemClass#itemClass.maxLevel,
                                 ItemClass#itemClass.realPrice,
                                 ItemClass#itemClass.repairPrice,
                                 ItemClass#itemClass.junkName,
                                 ItemClass#itemClass.durabilityMax
                                ])).

recolor_to_xml(Recolor) ->
    lists:flatten(io_lib:format("<recolorInfo color='~w' price='~w' realPrice='~w' />",
                                [Recolor#recolorPrice.color,
                                 Recolor#recolorPrice.price,
                                 Recolor#recolorPrice.realPrice
                                ])).

postMessage_to_xml(PostMessageInfo) ->
    lists:flatten(io_lib:format("<postMessageInfo id='~w' fromNick='~s' timeStamp='~w' money='~w' sellPrice='~w' comment='~s' senderID='~w'>~s</postMessageInfo>",
        [
            PostMessageInfo#postMessageInfo.id,
            PostMessageInfo#postMessageInfo.fromNick,
            PostMessageInfo#postMessageInfo.timeStamp,
            PostMessageInfo#postMessageInfo.money,
            PostMessageInfo#postMessageInfo.sellPrice,
            PostMessageInfo#postMessageInfo.comment,
            PostMessageInfo#postMessageInfo.senderID,
            createXmlList("itemInfo", fun item_to_xml/1, PostMessageInfo#postMessageInfo.itemInfo)
        ])).

tip_to_xml(Tip) ->
    lists:flatten(io_lib:format("<tipInfo message='~s'/>",
        [
            Tip
        ])).

city_to_xml(City) ->
    lists:flatten(io_lib:format("<cityInfo id='~w' score='~w'/>",
        [
            City#city.id,
            City#city.score
        ])).

league_to_xml(League) ->
    lists:flatten(io_lib:format("<leagueInfo id='~w' minLevel='~w' />",
        [
            League#league.id,
            League#league.minLevel
        ])).

vkontakteInfo_to_xml(VkontakteInfo) ->
    lists:flatten(io_lib:format("<vkontakteInfo userAppBalance='~w' />",
        [
            VkontakteInfo#vkontakteInfo.userAppBalance
        ])).

triggerInfo_to_xml({TriggerName, TriggerValue}) ->
    lists:flatten(io_lib:format("<triggerInfo name='~w' value='~w' />", [TriggerName, TriggerValue]));

triggerInfo_to_xml(TriggerName) ->
    lists:flatten(io_lib:format("<triggerInfo name='~w' />",
        [
            TriggerName
        ])).

roleInfo_to_xml(RoleName) ->
    lists:flatten(io_lib:format("<roleInfo name='~w' />",
        [
            RoleName
        ])).
        
friendInfo_to_xml(FriendInfo) ->
    lists:flatten(io_lib:format("<friendInfo userID='~w' displayName='~s' level='~w' city='~w' rating='~w' vkontakteID='~w' isWashed='~w' />",
        [
            FriendInfo#friendInfo.userID,
            FriendInfo#friendInfo.displayName,
            FriendInfo#friendInfo.level,
            FriendInfo#friendInfo.city,
            FriendInfo#friendInfo.rating,
            FriendInfo#friendInfo.vkontakteID,
            FriendInfo#friendInfo.isWashed
        ])).
        
userDailyScoreInfo_to_xml(UserDailyScoreInfo) ->
    lists:flatten(io_lib:format("<userDailyScoreInfo userID='~w' displayName='~s' score='~w' homeCity='~w' />",
        [
            UserDailyScoreInfo#userDailyScoreInfo.userID,
            UserDailyScoreInfo#userDailyScoreInfo.displayName,
            UserDailyScoreInfo#userDailyScoreInfo.score,
            UserDailyScoreInfo#userDailyScoreInfo.homeCity
        ])).

createXmlList(Name, Fun, List) ->
    "<list name='" ++ Name ++ "'>"
    ++ lists:foldl(
        fun(Element, Acc) ->
            Acc  ++ Fun(Element)
        end, "", List)
    ++ "</list>".
    
createXmlAttributesList(KeyElement, ValueElement, List) ->
    lists:foldl(
        fun(Element, Acc) ->
            Acc ++ utils:fmt("~w='~w' ", [element(KeyElement, Element), element(ValueElement, Element)])
        end, "", List).
