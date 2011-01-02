-module(dbUserProgress).

-include_lib("stdlib/include/qlc.hrl").
-include("data.hrl").

-export([addOnlineStats/4, getOrCreate/1, getOrCreate_nt/1, newLevel/2, 
    addKilometers/2, incWorksCounter/1, createLevelingJSON/2, incRealPurchases/1]).
    
-define(MILLISECONDS_IN_HOUR, 3600000).

getOrCreate(UserID) ->
    TransactionResult = mnesia:transaction(fun() -> 
        getOrCreate_nt(UserID)
    end), 
    
    case TransactionResult of
        {atomic, Result} ->
            Result;
        _Other ->
            error
    end.

getOrCreate_nt(UserID) ->    
    Result = qlc:e(qlc:q([X || X <- mnesia:table(userProgress), X#userProgress.userID =:= UserID])),
    UserProgress = case Result of
        [Element] ->
            Element;
        _Other ->
            UP = #userProgress{userID=UserID},
            mnesia:write(UP),
            UP
    end,
    UserProgress.
    
addOnlineStats(UserID, Time, Kilometers, WorksCounter) ->
    {atomic, ok} = mnesia:transaction(fun() -> 
        UserProgress = getOrCreate_nt(UserID),
        
        OldOnlineTime = UserProgress#userProgress.onlineTime,
        OldKilometers = UserProgress#userProgress.kilometers,
        OldWorksCounter = UserProgress#userProgress.worksCounter,
        
        mnesia:write(UserProgress#userProgress{onlineTime=OldOnlineTime + Time, 
                kilometers=OldKilometers + Kilometers, worksCounter=OldWorksCounter + WorksCounter})
    end).
    
newLevel(Level, Session) ->
    {atomic, NewProgress} = mnesia:transaction(fun() -> 
        UserProgress = getOrCreate_nt(Session#session.userID),
        SessionTime = utils:now() - Session#session.startTime,
        OnlineTime = UserProgress#userProgress.onlineTime + SessionTime,
    
        {_, LastTime} = lists:last(UserProgress#userProgress.leveling),
        NewLeveling = lists:append(UserProgress#userProgress.leveling, [{Level, OnlineTime - LastTime}]),
        NewUserProgress = UserProgress#userProgress{leveling=NewLeveling},
        mnesia:write(NewUserProgress),
        NewUserProgress
    end),
    NewProgress.
    
addKilometers(Session, LobbyInfo) ->
    UserID = Session#session.userID,
    
    {atomic, NewSession} = mnesia:transaction(fun() -> 
        Route = mneser:getRecord_nt(route, LobbyInfo#lobbyInfo.routeID),
        Kilometers = (Route#route.length / 1000) * LobbyInfo#lobbyInfo.lapNumber,
        UserProgress = getOrCreate_nt(UserID),
        OldProgressKm = UserProgress#userProgress.kilometers,
        OldSessionKm = Session#session.kilometers,
        mnesia:write(UserProgress#userProgress{kilometers=OldProgressKm + Kilometers}),
        Session#session{kilometers=OldSessionKm + Kilometers}
    end),
    NewSession.
    
incWorksCounter(Session) ->
    {atomic, NewSession} = mnesia:transaction(fun() -> 
        UserProgress = getOrCreate_nt(Session#session.userID),
        OldProgresCounter = UserProgress#userProgress.worksCounter,
        OldSessionCounter = Session#session.worksCounter,
        mnesia:write(UserProgress#userProgress{worksCounter=OldProgresCounter + 1}),
        Session#session{worksCounter=OldSessionCounter + 1}
    end),
    NewSession.
    
incRealPurchases(User) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        UserProgress = dbUserProgress:getOrCreate_nt(User#user.id),
        
        if UserProgress#userProgress.realPurchases =:= 0 ->
            case dbGlobal:get_nt(addRating) > 0 of
                true ->
                    Text = dbGlobal:get_nt(ratingText),
                    vkontakte:sendVkontakteRating(User#user.vkontakteID, 1, Text, true);
                false ->
                    ok
            end;
        true -> ok end,
        
        NewPurchases = UserProgress#userProgress.realPurchases + 1,
        mnesia:write(UserProgress#userProgress{realPurchases=NewPurchases})
    end).
    
createLevelingJSON(From, To) ->
    UserIDs = dbUser:getRegisteredUserIDs(From, To),
    
    List = lists:foldl(fun(UserID, LevelingList) -> 
        UserProgress = getOrCreate(UserID),
        lists:foldl(fun({UserLevel, UserTime}, NewList) -> 
            case lists:keysearch(UserLevel, 1, NewList) of
                false ->
                    if is_number(UserLevel) ->
                        NewList ++ [{UserLevel, 1, UserTime}];
                    true ->
                        NewList
                    end;
                {value, {Level, Count, TimeSum}} ->
                    lists:keyreplace(UserLevel, 1, NewList, {Level, Count + 1, TimeSum + UserTime})
            end
        end, LevelingList, UserProgress#userProgress.leveling)
    end, [], UserIDs),
    
    LevelingListStripped = if length(List) > 0 -> lists:keydelete(1, 1, List); true -> List end,        
    SortedLevelingList = lists:keysort(1, LevelingListStripped),
    
    {Max, Values, Labels} = lists:foldl(fun({Lv, Cnt, Tm}, {M, VList, LList}) -> 
        NewMax = if Cnt > M -> Cnt; true -> M end,
        Value = io_lib:format(
        "
        {
            \"type\": \"dot\",
            \"value\": ~b,
            \"colour\":  \"#D02020\",
            \"tip\": \"Кол-во: #val#<br>Среднее время: ~.2f\"
        },
        ", [Cnt, Tm / ?MILLISECONDS_IN_HOUR / Cnt]),
        Label = io_lib:format("\"~b\",", [Lv]),
        {NewMax, VList ++ [Value], LList ++ Label}
    end, {0, [], []}, SortedLevelingList),
    
    SValues = string:strip(Values, right, $,),
    SLabels = string:strip(Labels, right, $,),
    
    io_lib:format(
    "
    {
      \"elements\": [
        {
          \"type\": \"line\",
          \"values\": [~s],
          \"colour\": \"#FFEF3F\"
        }
      ],
      \"title\": {
        \"text\": \"Статистика по уровням\"
      },
      \"bg_colour\": \"#FFFFFF\",
      \"y_axis\": {
        \"min\": 0,
        \"max\": ~b,
        \"steps\": 5
      },
      \"x_axis\": {
        \"labels\": {
          \"labels\": [~s]
        }
      }
    }    
    ",
    [SValues, Max + 10, SLabels]
    ).