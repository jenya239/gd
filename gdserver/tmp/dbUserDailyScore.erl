-module(dbUserDailyScore).

-export([            
            addScore_nt/2,
            topScore/3
        ]).

-include("data.hrl").
-include("config.hrl").

-include_lib("stdlib/include/qlc.hrl").
   
addScore_nt(UserID, Delta) ->
    case mnesia:read({userDailyScore, UserID}) of
        [UserDailyScore] -> 
            mnesia:write(UserDailyScore#userDailyScore{score=UserDailyScore#userDailyScore.score+Delta});
        [] -> 
            User = dbUser:getRecord_nt(id, UserID),
            if erlang:is_record(User, user) ->
                HomeCity = User#user.homeCity,
                mnesia:write(#userDailyScore{userID=UserID, score=Delta, homeCity=HomeCity});
            true -> ok end
    end.
    
topScore(DateFrom, DateTo, CityID)  ->
    {atomic, {_, _, Result}} = mnesia:transaction(fun() ->
    lists:foldl(fun mneser:groupBy_Sum/2, {2, {4, 3}, []}, 
        lists:keysort(2, 
            lists:filter(fun(E) -> element(5, E) =:= CityID end, 
                lists:flatten(
                    qlc:e(qlc:q([Scores || {_, Date, {_, Scores}} <- mnesia:table(scoreHistory), Date >= DateFrom, Date =< DateTo]))
        ))))
    end),
    lists:foldl(fun({UserID, Score, Nick}, NewList) ->
        case dbUser:getRecord(id, UserID) of
            User when is_record(User, user) ->
                [{User#user.vkontakteID, Score, Nick} | NewList];
            _NotUser ->
                NewList
        end
    end, [], Result).
