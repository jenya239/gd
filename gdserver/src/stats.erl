-module(stats).

-include("data.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([aggregate/2, getActiveUserIDs/2, getPayingUserIDs/2, getRegisteredUserIDs/2, getUserNames/1, groupByElement/2,
        countLobbyCreations/2, getMonthlyNavigationHtmlAndDateRange/1, getDailyNavigationHtmlAndDateRange/1]).

-define(MIN_LOGIN_COUNT_FOR_ACTIVE, 2).

aggregate(#activity{userid = UserID, date = Date}, {From, PeriodLength, [{UserID, PeriodIndex, Count} | List]}) when Date < From + (PeriodIndex + 1) * PeriodLength  ->
    {From, PeriodLength, [{UserID, PeriodIndex, Count + 1} | List]};

aggregate(#activity{userid = UserID, date = Date}, {From, PeriodLength, List}) ->
    {From, PeriodLength, [{UserID, (Date - From) div PeriodLength, 1} | List]}.

groupByElement(Tuple, {ElementIndex, [Head | _Tail]} = Acc) when Head =:= element(ElementIndex, Tuple) ->
    Acc;

groupByElement(Tuple, {ElementIndex, List}) ->
    {ElementIndex, [element(ElementIndex, Tuple) | List]}.

getActivityCount(From, To, Activity, IsTuple) ->
    {atomic, Activities} = mnesia:transaction( fun() ->
        if not IsTuple ->
            qlc:e(qlc:q([X || X <- mnesia:table(activity),
                              X#activity.action =:= Activity, 
                              X#activity.result =:= ok,
                              X#activity.date >= From, 
                              X#activity.date < To]));
        true ->                  
            qlc:e(qlc:q([X || X <- mnesia:table(activity),
                            element(1, X#activity.action) =:= Activity, 
                            X#activity.result =:= ok,
                            X#activity.date >= From, 
                            X#activity.date < To]))
        end        
    end),
        
    {_, Result} = lists:foldl(fun groupByElement/2, {#activity.userid, []}, Activities),
    Result.    
    
getActivityCountWithAggregation(From, To, Activity, IsTuple) ->
    {atomic, ActivityCount} = mnesia:transaction( fun() ->
        A = if not IsTuple ->
            qlc:e(qlc:q([X || X <- mnesia:table(activity),
                              X#activity.action =:= Activity, 
                              X#activity.result =:= ok,
                              X#activity.date >= From, 
                              X#activity.date < To]));
        true ->                  
            qlc:e(qlc:q([X || X <- mnesia:table(activity),
                            element(1, X#activity.action) =:= Activity, 
                            X#activity.result =:= ok,
                            X#activity.date >= From, 
                            X#activity.date < To]))
        end,                      
        Q = qlc:keysort(#activity.userid, A),
        PeriodLength = 7 * 24 * 3600 * 1000000,
        {_, _, Result} = qlc:fold(fun aggregate/2, {From, PeriodLength, []}, Q),
        Result
    end), 
    ActivityCount.
    
getActiveUserIDs(From, To) ->
    LoginCounts = getActivityCountWithAggregation(From, To , authorize, false),    
    FilteredLoginCounts = lists:filter(fun({_, _, Count}) -> Count >= ?MIN_LOGIN_COUNT_FOR_ACTIVE end, LoginCounts),

    {_, Result} = lists:foldl(fun groupByElement/2, {1, []}, FilteredLoginCounts), 
    Result.
    
getPayingUserIDs(From, To) ->
    Result = getActivityCount(From, To, exchangeVkontakteVotes, true), 
    %log:write("~p~n", [Result]),
    Result.

getRegisteredUserIDs(From, To) ->
    getActivityCount(From, To, register, false).

getUserNames(UserIDs) ->
    {atomic, R} = mnesia:transaction(
        fun() ->
            qlc:e(qlc:q([U#user.name ||
                         UserID <- UserIDs, 
                         U <- mnesia:table(user), 
                         UserID =:= U#user.id]))
        end  
    ), R.
    
countLobbyCreations(From, To) ->
    {atomic, LobbyCreations} = mnesia:transaction( fun() ->
        qlc:e(qlc:q([X || X <- mnesia:table(activity),
                              X#activity.action =:= createLobby, 
                              X#activity.result =:= ok, 
                              X#activity.date >= From, 
                              X#activity.date < To]))
    end),
    
    length(LobbyCreations).

getMonthlyNavigationHtmlAndDateRange(Arg) ->
    {{LocalYear, LocalMonth, _LocalDay}, _LocalTime}= calendar:local_time(),
    M = case yaws_api:getvar(Arg, "m") of {ok, M1} -> list_to_integer(M1); undefined -> LocalMonth end,
    Y = case yaws_api:getvar(Arg, "y") of {ok, Y1} -> list_to_integer(Y1); undefined -> LocalYear end,

    {PrevYear, PrevMonth} = utils:prevMonth({Y, M}),
    {NextYear, NextMonth} = utils:nextMonth({Y, M}),

    NavigationHtml = {p, [], [
        {a, [{href, utils:fmt("?t=monthly&y=~b&m=~b", [PrevYear, PrevMonth])}], "<"},
        {span, [], utils:fmt(" ~b/~b ", [Y, M])},
        {a, [{href, utils:fmt("?t=monthly&y=~b&m=~b", [NextYear, NextMonth])}], ">"}]},
    
    From = utils:dateToUniversalErlangTime({Y, M, 1}),
    To = utils:dateToUniversalErlangTime({NextYear, NextMonth, 1}),
    
    {NavigationHtml, From, To}.

getDailyNavigationHtmlAndDateRange(Arg) ->
    {{LocalYear, LocalMonth, LocalDay}, _LocalTime}= calendar:local_time(),
    D = case yaws_api:getvar(Arg, "d") of {ok, D1} -> list_to_integer(D1); undefined -> LocalDay end,
    M = case yaws_api:getvar(Arg, "m") of {ok, M1} -> list_to_integer(M1); undefined -> LocalMonth end,
    Y = case yaws_api:getvar(Arg, "y") of {ok, Y1} -> list_to_integer(Y1); undefined -> LocalYear end,

    {PrevYear, PrevMonth, PrevDay} = utils:prevDay({Y, M, D}),
    {NextYear, NextMonth, NextDay} = utils:nextDay({Y, M, D}),

    NavigationHtml = {p, [], [
        {a, [{href, utils:fmt("?t=daily&y=~b&m=~b&d=~b", [PrevYear, PrevMonth, PrevDay])}], "<"},
        {span, [], utils:fmt(" ~b/~b/~b ", [Y, M, D])},
        {a, [{href, utils:fmt("?t=daily&y=~b&m=~b&d=~b", [NextYear, NextMonth, NextDay])}], ">"}]},

    From = utils:dateToUniversalErlangTime({Y, M, D}),
    To = utils:dateToUniversalErlangTime({NextYear, NextMonth, NextDay}),
        
    {NavigationHtml, From, To}.
