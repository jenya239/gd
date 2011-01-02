-module(dbGame).
-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([
	getLevel_nt/2,
	getLevel/2,
    getTips/1,
    getTips/2, 
    getRoutesWithMinimumLevel/1
]).


%TODO избавиться от дублирования кода
getLevel(number, LevelNumber) ->
	Result = mneser:do(qlc:q([L || L <- mnesia:table(level), L#level.number =:= LevelNumber])),
	case Result of
		[Item] ->
			Item;
		[] ->
			undefined;
		Other ->
			Other
	end.

getLevel_nt(number, LevelNumber) ->
	Result = qlc:e(qlc:q([L || L <- mnesia:table(level), L#level.number =:= LevelNumber])),
	case Result of
		[Item] ->
			Item;
		[] ->
			undefined;
		Other ->
			Other
	end.

getTips(Level) ->
   mneser:do(qlc:q([T#tip.message || T <- mnesia:table(tip),
                    T#tip.minLevel =< Level,
                    T#tip.maxLevel >= Level,
                    T#tip.available]
                    )).

getTips(userID, UserID) ->
    User = mneser:getRecord(user, UserID),
    Level = User#user.level,
    getTips(Level).

getRoutesWithMinimumLevel(Level) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
    	qlc:e(qlc:q([R || R <- mnesia:table(route), R#route.minLevel =:= Level]))
    end),
    
    Result.
