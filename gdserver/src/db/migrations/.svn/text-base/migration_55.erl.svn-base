-module(migration_55).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-define(RATING_COLUMN, 11).

-record(
    userOld,
    {
    id,
    name,
    currentCarID,
    level=1,
    experience=0,
    roles=[],
    triggers=[],
    date = erlang:now(),
    vkontakteID,
    rating=0,
    city,
    money=0,
    realMoney=0,
    referer=0
    }).


-record(
    user,
    {
    id,
    name,
    currentCarID,
    level=1,
    experience=0,
    roles=[],
    triggers=[],
    date = erlang:now(),
    vkontakteID,
    rating=0,
    city,
    money=0,
    realMoney=0,
    referer=0,
    duelWin=0,
    duelCount=0
    }).

-record(global,
    {
        key,
        value
    }
).

migrate() ->
    MaxRating = getMaxRating(),
    {atomic,ok} = mnesia:transform_table(user, fun(User) -> migrateUser(User,MaxRating) end, record_info(fields, user), user),
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:write(#global{key=duelRate, value=0.0}),
        set_nt({minCityRating, 1}, 0),
        set_nt({minCityRating, 2}, 0)
    end),
    {migrated, 55}.

migrateUser(OldUser1,MaxRating) ->
    Old = utils:changeRecordName(OldUser1, userOld),
    Rating = recalcRating(Old#userOld.rating, MaxRating),
    #user{
        id = Old#userOld.id,
            name = Old#userOld.name,
            currentCarID = Old#userOld.currentCarID,
            level = Old#userOld.level,
            experience = Old#userOld.experience,
            roles = Old#userOld.roles,
            triggers = Old#userOld.triggers,
            date = Old#userOld.date,
            vkontakteID = Old#userOld.vkontakteID,
            rating = Rating,
            city = Old#userOld.city,
            money = Old#userOld.money,
            realMoney = Old#userOld.realMoney,
            referer = Old#userOld.referer
         }.

recalcRating(Rating, MaxRating) ->
    if MaxRating =:= 0 ->
        1000;
    true ->
        1000 + ( (1500 * Rating) div MaxRating)
    end.
    
% return max rating in global
getMaxRating() ->
    {atomic, Ratings} = mnesia:transaction (
		fun() ->
            MinRating =
            case get_nt({minCityRating, 1}) of
                undefined ->
                    0;
                M ->
                    M
            end,
            SortedHandle = qlc:keysort(?RATING_COLUMN, 
                                       qlc:q([X || X <- mnesia:table(user),
                                              element(?RATING_COLUMN, X) >= MinRating,
                                              is_integer(element(2, X))]),
                                       {order, descending}),
            Cursor = qlc:cursor(SortedHandle),
            PagedHandle = qlc:next_answers(Cursor, 1),
            qlc:delete_cursor(Cursor),
		        case PagedHandle of
              [Y]   -> element(?RATING_COLUMN, Y);
              _other -> 1
            end
     end
	 ),
	Ratings.

get_nt({minCityRating, 1}) -> getValue_nt(list_to_atom("minCityRating" ++ "1"));
get_nt({minCityRating, 2}) -> getValue_nt(list_to_atom("minCityRating" ++ "2"));
get_nt(A) -> getValue_nt(A).

set_nt({minCityRating, 1}, Value) -> setValue_nt(list_to_atom("minCityRating" ++ "1"), Value);
set_nt({minCityRating, 2}, Value) -> setValue_nt(list_to_atom("minCityRating" ++ "2"), Value).

getValue_nt(Name) ->
    case mnesia:read({global, Name}) of
        [Value] ->
            Value#global.value;
        _Other ->
            undefined
    end.

setValue_nt(Name, Value) ->
	mnesia:write(#global{key=Name, value=Value}).





