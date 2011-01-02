-module(dbCity).

-export([            
            getCities/0,
            getCitiesRounded/0,
            get_nt/1,
            resetScore_nt/1,
            addScore_nt/2,
            get/1,
            getScores/0
        ]).

-include("data.hrl").
-include("config.hrl").

-include_lib("stdlib/include/qlc.hrl").

getCities() ->
    {atomic, Result} = mnesia:transaction(fun() ->
        qlc:e(qlc:q([C || C <- mnesia:table(city)]))
    end),
    Result.
    
getCitiesRounded() ->
    lists:map(fun(City) ->
        City#city{score=trunc(City#city.score)}
    end, getCities()).

get(CityID) ->
    case mnesia:dirty_read({city, CityID}) of
        [City] -> City;
        [] -> notFound
    end.

getScores() ->
    {(dbCity:get(1))#city.score, (dbCity:get(2))#city.score}.

get_nt(CityID) ->
    case mnesia:read({city, CityID}) of
        [City] -> City;
        [] -> notFound
    end.

resetScore_nt(CityID) ->
    case mnesia:read({city, CityID}) of
        [City] -> mnesia:write(City#city{score=0, minScore=0});
        [] -> notFound
    end.
    
addScore_nt(CityID, Delta) ->
    case mnesia:read({city, CityID}) of
        [City] -> mnesia:write(City#city{score=City#city.score+Delta});
        [] -> notFound
    end.

