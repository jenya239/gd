-module(migration_43).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

replace_nt(CityId) ->
	case mnesia:read({global, {minCityRating, CityId}}) of
		[{global, _, Value}] ->
			mnesia:delete(global, {minCityRating, CityId}, write),
			mnesia:write({global, list_to_atom("minCityRating" ++ integer_to_list(CityId)), Value});
		_Other ->
			mnesia:write({global, list_to_atom("minCityRating" ++ integer_to_list(CityId)), 0})
	end.

migrate() ->
	mnesia:transaction(fun() ->
		replace_nt(1),
		replace_nt(2)
	end),
	
	{migrated, 43}.