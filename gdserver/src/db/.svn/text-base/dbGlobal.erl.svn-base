-module(dbGlobal).

-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	getAll/0,
	get_nt/1,
	get/1,
	exists/1
]).

getAll() ->
	{atomic, GlobalInfo} = mnesia:transaction(
		fun() ->
			qlc:e(qlc:q([X || X <- mnesia:table(global)]))
		end
	),
	GlobalInfo.

get_nt({fuelCost, 1}) -> get_nt(fuelCost1);
get_nt({fuelCost, 2}) -> get_nt(fuelCost2);
get_nt({fuelCost, 3}) -> get_nt(fuelCost3);

get_nt(Name) ->
    case mnesia:read({global, Name}) of
        [Value] ->
            Value#global.value;
        _Other ->
            undefined
    end.

get(Name) ->
	{atomic, Value} = mnesia:transaction(fun() -> get_nt(Name) end),
	Value.

exists(Key) ->
	mneser:recordExists(global, Key).
