-module(dbAdvertisement).
-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([
	put/1,
	getLast/0,
	getAll/0
]).

put(Message) ->
	mneser:writeRecord(#advertisement{id = dbUuid:get(advertisement), message = Message}).

getLast() ->
	Result = mneser:do(qlc:q([
		Y#advertisement.message
		||
		X <- mnesia:table(uuid),
		Y <- mnesia:table(advertisement),
		X#uuid.type =:= advertisement,
		X#uuid.current =:= Y#advertisement.id
	])),
	case Result of
		[Element] -> Element;
		[] -> "";
		_  -> ""
	end.

getAll() ->
	mneser:do (qlc:q([
		{ X#advertisement.id, X#advertisement.date, X#advertisement.message }
		|| 
		X <- mnesia:table(advertisement)
	])).
