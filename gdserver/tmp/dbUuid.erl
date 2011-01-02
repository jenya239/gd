-module(dbUuid).

-include("data.hrl").
-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
	get_nt/1,
	get/1,
	fix/1,
	reset/1
]).

get_nt(Type) ->
	case qlc:e((qlc:q([X || X <- mnesia:table(uuid), X#uuid.type =:= Type])))  of
		[] -> mnesia:write(#uuid{current = 1, type = Type}),
			1;
		[OldUUID]->Answer = OldUUID#uuid.current + 1,
			mnesia:write(OldUUID#uuid{current=Answer}),
			Answer
	end.

%todo: must be removed.
get(Type)->
	{atomic, Val} = mnesia:transaction(fun() -> get_nt(Type) end),
	Val.

fix(Type) ->
	{atomic, ok} =
		mnesia:transaction(
		fun() ->
			N = lists:max([1]++mnesia:all_keys(Type)),
			mnesia:write( #uuid{type=Type,current=N} )
		end).

reset(Type) ->
	{atomic, ok} =
		mnesia:transaction(
		fun() ->
				mnesia:delete({uuid, Type})
		end).
