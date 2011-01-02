-module(migration_51).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
  league,
  {
   id,
   name,
   minLevel,
   moneyCoef=1,
   maxPlayerCount=10,
   ratingCoef=1
  }
).

-record(
    uuid,
    {
        type,
        current
    }
).

migrate() ->
    mneser:createTables([
		{league,
      [{attributes, record_info(fields, league)},
       {disc_copies,  [node()]},
       {type, set}]}]),
    {atomic, _} = mnesia:transaction(fun() ->
        mnesia:write(#league{id=get_nt(league),
                            name="новичок",
                            minLevel=1,
                            moneyCoef=1}),
        mnesia:write(#league{id=get_nt(league),
                            name="гонщик",
                            minLevel=4,
                            moneyCoef=1.5}),
        mnesia:write(#league{id=get_nt(league),
                            name="эксперт",
                            minLevel=8,
                            moneyCoef=2}),
        mnesia:write(#league{id=get_nt(league),
                            name="дуэль",
                            minLevel=5,
                            moneyCoef=1,
                            maxPlayerCount=2})
    end),
    {migrated, 51}.

get_nt(Type) ->
	case qlc:e((qlc:q([X || X <- mnesia:table(uuid), X#uuid.type =:= Type])))  of
		[] -> mnesia:write(#uuid{current = 1, type = Type}),
			1;
		[OldUUID]->Answer = OldUUID#uuid.current + 1,
			mnesia:write(OldUUID#uuid{current=Answer}),
			Answer
	end.
    
