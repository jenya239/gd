-module(migration_50).

-export([migrate/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(
  recolorPrice,
  {
   id,
   carClassID,
   color,
   price,
   realPrice
  }
).

-record(
    carClass,
    {
    id,
    displayName,
    fileName,               %-- имя xml файла с физическими параметрами авто

    speed=0,                %-- параметры врожденного апгрейда
    breaking=0,
    power=0,
    controllability=0,

    speedView,              %  -- отображаемые параметры авто
    breakingView,
    powerView,
    controllabilityView,

    fuelCapacity,     % -- вместимость бака
    fuelConsumption,  % -- расход литров на км

    durabilityMax=1000,
    durabilityCoef=0,   % -- потеря хитпоинтов на км

    price=1000,             %     -- цены
    realPrice=0,

    minLevel,
    colorCount=9,
    repairPrice=0,         %-- стоимость ремонта, за восстановление одного хитпоинта
    available=true,
    description="ПРОСТОЙ, УДОБНЫЙ, НАДЕЖНЫЙ ВЫБОР НОВИЧКА",
    junkName="ОБЛОМКИ АВТОМОБИЛЯ"
   }).

-record(
    uuid,
    {
        type,
        current
    }
).

migrate() ->
    mneser:createTables([
		{recolorPrice,
      [{attributes, record_info(fields, recolorPrice)},
       {disc_copies,  [node()]},
       {type, set}]}]),
    %foldl(Function, Acc, Table) -> NewAcc | transaction abort
    {atomic, _Val} = mnesia:transaction(
        fun() -> 
          mnesia:foldl(fun createPrices/2, ok, carClass) 
        end),
    {migrated, 50}.

createPrices(CarClass,_Acc) ->
    io:format("createPrices"),
    Fun =
        fun(Color) ->
            mnesia:write(#recolorPrice{
                 id = get_nt(recolorPrice),
                 carClassID = CarClass#carClass.id,
                 color = Color,
                 price = CarClass#carClass.price / 2,
                 realPrice = CarClass#carClass.realPrice / 2
                                     }  )
        end,
    lists:foreach(Fun, lists:seq(1, 8)),
    ok.

get_nt(Type) ->
	case qlc:e((qlc:q([X || X <- mnesia:table(uuid), X#uuid.type =:= Type])))  of
		[] -> mnesia:write(#uuid{current = 1, type = Type}),
			1;
		[OldUUID]->Answer = OldUUID#uuid.current + 1,
			mnesia:write(OldUUID#uuid{current=Answer}),
			Answer
	end.
