-module( migration_73 ).

-include_lib( "stdlib/include/qlc.hrl" ).

-export( [ migrate/0 ] ).

-record( carClass, {
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
  junkName="ОБЛОМКИ АВТОМОБИЛЯ",
  count=9999999
 }).

migrate() ->
  Fun = fun( OldRecord ) ->
    EmptyNewRecord = #carClass{},
    erlang:append_element( OldRecord, element( tuple_size( EmptyNewRecord ), EmptyNewRecord ) )
  end,
  { atomic, ok } = mnesia:transform_table( carClass, Fun, record_info( fields, carClass ), carClass ),
  { migrated, 73 }.