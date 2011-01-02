-module(migration_46).
-include_lib("stdlib/include/qlc.hrl").
-export([migrate/0]).

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
    carOld,
    {
    id,
    parentID,       % will be lost
    displayName,
    fileName,
    maxSpeed,
    acceleration,
    steering,
    minLevel,
    fuelConsumption = 0,
    fuelCapacity = 40
    }).

-record(
    car,
    {
    id,
    carClassID,
    durability,         % исправность авто
    durabilityMax,      % ресурс автомобиля
    upgrades,           % список id апгрейдов установленных на автомобиль
    fuel,               % количество бензина в баке данного авто в данный момент
    color,              % номер картинки для этого авто
    nitroCount
    }).

-record(
    oldUser,
    {
    id,
    login,
    password,
    salt,
    email,
    displayName,
    image = "",
    currentCarID = 1,
    defaultColor = 1,
    level = 1,
    experience = 0,
    roles = [],
    triggers = [],
    date = erlang:now(),
    vkontakteID = 0,
    nitroCount = 5,
    rating = 0,
    city = undefined,
    money = 1000,
    realMoney = 0,
    fuelVolume = 40
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
    realMoney=0
    }).

-record(
    oldUserDetails,
    {
    id,
    inventory = [],
    equipment = []
   }).

-record(
    userDetails,
    {
    id,
    inventory = [],
    cars = []
   }).

-record(
    uuid,
    {
        type,
        current
    }
).

migrate() ->
    % io:format("start~n"),
    createCarClassTable(),
    % io:format("create car class~n"),
    createCarClassRecords(),
    fix(carClass),
    % io:format("create records~n"),
    mnesia:delete_table(car),
    % io:format("delete old car~n"),
    createCarTable(),
    % io:format("create new car~n"),
    deleteOldUsers(),
    % io:format("delete users~n"),
    createCarRecords(),
    % io:format("create cars~n"),
    transformUsers(),
    % io:format("transform users ~n"),
    transformUserDetails(),
    % io:format("transform users details ~n"),
    removeDeprecatedTables(),
    {migrated, 46}.

createCarClassTable() ->
    mneser:createTables([
		{carClass,
      [{attributes, record_info(fields, carClass)},
       {disc_copies,  [node()]},
       {type, set}]}]).

createCarTable() ->
    mneser:createTables([
		{car,
      [{attributes, record_info(fields, car)},
       {disc_copies,  [node()]},
       {type, set}]}]).

createCarClassRecords() ->
    Cars = mneser:getAllRecords(car),
    Fun = fun (Car) ->
            ok = writeRecord(transformCarToCarClass(Car))
          end,
    lists:foreach(Fun,Cars).

transformCarToCarClass(CarOld) ->
     Car = utils:changeRecordName(CarOld, carOld),
     #carClass{
         id = Car#carOld.id,
         displayName = Car#carOld.displayName,
         fileName = if (Car#carOld.minLevel == 1) -> "focus";
                       true -> Car#carOld.fileName
                    end,
         available = (Car#carOld.minLevel == 1),
         speedView = Car#carOld.maxSpeed,
         breakingView = 0,
         powerView = Car#carOld.acceleration,
         controllabilityView = Car#carOld.steering,

         fuelCapacity = Car#carOld.fuelCapacity,     % -- вместимость бака
         fuelConsumption = Car#carOld.fuelConsumption,
         minLevel = Car#carOld.minLevel}.

createCarRecords() ->
    io:format("Creating cars... ~n", []),
    Users = mneser:getAllRecords(user),
    Fun = fun(UserOld) ->
            User = utils:changeRecordName(UserOld, oldUser),
            {Inv,Equipment} = getUserInventoryAndEquipment(User#oldUser.id),
            Car = createCarForUser(User,Equipment),
            UserDetails = #oldUserDetails{id = User#oldUser.id,
                                         inventory = Inv,
                                         equipment = [Car#car.id] },
            ok = writeRecord(utils:changeRecordName(UserDetails,userDetails) ),
            ok = writeRecord(Car),
            ok = writeRecord(utils:changeRecordName(User#oldUser{currentCarID = Car#car.id},user) )
          end,
    lists:foreach(Fun,Users).

createCarForUser(User,Equipment) ->
    createCar(User#oldUser.currentCarID,
                    Equipment,
                    User#oldUser.defaultColor,
                    User#oldUser.nitroCount).

createCar(CarClassID, Upgrades, Color, NitroCount) ->
    CarClass = mneser:getRecord(carClass, CarClassID),
    #car{id=dbUuid:get(car),
         carClassID = CarClassID,
         durability = CarClass#carClass.durabilityMax,
         durabilityMax = CarClass#carClass.durabilityMax,
         upgrades = Upgrades,
         fuel = CarClass#carClass.fuelCapacity,
         color = Color,
         nitroCount = NitroCount}.

% удаляем пользователей у которых не проставлен вконтакте id
deleteOldUsers() ->
    io:format("~p Getting old (non-vkontakte) users... ~n", [utils:nowString()]),
    UserIDs = getUserIDs(vkontakteId, 0),
    io:format("NVK users count: ~p ~n", [length(UserIDs)]),
    io:format("~p Deleting indices... ~n", [utils:nowString()]),
    mnesia:del_table_index(user,vkontakteID),
    mnesia:del_table_index(user,rating),
    mnesia:del_table_index(user,displayName),
    io:format("~p Deleting old (non-vkontakte) users... ~n", [utils:nowString()]),
    lists:foldl(fun(UserID, Count) ->
        deleteUser(UserID),
        if Count rem 1 =:= 100 ->
            io:format("~p count = ~p... ~n", [utils:nowString(), Count]);
        true -> ok end,
        Count + 1
    end, 0, UserIDs),
    io:format("~p Users deleted~n", [utils:nowString()]).

transformUserDetails() ->
    {atomic, ok} = mnesia:transform_table(userDetails, ignore, record_info(fields, userDetails), userDetails).

transformUsers() ->
    Fun = fun(A) ->
            OldUser = utils:changeRecordName(A, oldUser),
            #user{
                id = OldUser#oldUser.id,
                name = OldUser#oldUser.displayName,
                currentCarID = OldUser#oldUser.currentCarID,
                level = OldUser#oldUser.level,
                experience = OldUser#oldUser.experience,
                roles = OldUser#oldUser.roles,
                triggers = OldUser#oldUser.triggers,
                date = OldUser#oldUser.date,
                vkontakteID = OldUser#oldUser.vkontakteID,
                rating = OldUser#oldUser.rating,
                city = OldUser#oldUser.city,
                money = OldUser#oldUser.money,
                realMoney = OldUser#oldUser.realMoney
            }
          end,
    io:format("Transforming users... ~n", []),
    {atomic, ok} = mnesia:transform_table(user, Fun, record_info(fields, user), user),
    {atomic, ok} = mnesia:add_table_index(user, rating),
    {atomic, ok} = mnesia:add_table_index(user, vkontakteID),
    {atomic, ok} = mnesia:add_table_index(user, name).

removeDeprecatedTables() ->
    mnesia:delete_table(smsCode),
    mnesia:delete_table(carUpgrade),
    mnesia:delete_table(gameType),
    mnesia:delete_table(gameReward),
    mnesia:delete_table(race),
    mnesia:delete_table(role),
    mnesia:delete_table(userReward),
    mnesia:delete_table(gameResult),
    mnesia:delete_table(client).

% dbItem.erl

getUserInventoryAndEquipment_nt(UserID) ->
	Result = qlc:e(qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.id =:= UserID])),
	case Result of
		[UserDetailsOld] ->
      UserDetails = utils:changeRecordName(UserDetailsOld, oldUserDetails),
			{UserDetails#oldUserDetails.inventory, UserDetails#oldUserDetails.equipment};
		[] ->
			{[], []}
	end.

getUserInventoryAndEquipment(UserID) ->
	{atomic, Result} = mnesia:transaction(
        fun() ->
            getUserInventoryAndEquipment_nt(UserID)
        end),
  Result.
% dbUuid
fix(Type) ->
	{atomic, ok} =
		mnesia:transaction(
		fun() ->
			N = lists:max([1]++mnesia:all_keys(Type)),
			mnesia:write( #uuid{type=Type,current=N} )
		end).

% mneser
deleteRecords(Table, Nth, Value) ->
%	{atomic, Val} = mnesia:transaction(fun() ->
%		Records = qlc:e(qlc:q([ X || X <- mnesia:table( Table ), element( Nth, X ) =:= Value ])),
%		lists:foreach( fun( X ) -> mnesia:delete_object( X ) end, Records )
%	end), Val.
    Val = mnesia:activity(async_dirty, fun() ->
		Records = qlc:e(qlc:q([ X || X <- mnesia:table( Table ), element( Nth, X ) =:= Value ])),
		lists:foreach( fun( X ) -> mnesia:delete_object( X ) end, Records )
	end), Val.

writeRecord(Record) ->
	{atomic, Executed} = mnesia:transaction(fun() ->
        mnesia:write(Record)
	end),
	Executed.

  % dbUser
deleteUser(Id) ->
		ok = deleteRecords(user, 2, Id),
		% ok = deleteRecords(activity, 3, Id),
		%ok = deleteRecords(carUpgrade, 3, Id),
		%ok = deleteRecords(gameResult, 4, Id),
		ok = deleteRecords(message, 3, Id),
		ok = deleteRecords(message, 6, Id),
		ok = deleteRecords(postMessage, 3, Id),
		ok = deleteRecords(postMessage, 4, Id),
		ok = deleteRecords(userDetails, 2, Id).

 getUserIDs(vkontakteId, VkontakteId) ->
	mneser:do(qlc:q([element(2,X) || X <- mnesia:table(user),
                   is_integer(element(2,X) ),
                   element(16,X) =:= VkontakteId])).