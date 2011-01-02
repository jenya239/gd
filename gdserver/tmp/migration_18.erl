-module(migration_18).
-include_lib("stdlib/include/qlc.hrl").
-define(MIGRATION_NUMBER, 18).
-export([migrate/0]).

-include("./lib/eunit/include/eunit.hrl").

-record(activity, {
	date,
  userid = none,
  action,
  result = ok
}).
-record(client, {
	userID,
  pID
}).
-record(car, {
	id,
  parentID,
  displayName,
  fileName,
  maxSpeed,
  acceleration,
  steering,
  minLevel
}).
-record(gameResult, {
	gameTypeID,
  carID,
  userID,
  date,
  time
}).
-record(gameReward, {
	id,
  gameTypeID,
  carID,
  name,
  time,
  experience
}).
-record(gameType, {
	id,
  routeID,
  direction,
  lapNumber,
  minLevel
}).
-record(level, {
	id,
  number,
  experience,
  trigger,
  message
}).
-record(race, {
	pid,
  id,
  gameTypeID,
  clientCount = 1
}).
-record(role, {
	name,
  permissions = []
}).
-record(route, {
	id,
  displayName,
  fileName,
  length,
  minLevel = 1
}).
-record(smsCode, {
	code,
  user,
  dateCreation,
  used = false,
  phone,
  dateActivation
}).
-record(user, {
	id,
  login,
  password,
  salt,
  email,
  displayName,
  image = "",
  currentCarID = 1,
  defaultColor = "ff0000",
  level = 1,
  experience = 0,
  roles = [],
  triggers = [],
  date = erlang:now(),
  vkontakteID = 0,
  nitroCount = 5,
  rating = 0
}).
-record(userReward, {
	userID,
  gameRewardID,
  time
}).
-record(uuid, {
	type,
  current
}).
-record(version, {
	number,
  result
}).
-record(message, {
	id,
  from,
  subject = none,
  date,
  receiver ,
  body
}).
-record(advertisement, {
	id,
  date = erlang:localtime(),
  message
}).
-record(itemClass, {
	id,
	name,
	category,
	description = "",
	price = 0,
	targetCars,
	usingType = slot, %slot Ð¸Ð»Ð¸ cast
	slot,
	usingCount = 0,
	minLevel = 1,
	maxLevel = 0,
	durability = 100,
	durabilityCoeff = 1,
	power = 0,
	speed = 0,
	braking = 0,
	controllability = 0,
	targetType = self %self, other, world
}).

createStructure() ->
	mneser:createTables([
		{activity,			[{attributes, record_info(fields, activity)},			{disc_only_copies, [node()]}, {type, bag}]},
		{car,						[{attributes, record_info(fields, car)},					{disc_copies, [node()]},      {type, set}]},
		{gameResult,		[{attributes, record_info(fields, gameResult)},		{disc_copies, [node()]},      {type, bag}]},
		{gameReward,		[{attributes, record_info(fields, gameReward)},		{disc_copies, [node()]},      {type, set}]},
		{gameType,			[{attributes, record_info(fields, gameType)},			{disc_copies, [node()]},      {type, set}]},
		{level,					[{attributes, record_info(fields, level)},				{disc_copies, [node()]},      {type, set}]},
		{race,					[{attributes, record_info(fields, race)},					{ram_copies,  [node()]},      {type, set}]},
		{role,					[{attributes, record_info(fields, role)},					{disc_copies, [node()]},      {type, set}]},
		{route,					[{attributes, record_info(fields, route)},				{disc_copies, [node()]},      {type, set}]},
		{user,					[{attributes, record_info(fields, user)},					{disc_copies, [node()]},      {type, set}]},
		{userReward,		[{attributes, record_info(fields, userReward)},		{disc_copies, [node()]},      {type, bag}]},
		{client,				[{attributes, record_info(fields, client)},				{ram_copies, [node()]},				{type, set}]},
		{uuid,					[{attributes, record_info(fields, uuid)},					{disc_copies, [node()]},      {type, set}]},
		{version,				[{attributes, record_info(fields, version)},			{disc_copies, [node()]},      {type, set}]},
		{smsCode,				[{attributes, record_info(fields, smsCode)},			{disc_copies,[node()]},				{type, set}]},
		{message,				[{attributes, record_info(fields, message)},			{disc_copies,[node()]},				{type, set}]},
		{advertisement,	[{attributes, record_info(fields, advertisement)},{disc_copies,[node()]},				{type, ordered_set}]},
		{itemClass, [{attributes, record_info(fields, itemClass)}, {disc_copies, [node()]}, {type, set}]}
	]).

get(What, Terms) -> utils:extractNamedList(What, Terms).

generateGameData(Terms) ->
	Levels = get(levels, Terms),
	Routes = get(routes, Terms),
	Cars = get(cars, Terms),
	Laps = get(laps, Terms),
	Users = get(users, Terms),
	Advs = get(advertisements, Terms),

  Transaction = fun() ->
    lists:foreach(fun(Level) ->
			mnesia:write(Level#level{message = utils:cp1251_to_utf8(Level#level.message)})
    end, Levels),

    lists:foreach(fun(Car) ->
			mnesia:write(element(1, Car))
		end, Cars),

		GameTypeRecords = [#gameType{
			id = dbUuid:get(gameType),
			routeID = (element(1, RG))#route.id,
			direction = element(1, DG),
			lapNumber = element(1, LG),
			minLevel = element(2, DG)
    } ||
			RG <- Routes,
			LG <- Laps,
			DG <- element(3, RG)
    ],

    NewGameRewardRecords = [
		{
			#gameReward{
				id=dbUuid:get(gameReward),
				gameTypeID = GTG#gameType.id,
				carID = (element(1, CG))#car.id,
				name = element(1, MG),
				time = trunc(1000*element(2, MG)*element(2, CG)*element(2, LG)),
				experience = trunc(element(3, MG)*element(3, LG))
			}, {
				(element(1, RG))#route.fileName,
				element(1, DG),
				element(1, LG),
				(element(1, CG))#car.displayName
			}
		} ||
			RG <- Routes,
			CG <- Cars,
			LG <- Laps,
			DG <- element(3, RG),
			MG <- element(2, RG),
			GTG <- GameTypeRecords,
			GTG#gameType.routeID =:= (element(1, RG))#route.id,
			GTG#gameType.lapNumber =:= element(1, LG),
			GTG#gameType.direction =:= element(1, DG)
		],

		NewRouteRecords = [
			(element(1, RG))#route{
				displayName = utils:cp1251_to_utf8((element(1, RG))#route.displayName)
			} || RG <- Routes
		],

    lists:foreach(fun(Element) ->
			mnesia:write(Element)
    end, NewRouteRecords),

    lists:foreach(fun(Element) ->
			mnesia:write(Element)
    end, GameTypeRecords),

    lists:foreach(fun(Element) ->
			mnesia:write((element(1, Element)))
    end, NewGameRewardRecords),

    lists:foreach(fun({Login, Password, DisplayName, DefaultColor}) ->
      addUser(Login, Password, DisplayName, DefaultColor)
    end, Users),

    lists:foreach(fun(Msg) ->
      mnesia:write(#advertisement{id = dbUuid:get(advertisement), message = Msg})
    end, Advs)
	end,
	{atomic, ok} = mnesia:clear_table(car),
	{atomic, ok} = mnesia:clear_table(route),
	{atomic, ok} = mnesia:clear_table(gameType),
	{atomic, ok} = mnesia:clear_table(gameReward),
	{atomic, ok} = mnesia:clear_table(level),
	{atomic, ok} = mnesia:clear_table(user),
	dbUuid:reset(gameReward),
	dbUuid:reset(gameType),
	dbUuid:reset(level),
	dbUuid:reset(car),
	dbUuid:reset(route),
	dbUuid:reset(user),
        crypto:start(),
	{atomic, ok} = mnesia:transaction(Transaction).

createGameData() ->
	{ok, Terms} = file:consult("src/db/migrations/migration_18.data"),
	generateGameData(Terms).

createGuestUser() ->
	mneser:writeRecord(#user{
		id = guest,
		login = "guest",
		password = "guest123",
		displayName = "Racer",
		defaultColor = "ff4433",
		currentCarID = 5
	}).
        
createTestUsers() ->
    {atomic, ok} = mnesia:transaction(fun() ->
        addUser("test", "mnogotestov", "Testuser", "ff1de2", test, 177017),
        addUser("test1", "mnogotestov", "Testuser1", "ff1de2", -2, 0),
        addUser("test2", "mnogotestov", "Testuser2", "ff1de2", -3, 0)
    end),
    io:format("Created test users~n", []).

addUser(Login, Password, DisplayName, DefaultColor) ->
    addUser(Login, Password, DisplayName, DefaultColor, dbUuid:get(user), 0).

addUser(Login, Password, DisplayName, DefaultColor, ID, VKID) ->
	Salt = crypto:rand_bytes(20),
	EncryptedPassword = crypto:sha_mac(Salt, Password),
	mnesia:write(#user {
		id = ID,
		login = Login,
		salt = Salt,
		password = EncryptedPassword,
		displayName = DisplayName,
		defaultColor = DefaultColor,
		roles = [],
                vkontakteID = VKID
	}).

createData() ->
	createGameData(),
	createGuestUser(),
        crypto:start(),
        createTestUsers().

migrate() ->
	createStructure(),
	createData(),
	{migrated, ?MIGRATION_NUMBER}.
	