-record(
    activity, 
    {
        date,
        userid = none,
        action,
        result = ok
    }
).

-record(
    buyItem,
    {
    	itemClassID,
    	itemPrice,
    	itemRealPrice,
    	oldUserMoney,
    	oldUserRealMoney,
    	newUserMoney,
    	newUserRealMoney
    }
).

-record(
    city,
    {
       id,
       score,
       minRating,
       minScore
    }).
    
-record(
    client,
    {
        userID,
        pID
    }
).

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
    wearInfo,
    {
        id,
        userID,
        displayName,
        delta,
        durability,
        durabilityMax
    }
).

-record(
    upgradeInfo,		
    {
        userID=-1,
        speed=0,
        power=0,
        controllability=0,
        braking=0
    }
).

-record(
    carInfo,
    {
    car,
    carClass,
    sellPrice,
    repairPrice=0,
    capitalRepairPrice=0,
    carUpgrade=#upgradeInfo{},
    recolor=[]
    }).

-record(
    clientInfo,
    {
        clientID,
        userID,
        displayName,
        homeCity,
        currentCity,
        carID,
        carFileName,
        carColor,
		level,
        %todo think about this approach              
        startTime = 0,
        lastTime = 0,
        bestTime = 0,
        loadingProgress = 0,
        loadingStartTimestamp,
        readiness = none, %none, loaded, initialized
        upgrades = #upgradeInfo{},
        userInfo
    }
).

-record(
    friendInfo,
    {
        userID,
        displayName,
        level,
        city,
        rating,
        vkontakteID,
        isWashed = false
    }
 ).

-record(
    friendAction,
    {
        userID,
        friendID,
        washTime = 0
    }
 ).

-record(
    gasInfo,
    {
        fuelPrice,
        maxFuelCount=0,
        jobOffers = []
    }
).

-record(
    item,
    {
        id, 
        itemClassID, 
        durability, 
        durabilityMax
    }
).

-record(
	itemClass,
	{
		id,
		name,
        junkName = "Обломки",
		category,
		description = "",
		price = 0,
        realPrice = 0,
        repairPrice = 1.3,
		targetCars,
		usingType = slot, %slot, cast, charge
		slot,
		usingCount = 0,
		minLevel = 1,
		maxLevel = 0,
		durabilityMax = 100,        
		power = 0,
		speed = 0,
		braking = 0,
		controllability = 0,
		targetType = self, %self, other, world
        available = true
    }
).

-record(
    level,
    {
        id,
        number,
        experience,
        money = 0,
        realMoney = 0,
        itemID,
        nitroID,
        message
    }
).    
    
-record(
    lobbyInfo, 
    {
        id,
        creatorName,
        creatorClientID,
        routeID,
        lapNumber,
        direction,
        playerCountBlue = 0,
        playerCountRed = 0,
        playerMax,
        status = enum_status_hall,
        timerLength = 30000,
        allowedCarID = -1,
        timerEnd = 0,
        timerCheckingSlow = 0,
        league=1,
        stake=0,
        creatorCarName="megaCar",
        creatorRating=0,
        type = normal
    }
).
    
-record(
    lobbyResult,
    {
        clientID,
        userID,
        displayName,
        time,
        position, 
        oldRating, 
        newRating,
        score,
        fuel,
        money,
        finished = false,
		experience = 0,
        city
    }
).

-record(
    postMessage,
    {
        id,
        senderID,
        recepientID,
        timeStamp,
        item,
        money,
        sellPrice,
        comment
    }
).

-record(
    postMessageInfo,
    {
        id,
        fromNick,        
        itemInfo,
        timeStamp,
        money,
        sellPrice,
        senderID,
        comment
    }
).
    
-record(
    ratingInfo,
    {
        userID,
        displayName,
        rating,
        position
    }
).

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
	recolorInfo,
	{
		carDisplayName,
		colorID,
		price,
		realPrice
	}).

-record(
    route,
    {
        id,
        displayName,
        fileName,
        length,
        minLevel = 1,
        maxLevel = 100,
        moneyPrize = 5000,
        isHomeCity = true,
        isBattleCity = true,
        difficulty = 1
    }
).

-record(
    systemInfo,
    {
        timestamp = 0, % in minutes
        cpuLoad, 
        memUsage,
        memTotal
    }
).

-record(
    tip,
    {
        id,
        message,
        minLevel,
        maxLevel,
        available=true
    }
).

-record(
    trafficStat,
    {
       time,
       name,
       size
    }
).

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
    homeCity,
    currentCity,
    money=0,
    realMoney=0,
    referer=0,
    duelWin=0,
    duelCount=0
    }).
 
-record(
    userInfo, 
    {
        user,
        car,
        expPrevLevel,
        expNextLevel,
        inventory = [],
        equipment = [],
        cars=[],
        upgradeInfo = #upgradeInfo{},
        carSlots = 3,
        isWashed
    }
).
    
-record(
    userDetails,
    {
        id,
        inventory = [],
        cars = [],
        carSlots = 3
    }
).

-record(
    userProgress, 
    {
        userID,
        leveling = [{1, 0}],
        onlineTime = 0,
        kilometers = 0,
        worksCounter = 0,
        invites = 0,
        activeInvites = 0,
        realPurchases = 0
    }
).

-record(
    userState, 
    {
        userID,
        state
    }
).

-record(
    userTransferState, 
    {
        citySrc,
        cityDst,
        type,
        arrivalTime
    }
).

-record(
    stopList,
    {
        id,      %  id забаненного пользователя
        time,    %  время когда его забанили
        period,  %
        admin    %  id админа который забанил
    }
).

-record(
    session,
    {
        id,
        userID,
        startTime,
        endTime,
        kilometers = 0,
        worksCounter = 0
    }
).

-record(
    scoreHistory,
    {
        date,
        table
    }
).

-record(
    scheduledTask,
    {
        id,
        dateTime
    }
).

-record(
    onlineStats,
    {
        timestamp = 0, % in hours
        min = 0,
        max = 0
    }
).
    
-record(
    uuid, 
    {
        type, 
        current
    }
).

-record(
    userDailyScore,
    {
        userID,
        score,
        homeCity
    }
).

-record(
    userDailyScoreInfo,
    {
        userID,
        displayName,
        score,
        homeCity
    }
).

-record(
    version,
    {
        number,
        result
    }
).
    
-record(
    message,
    {
        id,
        from,
        subject = none,
        date,
        receiver ,
        body    
     }
).

-record(
    advertisement,
    {
        id,
        date = erlang:localtime(),
        message
    }
).

-record(
    global,
    {        
        key,
        value
    }
).

-record(
    vkontakteInfo,
    {
        userAppBalance
    }
).

-record(
    workOffer,
    {
        id,
        time,
        fuel,
        message
    }
).    

-record(
    workInfo,
    {
        offerID,
        startTime,
        timerRef
    }
).

