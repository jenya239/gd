-module(registration).

-export([createUser/4,
         createUser_nt/4,
         tryUpdateRegistration/5,
         createCar_nt/4,
         createCar/4]).

-include("config.hrl").
-include("data.hrl").

tryUpdateRegistration(UserID, Nickname, CarClass, Color, City) ->
    TransactionResult = mnesia:transaction(fun() ->
        User = dbUser:getRecord_nt(id, UserID),
        Car = mneser:getRecord_nt(car, User#user.currentCarID),
        case users:checkTrigger(User, register) of
        false ->
            mnesia:abort({error, {alreadyRegistered, "[[youHaveAlreadyBeenRegistered]]"}});
        true ->
            NewTriggers = lists:subtract(User#user.triggers, [register]),
            {NewUser,NewCar} =
                      if Nickname == "" ->
                          NewNickname = utils:cutNickname(User#user.name),
                          {User#user{name=NewNickname, homeCity=City, currentCity=City, triggers=NewTriggers},
                           Car#car{carClassID=CarClass, color=Color} };
                      true ->
                          case dbUser:getRecord_nt(nickname, Nickname) of
                              {error, noNickname, _Nickname} ->
                                  {User#user{name=Nickname, homeCity=City, currentCity=City, triggers=NewTriggers},
                                    Car#car{carClassID=CarClass, color=Color}};
                              _User ->
                                  mnesia:abort({error, {nickIsAlreadyTaken, "[[nickNameIsAreadyTaken]]"}})
                          end
                      end,
            NickLength = utils:utf8length(NewUser#user.name),
            if NickLength > ?MAX_NICKNAME_LENGTH ->
                mnesia:abort({error, {nickIsTooLong, "[[nicknameIsTooLong]]"}});
            true ->
                ok
            end,

            mnesia:write(NewUser),
            mnesia:write(NewCar),
            {ok, NewUser}
        end
    end),
    case TransactionResult of
        {atomic, Result} ->
            Result;
        {aborted, Result} ->
            Result
    end.
    
createUser_nt(VkontakteID, FirstLastName, VkontakteOwnerID, Suffix) ->
    Nickname =
        case Suffix of
            0 -> FirstLastName;
            _ -> FirstLastName ++ " " ++ integer_to_list(Suffix)
        end,
    %City = random:uniform(2),
    City = 2,
    NewUser = #user{name=Nickname, currentCarID=0, homeCity=City, currentCity=City,
                    vkontakteID=VkontakteID,
                    rating=1000,
                    triggers=[register, {showClickOnCarTip, 3}, {showHowToDriveTip, 3}, {showNitroTip, 3}, {tutorialStage, 1}],
                    money=1000, referer=VkontakteOwnerID},
    case dbUser:add_nt(NewUser) of
        {ok, NewUserWithID} ->
            Car = createCar_nt(random:uniform(3), [], random:uniform(8), 5),
            mnesia:write(Car),
            mnesia:write(#userDetails{id = NewUserWithID#user.id,
                                            inventory = [],
                                            cars = [Car#car.id]}),
            ResUser = NewUserWithID#user{currentCarID = Car#car.id},
            mnesia:write(ResUser),            
            {ok, ResUser};
        {userAlreadyExsists, _Name} ->
            createUser_nt(VkontakteID, FirstLastName, VkontakteOwnerID, Suffix+1)        
    end.
    
createUser(VkontakteID, FirstLastName, VkontakteOwnerID, Suffix) ->
    {atomic, Result} = mnesia:transaction(fun() -> 
        createUser_nt(VkontakteID, FirstLastName, VkontakteOwnerID, Suffix)
    end),
    Result.

createCar(CarClassID, Upgrades, Color, NitroCount) ->
    {atomic, Result} = mnesia:transaction(fun() ->
        createCar_nt(CarClassID, Upgrades, Color, NitroCount)
    end),
    Result.


createCar_nt(CarClassID, Upgrades, Color, NitroCount) ->
    CarClass = mneser:getRecord_nt(carClass, CarClassID),
    #car{id=dbUuid:get(car),
         carClassID = CarClassID,
         durability = CarClass#carClass.durabilityMax,
         durabilityMax = CarClass#carClass.durabilityMax,
         upgrades = Upgrades,
         fuel = CarClass#carClass.fuelCapacity,
         color = Color,
         nitroCount = NitroCount}.