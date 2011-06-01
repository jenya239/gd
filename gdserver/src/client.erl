-module(client).

-behaviour(gen_server).

-export([start/3]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
        
-export([getClientInfo/1, 
        getState/1, getStateRecord/1,  
        checkAndUpdateFirstLogin/1,
        preprocess/2,
        process/2]).
        
-include("config.hrl").
-include("data.hrl").
-include("lib/eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MAX_MESSAGES_IN_QUEUE, 1000).
        
-record(state, {name, socket, socketSenderPID, clientID, userID, currentCity, lobbyPID, workInfo, pingTimer, lastPingTime, session, friends={0, []}}).
        
start(Socket, SocketSenderPID, ClientID) ->    
    ProcessName = list_to_atom(?MODULE_STRING ++ integer_to_list(ClientID)),
    gen_server:start({local, ProcessName}, ?MODULE, [Socket, SocketSenderPID, ClientID], []).

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([Socket, SocketSenderPID, ClientID]) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    process_flag(trap_exit, true),
    link(SocketSenderPID),
    
    {ok, PingTimer} = timer:send_interval(?CLIENT_SOCKET_TIMEOUT div 4, self(), pingTimer),
    
    State = #state{name=notAuthorized, socket=Socket, socketSenderPID=SocketSenderPID,
                    clientID=ClientID, userID=undefined, 
                    pingTimer=PingTimer, lastPingTime=utils:now()},
                    
    sendMessage(State, {serverTime, utils:now(), dbAdvertisement:getLast()}),
    
    log:write(debug, ?MODULE_STRING, ClientID, "process has been started ~n", []),
    {ok, State}.

%must NOT be used in game processes, use by YAWS and TESTS ONLY
getState(ClientPID) ->
    gen_server:call(ClientPID, getState).

getStateRecord(ClientPID) ->
    gen_server:call(ClientPID, getStateRecord).

%must NOT be used in game processes, use by YAWS and TESTS ONLY
getClientInfo(ClientPID) ->
    gen_server:call(ClientPID, getClientInfo).
        
%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(getState, _From, State) ->
    {reply, State#state.name, State};

handle_call(getStateRecord, _From, State) ->
    {reply, State, State};

handle_call(getClientInfo, _From, State) ->
    Reply = if State#state.name == notAuthorized ->
        {error, {notAuthorized, "Cannot get clientInfo for not authorized users"}};
    true ->
        getClientInfo(userID, State#state.userID, State#state.clientID)
    end,
    
    {reply, Reply, State};
       
handle_call(_Message, _From, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------
handle_cast(_Message, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% Handle infos
%%-----------------------------------------------------------------------------
handle_info(pingTimer, State) ->
    Result = if is_number(State#state.lastPingTime) ->
        TimeDiff = utils:now() - State#state.lastPingTime,
        if TimeDiff >= ?CLIENT_SOCKET_TIMEOUT ->
            log:write(info, ?MODULE_STRING, State#state.clientID, "Closing socket by timeout ~n", []),
            {stop, normal, State};
        true ->
            {message_queue_len, MessagesQueueLen} = erlang:process_info(self(), message_queue_len),
            if MessagesQueueLen >= ?MAX_MESSAGES_IN_QUEUE ->
                log:write(info, ?MODULE_STRING, State#state.clientID, "Exiting because message queue overflow occured ~n", []),
                {stop, normal, State};
            true->
                {noreply, State}
            end
        end;
    true ->
        {noreply, State}
    end,
    Result;
        
handle_info({newLevel, NewLevel}, State) ->
    dbUserProgress:newLevel(NewLevel, State#state.session),
    {noreply, State};

handle_info({addKilometers, LobbyInfo}, State) ->
    NewSession = dbUserProgress:addKilometers(State#state.session, LobbyInfo),
    {noreply, State#state{session=NewSession}};

handle_info(incWorksCounter, State) ->
    NewSession = dbUserProgress:incWorksCounter(State#state.session),
    {noreply, State#state{session=NewSession}};

handle_info({'EXIT', From, Reason}, State) ->
    if From == self() ->
        case Reason of
            {error, {Reason, _}} when Reason == closed orelse Reason == timeout ->
                {stop, normal, State};
            _Other ->
                {stop, Reason, State}
        end;
    true ->
        {noreply, State}
    end;

handle_info({ping}, State) ->    
    {noreply, State#state{lastPingTime=utils:now()}};

handle_info(Event, State) ->    
    log:writeMessage(debug, ?MODULE_STRING, State#state.clientID, State#state.name, Event, "<-- "),
    client:preprocess(Event, State).

%%-----------------------------------------------------------------------------
%% Code change
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Terminate
%%-----------------------------------------------------------------------------
terminate(Reason, State) ->
    if State#state.currentCity =/= undefined ->
        cityManager:removeClient(State#state.currentCity, State#state.clientID);
    true -> ok end,
    
    timer:cancel(State#state.pingTimer),
    
    if State#state.lobbyPID /= undefined ->
            lobby:removeClient(State#state.clientID, State#state.lobbyPID);
       true ->
           ok
    end,
    
    Session = State#state.session,
    if Session =/= undefined ->
        UserID = State#state.userID,
        EndTime = utils:now(),
        StartTime = Session#session.startTime,
        dbSession:write(Session#session{endTime=EndTime}),
        dbUserProgress:addOnlineStats(UserID, EndTime - StartTime, Session#session.kilometers, Session#session.worksCounter);
    true -> ok end,
    
    if State#state.socket =/= nosocket ->
        if Reason =/= shutdown ->
            serverSocket:removeSession(State#state.userID, State#state.clientID);
        true -> ok end,
        
        gen_tcp:close(State#state.socket),
        exit(State#state.socketSenderPID, kill);
    true -> ok end.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
 
% отладочная заглушка для unit test'ов
sendMessage(#state{socket=nosocket}, Message) ->    
    protocol:serialize(Message),
    ok;

% отправляем одно сообщение
sendMessage(State, Message) ->
    State#state.socketSenderPID ! Message.

checkAndUpdateFirstLogin(User) -> 
    case users:checkTrigger(User, showIntro) of
        true -> 
            users:deleteTrigger(User#user.id, showIntro),
            true;
        false ->
            false
    end.
        
preprocess(Event, State) ->
    case Event of
        {socketClosed, _} ->
            changeState(State, State#state{name=socketClosed, lobbyPID=undefined});
        {killSelf} ->
            {stop, normal, State};
        {killself, shutdown} ->
            {stop, shutdown, State};
        AnotherEvent ->
            client:process(AnotherEvent, State)
    end.
    
changeState(OldState, NewState) ->
    log:write(debug, ?MODULE_STRING, OldState#state.clientID, OldState#state.name, "==>> ~w ~n", [NewState#state.name]),
    if ((OldState#state.name == registering) or (OldState#state.name == notAuthorized)) andalso (NewState#state.name == notConnected) ->
        ClientInfo = getClientInfo(userID, NewState#state.userID, NewState#state.clientID),
        cityManager:addClient(ClientInfo, self()),
        self() ! {get, tipInfo},
        self() ! {get, cities},
        ActiveInvites = (dbUserProgress:getOrCreate(NewState#state.userID))#userProgress.activeInvites,
        sendMessage(NewState, {get, invitesInfo, ok, ActiveInvites});
    true -> ok end,
    IsOldStateLobbyGroup = inGroupState(lobbyGroup, OldState),
    IsNewStateLobbyGroup = inGroupState(lobbyGroup, NewState),
    if 
        IsOldStateLobbyGroup,
        not IsNewStateLobbyGroup ->
            if 
                is_pid(OldState#state.lobbyPID) ->
                    lobby:removeClient(OldState#state.clientID, OldState#state.lobbyPID);
                true ->
                    ok
            end;
        true ->
            ok
    end,    

    if NewState#state.name == socketClosed ->
        dbActivity:register(OldState#state.userID, socketClosed, ok),
        log:write(debug, ?MODULE_STRING, OldState#state.clientID, "process finished by socketClosed ~n", []),
        {stop, normal, NewState};
    true ->
        {noreply, NewState}
    end.
    
%%-----------------------------------------------------------------------------
%% Event handlers
%%-----------------------------------------------------------------------------
-define(STATES_AUTHORIZED(State), (State)#state.name /= notAuthorized andalso (State)#state.name /= registering).
-define(STATES_LOBBY(State), (State)#state.name == lobbyHall orelse (State)#state.name == lobbyRacing).

process({chatMessage, city, Text, ""}, State) when ?STATES_AUTHORIZED(State) ->
    case dbUser:canChat(State#state.userID) of
        true ->
             CityID = State#state.currentCity,
             cityManager:getCityChatByID(CityID) ! {chatMessage, city, Text, State#state.clientID};
        _ -> ok 
    end,
    {noreply, State};

process({chatMessage, trade, Text, ""}, State) when ?STATES_AUTHORIZED(State) ->
     case dbUser:canChat(State#state.userID) of
        true ->
            CityID = State#state.currentCity,
            cityManager:getCityChatByID(CityID) ! {chatMessage, trade, Text, State#state.clientID};
        _ -> ok
     end,
    {noreply, State};

process({otherChatMessage, Channel, Text, system, HomeCity, Nick, TimeStamp}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {otherChatMessage, Channel, Text, system, HomeCity, Nick, TimeStamp,false,false,false}),
    {noreply, State};

process({otherChatMessage, Channel, Text, -1, HomeCity, Nick, TimeStamp}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {otherChatMessage, Channel, Text, -1, HomeCity, Nick, TimeStamp,false,false,false}),
    {noreply, State};

process({otherChatMessage, Channel, Text, UserID, HomeCity, Nick, TimeStamp}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, UserID),
    sendMessage(State, {otherChatMessage, Channel, Text, UserID, HomeCity, Nick, TimeStamp, users:checkRole(User, admin), users:checkRole(User, rj), users:checkRole(User, warcity)}),
    {noreply, State};
    
process({chatSystemMessage, Text}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, State#state.userID),
    case users:checkRole(User, dev) of
        true ->
             cityManager:getCityChatByID(1) ! {chatMessage, city, Text, system},
             cityManager:getCityChatByID(2) ! {chatMessage, city, Text, system},
             cityManager:getCityChatByID(3) ! {chatMessage, city, Text, system};
        _ -> ok 
    end,
    {noreply, State};

process({ban, ID, Time}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, State#state.userID),
    case users:checkRole(User, admin) of
        true  ->
            BannedUser = dbUser:getRecord(id, ID),
            CityID = User#user.currentCity,
            case dbUser:canChat(ID) of 
                true ->
                    mneser:writeRecord(#stopList{id=ID, time = utils:now(), period = Time, admin = State#state.userID}),
                    cityManager:getCityChatByID(CityID) ! {chatMessage, city,
                              User#user.name ++ " [[banned]] " ++ BannedUser#user.name ++ " [[on]] " ++ utils:formatTime(Time), system};
                false ->
                    self() ! {otherChatMessage, city, BannedUser#user.name ++ " [[alreadyBanned]]", system, 0, "[[systemMessage]]", utils:now()}
            end;
        false ->
            ok
    end,
    {noreply, State};

process({unban, Id}, State) when ?STATES_AUTHORIZED(State) ->
	mnesia:transaction( fun() ->
		Actor = dbUser:getRecord_nt(id, State#state.userID),
		case users:checkRole(Actor, admin) of
			true ->
				ActiveBans = qlc:e(qlc:q(
						[X || X <- mnesia:table(stopList),
						 X#stopList.id =:= Id, X#stopList.time + X#stopList.period > utils:now()])),
				case ActiveBans of
					[] ->
						sendMessage(State, {unban, error, {notBanned, "[[notBanned]]"}});
					_ ->
						LastBan = lists:last( ActiveBans ),
						case LastBan#stopList.admin == Actor#user.id of
							true ->
								mnesia:delete_object( LastBan ),
								sendMessage(State, {unban, ok, "[[banDeleted]] " ++ utils:formatTime(LastBan#stopList.period)});
							false ->
								sendMessage(State, {unban, error, {noPermission, "[[noPermission]]"}})
						end
				end
		end
	end ),
  {noreply, State};  

process({changeNick, UserId, NewNick, ForFree}, State) when ?STATES_AUTHORIZED(State) ->
	Moder = dbUser:getRecord(id, State#state.userID),
	case users:checkRole( Moder, admin ) of
		true ->
			NickLength = utils:utf8length( NewNick ),
			case NickLength < 2 of
				true ->
					sendMessage(State, {changeNick, error, {nickIsTooShort, "[[nicknameIsTooShort]]"}});
				false ->
					case NickLength > ?MAX_NICKNAME_LENGTH of
						true ->
							sendMessage(State, {changeNick, error, {nickIsTooLong, "[[nicknameIsTooLong]]"}});
						false ->
							mnesia:transaction(fun() ->
								case ForFree of
									true ->
										case is_record( dbUser:getRecord_nt(nicknameIgnoreCase, NewNick), user ) of
											true -> sendMessage(State, {changeNick, error, {alreadyExists, "[[nicknameIsAreadyTaken]]"}});
											false ->
												User = dbUser:getRecord_nt(id, UserId),
												mnesia:write( User#user{ name = NewNick } ),
												dbActivity:register_nt(UserId,  {changeNick, User#user.name, NewNick, Moder#user.id, 0}, ok),
												sendMessage(State, {changeNick, ok})
										end;
									false ->
										User = dbUser:getRecord_nt(id, UserId),
										case User#user.realMoney >= 10 of % todo вынести константу
											true ->
												case is_record( dbUser:getRecord_nt(nicknameIgnoreCase, NewNick), user ) of
													true -> sendMessage(State, {changeNick, error, {alreadyExists, "[[nicknameIsAreadyTaken]]"}});
													false ->
														mnesia:write( User#user{ name = NewNick, realMoney = User#user.realMoney - 10 } ),
														dbActivity:register_nt(UserId,  {changeNick, User#user.name, NewNick, Moder#user.id, 10}, ok),
														sendMessage(State, {changeNick, ok})
												end;
											false -> sendMessage(State, {changeNick, error, {notEnoughMoney, "[[notEnoughMoney]]"}})
										end
								end
							end)
					end
			end;
		false ->
				ok
	end,
	{noreply, State};

process({finishWork, timerEvent},  #state{name=work} = State ) ->
    UserID = State#state.userID,
    case fuel:finishWork(UserID, State#state.workInfo) of
        {ok,_Message} ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            GasInfo = fuel:getGasInfo(State#state.userID),
            sendMessage(State, {finishWork, ok, GasInfo, UserInfo}),
            changeState(State, State#state{name=notConnected, workInfo = undefined});
        % unused pattern
        % todo refactor
        {error, {Reason, Message}} ->
            sendMessage(State, {finishWork, error, {Reason, Message}}),
            {noreply, State}
    end;

process({cancelWork}, #state{name=work} = State ) ->
    UserID = State#state.userID,
    case fuel:cancelWork(UserID, State#state.workInfo) of
        {ok,_Message} ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            GasInfo = fuel:getGasInfo(State#state.userID),
            sendMessage(State, {cancelWork, ok, GasInfo, UserInfo}),
            changeState(State, State#state{name=notConnected, workInfo = undefined});
        % unused pattern
        % todo refactor
        {error, {Reason, Message}} ->
            sendMessage(State, {cancelWork, error, {Reason, Message}}),
            {noreply, State}
    end;
    
process({get, secureUserInfo, UserID}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, UserID),
    UserInfo = users:getUserInfo(User),
    sendMessage(State, {get, secureUserInfo, ok, UserInfo}),
    {noreply, State};
    
process({changeTrigger, Trigger, Delta}, State) when ?STATES_AUTHORIZED(State) ->
    Delta_ = case Trigger of 
        tutorialStage -> 1;
        showInvitesReward -> -1;
        _Other -> Delta
    end,
    case lists:member(Trigger, [showClickOnCarTip, showHowToDriveTip, showNitroTip, tutorialStage, showInvitesReward]) of
        true ->
            UserID = State#state.userID,
            NewTriggerValue = users:changeTrigger(UserID, Trigger, Delta_),
            if Trigger =:= tutorialStage andalso NewTriggerValue =:= 3 ->
                dbUser:addMoney(UserID, dbGlobal:get(tutorialReward));
            true -> ok end, 
            if Trigger =:= tutorialStage ->
                dbActivity:register(UserID,  {changeTrigger, {Trigger, NewTriggerValue}}, ok);
            true -> ok end,
            User = dbUser:getRecord(id, UserID),
            UserInfo = users:getUserInfo(User),
            sendMessage(State, {changeTrigger, ok, UserInfo});
        false ->
            sendMessage(State, {changeTrigger, error, {error, "Protocol error"}})
    end,
    
    {noreply, State};

process({cleanCar, UserID}, State) when ?STATES_AUTHORIZED(State) ->
    NewState = case dbUser:cleanCar(State#state.userID, UserID) of 
        {ok, Money} ->
            {LastFriendsTime, FriendInfos} = State#state.friends,
            NewFriendInfos = markFriendCarCleaned(UserID, FriendInfos),
            sendMessage(State, {cleanCar, ok, Money}),
            State#state{friends={LastFriendsTime, NewFriendInfos}};
        {error, {_R, _M} = RM} ->
            sendMessage(State, {cleanCar, error, RM}),
            State
    end,
    
    {noreply, NewState};

process({get, friendInfos, FriendIDs}, State) when ?STATES_AUTHORIZED(State) ->
    Now = utils:now(),
    From = self(),
    {LastFriendsTime, LastFriendInfos} = State#state.friends,
    case Now > LastFriendsTime + 60*60*1000 of true ->
        spawn(fun() ->
            %log:write("getting friends from db~n", []),
            FriendInfos = lists:foldl(fun(FriendID, FriendInfosAcc) ->
                case dbUser:getRecord(vkontakteID, FriendID) of
                    User when is_record(User, user) ->
                        IsWashed = dbUser:isFriendCarClean(State#state.userID, FriendID),
                        NewFriendInfo = #friendInfo{vkontakteID=FriendID, userID=User#user.id, displayName=User#user.name, level=User#user.level, city=User#user.homeCity, rating=User#user.rating, isWashed=IsWashed},
                        [NewFriendInfo | FriendInfosAcc];
                    _Else ->
                        FriendInfosAcc
                    end
            end, [], FriendIDs),
            From ! {get, friendInfos, ok, FriendInfos, true}
        end);
    false ->
        %log:write("getting friends from cache~n", []),
        From ! {get, friendInfos, ok, LastFriendInfos, false}
    end,
    {noreply, State};

process({get, friendInfos, ok, FriendInfos, NeedUpdate}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, friendInfos, ok, FriendInfos}),
    case NeedUpdate of true ->
        {noreply, State#state{friends={utils:now(), FriendInfos}}};
    false ->
        {noreply, State}
    end;

process(_AnyEvent,  #state{name=work} = State ) ->
    {noreply, State};

process({startWork, WorkID}, #state{name=notConnected} = State) ->
    if WorkID > 0 ->
        case fuel:startWork(WorkID) of
            {ok, Time, Message, WorkInfo} ->
                sendMessage(State, {startWork, ok, Time, Message}),
                changeState(State, State#state{name=work, workInfo = WorkInfo});
            {error, Reason, Message} ->
                sendMessage(State, {startWork, error, {Reason, Message}}),
                {noreply, State}
        end;
    true ->
        {noreply, State}
    end;
    
process({startTransfer, TransitionType, CitySrcID, CityDstID}, #state{name=notConnected} = State) ->
    case dbUser:startTransferToCity(State#state.userID, TransitionType, CitySrcID, CityDstID) of
        {error, {Reason, Message}} ->
            sendMessage(State, {startTransfer, error, {Reason, Message}});
        {ok, UserState} ->
			UserInfoWithDetails = getUserInfoWithDetails(State#state.userID),
            sendMessage(State, {startTransfer, ok, UserState, UserInfoWithDetails})
    end,
    
    {noreply, State};

process({get, userState}, #state{name=notConnected} = State) ->
    UserState = dbUser:getUserState(State#state.userID),
    User = dbUser:getRecord(id, State#state.userID),
    
    NewCity = if State#state.currentCity =/= User#user.currentCity ->
        ClientInfo = getClientInfo(userID, User#user.id, State#state.clientID),
        cityManager:moveClient(ClientInfo, self(), State#state.currentCity, User#user.currentCity),
        User#user.currentCity;
    true ->
        State#state.currentCity
    end,
    
    sendMessage(State, {get, userState, ok, UserState, NewCity}),
    {noreply, State#state{currentCity=NewCity}};

%%%% Not authorized state    %%%%
    
process({authorizeVkontakte, ok, User}, #state{name=notAuthorized} = State) ->
    UserInfo = users:getUserInfo(User),
    dbActivity:register(User#user.id, authorize, ok),
    sendMessage(State, {authorize, ok, UserInfo}),
    case users:checkTrigger(User, register) of
        false ->
            Session = createSession(State#state{userID=User#user.id}),
            changeState(State, State#state{name=notConnected, userID=User#user.id, currentCity=User#user.currentCity, session=Session});
        true ->
            changeState(State, State#state{name=registering, userID=User#user.id})
    end;

process({authorizeVkontakte, error, Reason, Message, VkontakteID, AuthKey, FirstLastName}, #state{name=notAuthorized} = State) ->
    dbActivity:register(undefined, {authorizeVkontakte, VkontakteID, AuthKey, FirstLastName}, error),
    sendMessage(State, {authorize, error, {Reason, Message}}),   
    {noreply, State};

process({authorizeVkontakte, VkontakteID, AuthKey, FirstLastName, VkontakteOwnerID}, #state{name=notAuthorized} = State) ->
    vkontakte:authorize(VkontakteID, AuthKey, FirstLastName, VkontakteOwnerID),
    {noreply, State};

%%%% Registering state %%%%

process({register, Nickname, Car, Color, City}, #state{name=registering} = State) ->
    UserID = State#state.userID,
    case registration:tryUpdateRegistration(UserID, string:strip(Nickname), Car, Color, City) of
        {ok, User} ->
            sendMessage(State, {register, ok, dbUser:getInfoWithDetailsAndUpgrades(UserID, State#state.clientID)}),
            dbActivity:register(UserID, register, ok),
            Session = createSession(State),
            changeState(State, State#state{name=notConnected, currentCity=User#user.currentCity, session=Session});
        {error, RM} ->
            sendMessage(State, {register, error, RM}),
            dbActivity:register(UserID, register, error),
            {noreply, State}
    end;

process({get, cars}, #state{name=registering} = State) ->
    queryCache:requestCars(self()),
    {noreply, State};

process({requestCars, Cars}, #state{name=registering} = State) ->
    sendMessage(State, {get, cars, ok, Cars}),
    {noreply, State};

%%%% Not connected state %%%%

process({buyFuel, FuelCount}, #state{name=notConnected} = State) ->
    case fuel:buyFuel(State#state.userID, FuelCount)of
        ok ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            GasInfo = fuel:getGasInfo(State#state.userID),
            sendMessage(State, {buyFuel,ok, UserInfo, GasInfo});
        error->
            sendMessage(State, {buyFuel, error, {incorrectFuelCount, "[[youCantBuySoMuchFuel]]"}})
    end,
    {noreply, State};

process({joinLobby, LobbyID}, #state{name=notConnected} = State) ->
    User = dbUser:getRecord(id, State#state.userID),
    ClientInfo = getClientInfo(userID, User#user.id, State#state.clientID),
    LobbyManagerPID = cityManager:getLobbyManagerByID(State#state.currentCity),
    lobbyManager:joinLobby(LobbyManagerPID, LobbyID, self(), ClientInfo, User),
    {noreply, State};

process({joinLobby, ok, LobbyPID, LobbyInfo}, #state{name=notConnected} = State) ->
    sendMessage(State, {joinLobby, ok, LobbyInfo, carUpgrades:getUpgradeInfo(State#state.userID,State#state.clientID)}),
    dbActivity:register(State#state.userID, joinLobby, ok),
    changeState(State, State#state{name=lobbyHall, lobbyPID=LobbyPID});
    
process({joinLobby, error, {Reason, Message}}, #state{name=notConnected} = State) ->
    sendMessage(State, {joinLobby, error, {Reason, Message}}),
    {noreply, State};

process({createLobby, RouteID, LapNumber, Direction, PlayerMax, AllowedCarID, TimerLength, League,Stake}, #state{name=notConnected} = State) ->
    User = dbUser:getRecord(id, State#state.userID),
    ClientInfo = getClientInfo(userID, User#user.id, State#state.clientID),
    LobbyManagerPID = cityManager:getLobbyManagerByID(State#state.currentCity),
    Car = mneser:getRecord(car,User#user.currentCarID),
    CarClass = mneser:getRecord(carClass,Car#car.carClassID),
    lobbyManager:createLobby(LobbyManagerPID, #lobbyInfo{routeID=RouteID, 
                                creatorName=User#user.name,
                                creatorClientID=State#state.clientID,
                                lapNumber=LapNumber,
                                direction=Direction,
                                playerMax=PlayerMax,
                                allowedCarID=AllowedCarID,
                                timerEnd=0, 
                                timerLength=TimerLength,
                                league=League,
                                stake=Stake,
                                creatorRating=User#user.rating,
                                creatorCarName=CarClass#carClass.displayName}, self(), ClientInfo,User),
    {noreply, State};

process({createLobby, ok, LobbyPID, LobbyInfo}, #state{name=notConnected} = State) ->
    sendMessage(State, {createLobby, ok, LobbyInfo, carUpgrades:getUpgradeInfo(State#state.userID, State#state.clientID)}),
    dbActivity:register(State#state.userID, createLobby, ok),
    changeState(State, State#state{name=lobbyHall, lobbyPID=LobbyPID});
        
process({createLobby, error, {Reason, Message}}, #state{name=notConnected} = State) ->
    sendMessage(State, {createLobby, error, {Reason, Message}}),
    {noreply, State};

%%%% LobbyHall state %%%%

process({quitLobby, _Reason} = Event, #state{name=lobbyHall} = State) ->    
    lobby:removeClient(State#state.clientID, State#state.lobbyPID),
    sendMessage(State, Event),
    changeState(State, State#state{name=notConnected, lobbyPID=undefined});
    
process({addClient, lobby, _ClientInfo} = Event, #state{name=lobbyHall} = State) ->
    sendMessage(State, Event),
    {noreply, State};

process({removeClient, lobby, _ClientID} = Event, #state{name=lobbyHall} = State) ->
    sendMessage(State, Event),
    {noreply, State};

process({loadingProgress, lobby, Progress}, #state{name=lobbyHall} = State) ->
    lobby:setLoadingProgress(Progress, State#state.clientID, State#state.lobbyPID),
    {noreply, State};

process({loadingProgress, lobby, _Progress, _ClientID} = Event, #state{name=lobbyHall} = State) ->
    sendMessage(State, Event),
    {noreply, State};
    
process({readiness, Value}, #state{name=lobbyHall} = State) ->
    lobby:setReadiness(State#state.clientID, State#state.lobbyPID, Value),
    {noreply, State};

process({lobbyRaceEvent, initialize, _, _} = Event, #state{name=lobbyHall} = State) ->
    sendMessage(State, Event),
    changeState(State, State#state{name=lobbyRacing});

%%%% LobbyRacing state %%%%

process({readiness, Value}, #state{name=lobbyRacing} = State) ->
    lobby:setReadiness(State#state.clientID, State#state.lobbyPID, Value),
    {noreply, State};

process({carState, CarState}, #state{name=lobbyRacing} = State) ->
    lobby:carState(CarState, utils:now(), State#state.clientID, State#state.lobbyPID),
    {noreply, State};
    
process({otherCarState, _CarState, _Time, _ClientID} = Event, #state{name=lobbyRacing} = State) ->
    sendMessage(State, Event),
    {noreply, State};
    
process({removeClient, lobby, _ClientID} = Event, #state{name=lobbyRacing} = State) ->
    sendMessage(State, Event),
    {noreply, State};

process({startLobbyRace, _Type} = Event, #state{name=lobbyRacing} = State) ->
    sendMessage(State, Event),
    {noreply, State};
    
process({lapTime, lobby, Type, _LapTime}, #state{name=lobbyRacing} = State) ->
    lobby:lapTime(Type, State#state.clientID, State#state.lobbyPID),
    {noreply, State};

process({lobbyRaceResults, _LobbyResults, _LobbyUpg, _Wear, _BlueScore, _RedScore} = Event, #state{name=lobbyRacing} = State) ->
    sendMessage(State, Event),
    changeState(State, State#state{name=lobbyHall});

process({quitLobby, _Reason} = Event, #state{name=lobbyRacing} = State) ->
    lobby:removeClient(State#state.clientID, State#state.lobbyPID),
    sendMessage(State, Event),
    changeState(State, State#state{name=notConnected, lobbyPID=undefined});

%%% group states %%%%

process({chatMessage, lobby, Text, ""}, State) when ?STATES_LOBBY(State) ->
    UserID = State#state.userID,
     case dbUser:canChat(State#state.userID) of
        true ->
            Nick = (dbUser:getRecord(id, UserID))#user.name,
            lobby:chatMessage(Text, State#state.clientID, Nick, State#state.lobbyPID);
        _ -> ok
     end,
    {noreply, State};
    
process({lobbyRaceEvent, _Type, _Param, _ClientID} = Event, State) when ?STATES_LOBBY(State) ->
    sendMessage(State, Event),
    {noreply, State};

process({addClient, global, _ClientInfo} = Event, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, Event),
    {noreply, State};

%todo: not inluded in spec
process({addClientInitial, global, _ClientInfo} = Event, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, Event),
    {noreply, State};

process({removeClient, global, _ClientID} = Event, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, Event),
    {noreply, State};
    
process({get, userInfo}, State) when ?STATES_AUTHORIZED(State) ->
    UserInfoWithDetailsAndUpgrades = dbUser:getInfoWithDetailsAndUpgrades(State#state.userID, State#state.clientID),
    sendMessage(State, {get, userInfo, ok, UserInfoWithDetailsAndUpgrades}),
    {noreply, State};

process({get, gasInfo}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, gasInfo, ok, fuel:getGasInfo(State#state.userID)}),
    {noreply, State};

process({get, tipInfo}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, tipInfo, ok, dbGame:getTips(userID,State#state.userID)}),
    sendMessage(State, {get, leagueInfo, ok, mneser:getAllRecords(league)}),
    {noreply, State};

process({get, cities}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, cities, ok, dbCity:getCitiesRounded()}),    
    {noreply, State};

process({get, cars}, State) when ?STATES_AUTHORIZED(State) ->
    queryCache:requestCars(self()),
    {noreply, State};

process({requestCars, Cars}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, cars, ok, Cars}),
    {noreply, State}; 
    
process({get, inbox}, State) when ?STATES_AUTHORIZED(State) ->        
    Inbox = dbMessage:getAllPost(State#state.userID),
    sendMessage(State, {get, inbox, ok, Inbox}),
    {noreply, State};
    
process({processPostMessage, PostMessageID, Action}, State) when ?STATES_AUTHORIZED(State) ->        
    case dbMessage:processPost(Action, PostMessageID, State#state.userID) of
        {ok, PostMessageInfos} ->
            UserInfoWithDetailsAndUpgrades = dbUser:getInfoWithDetailsAndUpgrades(State#state.userID, State#state.clientID),
            sendMessage(State, {processPostMessage, Action, ok, UserInfoWithDetailsAndUpgrades, PostMessageInfos});
        {error, {Reason, Message}} -> 
            sendMessage(State, {processPostMessage, Action, error, {Reason, Message}})
    end,
    {noreply, State};

process({get, routes}, State) when ?STATES_AUTHORIZED(State) ->
    queryCache:requestRoutes(self()),
    {noreply, State};

process({requestRoutes, Routes}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, routes, ok, Routes}),
    {noreply, State};
    
process({showIntro, _Type} = Event, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, Event),
    {noreply, State};

process({reduceNitroCount}, State) when ?STATES_AUTHORIZED(State) ->
    dbUser:addNitroCount(State#state.userID, -1),
    {noreply, State};
    
process({get, lobbies}, State) when ?STATES_AUTHORIZED(State) ->
    LobbyManagerPID = cityManager:getLobbyManagerByID(State#state.currentCity),
    lobbyManager:requestLobbies(LobbyManagerPID),
    {noreply, State};
    
process({requestLobbies, ok, Lobbies}, State) when ?STATES_AUTHORIZED(State) ->    
    sendMessage(State, {get, lobbies, ok, Lobbies}),
    {noreply, State};
    
process({get, ratings}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, State#state.userID),
    queryCache:requestRatings(User#user.homeCity, self()),    
    {noreply, State};

process({requestRatings, Ratings}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, ratings, ok, Ratings}),
    {noreply, State};

process({get, topUserDailyScores}, State) when ?STATES_AUTHORIZED(State) ->
    queryCache:requestTopUserDailyScores(self()),    
    {noreply, State};

process({requestTopUserDailyScores, Scores}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, topUserDailyScores, ok, Scores}),
    {noreply, State};

process({repair, ItemID}, State) when ?STATES_AUTHORIZED(State) ->
    ResponseMessage = case dbItem:repairEquipment(State#state.userID, ItemID) of
        ok ->
            {repair, ok, dbUser:getInfoWithDetailsAndUpgrades(State#state.userID, State#state.clientID)};
       {error, {Reason, Message}} ->
            {repair, error, {Reason, Message}}
    end,
    sendMessage(State, ResponseMessage),
    {noreply, State};

process({repairCar, CarID}, State) when ?STATES_AUTHORIZED(State) ->
    ResponseMessage = case dbCar:capitalRepairCar(State#state.userID, CarID) of
        ok ->
            {repair, ok, dbUser:getInfoWithDetailsAndUpgrades(State#state.userID, State#state.clientID)};
       {error, {Reason, Message}} ->
            {repair, error, {Reason, Message}}
    end,
    sendMessage(State, ResponseMessage),
    {noreply, State};
    
process({get, lobby, LobbyID}, State) when ?STATES_AUTHORIZED(State) ->  
    LobbyManagerPID = cityManager:getLobbyManagerByID(State#state.currentCity),
    lobbyManager:requestLobbyInfo(LobbyManagerPID, LobbyID),
    {noreply, State};

process({requestLobbyInfo, ok, LobbyInfo}, State) when ?STATES_AUTHORIZED(State) ->  
    sendMessage(State, {get, lobby, ok, LobbyInfo}),
    {noreply, State};
    
process({requestLobbyInfo, error, {Reason, Message}}, State) when ?STATES_AUTHORIZED(State) ->  
    sendMessage(State, {get, lobby, error, {Reason, Message}}),
    {noreply, State};

process({useCarUpgrade, ID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbItem:useCarUpgrade(UserID, ID) of
        ok ->
            UserInfoWithDetailsAndUpgrades = dbUser:getInfoWithDetailsAndUpgrades(State#state.userID, State#state.clientID),
            sendMessage(State, {useCarUpgrade, ok, UserInfoWithDetailsAndUpgrades});
        {error, {Reason, Message}} ->
            sendMessage(State, {useCarUpgrade, error, {Reason, Message}})
    end,
    {noreply, State};
    
process({removeCarUpgrade, ID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbItem:removeCarUpgrade(UserID, ID) of
        ok ->
            UserInfoWithDetailsAndUpgrades = dbUser:getInfoWithDetailsAndUpgrades(State#state.userID, State#state.clientID),
            sendMessage(State, {removeCarUpgrade, ok, UserInfoWithDetailsAndUpgrades});
        {error, {Reason, Message}} ->
            sendMessage(State, {removeCarUpgrade, error, {Reason, Message}})
    end,            
    {noreply, State};    

process({get, shopInfo}, State) when ?STATES_AUTHORIZED(State) ->
    ShopInfo = dbItem:getShopInfo(State#state.userID),
    sendMessage(State, {get, shopInfo, ok, ShopInfo}),
    {noreply, State};

process({get, carShopInfo}, State) when ?STATES_AUTHORIZED(State) ->
    CarShopInfo = dbCar:getShopInfo(),
    sendMessage(State, {get, carShopInfo, ok, CarShopInfo}),
    {noreply, State};

process({buyItem, ItemID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbItem:buy(UserID, ItemID) of
        ok ->
			ItemClass = dbItem:getClass(ItemID),
			User = dbUser:getRecord(id, State#state.userID),			
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {buyItem, ok, UserInfoWithDetails, ItemClass});
        {error, {Reason, Message}} ->
            sendMessage(State, {buyItem, error, {Reason, Message}})
    end,
    {noreply, State};

process({setCar, CarID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbCar:setCar(UserID, CarID) of
        ok ->
            ok;
        {error, {_Reason, _Message}} ->
            ok
    end,
    {noreply, State};

process({buyCar, CarID, Color, NeedSlot}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    Result = if NeedSlot ->
        case dbCar:buySlot(UserID) of
            ok ->
                ok;
            {error, _} = Error ->
                Error
        end;
    true ->
        ok
    end,
    
    case Result of
        ok ->
            case dbCar:buy(UserID, CarID, Color) of
                ok ->
        			CarClass = dbCar:getClass(CarID),
                    User = dbUser:getRecord(id, UserID),
                    UserInfo = users:getUserInfo(User),
                    UserInfoWithDetails = users:getUserDetails(UserInfo),
                    sendMessage(State, {buyCar, ok, UserInfoWithDetails, CarClass, NeedSlot});
                {error, {Reason, Message}} ->
                    sendMessage(State, {buyCar, error, {Reason, Message}})
            end;
        {error, {Reason, Message}} ->
            sendMessage(State, {buyCar, error, {Reason, Message}})
    end,
            
    {noreply, State};            
    
process({deleteItem, ItemID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbItem:delete(UserID, ItemID) of
        ok ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {deleteItem, ok, UserInfoWithDetails});        
        {error, {Reason, Message}} ->
            sendMessage(State, {deleteItem, error, {Reason, Message}})
    end,
    {noreply, State};

process({exchangeMoney, RealMoney}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbUser:exchangeMoney(UserID, RealMoney) of
        ok ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {exchangeMoney, ok, UserInfoWithDetails, RealMoney});
        {error, {Reason, Message}} ->
            sendMessage(State, {exchangeMoney, error, {Reason, Message}})
    end,
    {noreply, State};
    
process({exchangeVkontakteVotes, {ok, Votes}}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, State#state.userID),
    
    dbUserProgress:incRealPurchases(User),
    
    UserInfo = users:getUserInfo(User),            
    UserInfoWithDetails = users:getUserDetails(UserInfo),
    sendMessage(State, {exchangeVkontakteVotes, ok, UserInfoWithDetails, Votes}),
    {noreply, State};

process({exchangeVkontakteVotes, {error, {Reason, Message}}}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {exchangeVkontakteVotes, error, {Reason, Message}}),
    {noreply, State};
    
process({exchangeVkontakteVotes, VkontakteVotes}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    VkontakteUserID = (dbUser:getRecord(id, State#state.userID))#user.vkontakteID,
    vkontakte:exchangeVkontakteVotes(UserID, VkontakteUserID, VkontakteVotes),
    {noreply, State};
        
process({sendItem, ItemID, Money, SellPrice, Comment, RecepientNick}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbItem:send(UserID, ItemID, Money, SellPrice, Comment, RecepientNick) of
        ok ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {sendItem, ok, UserInfoWithDetails});
        {error, {Reason, Message}} ->
            sendMessage(State, {sendItem, error, {Reason, Message}})
    end,
    {noreply, State};    

process({sellItem, ItemID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
	Item = dbItem:getItem(ItemID),
    case dbItem:sell(UserID, ItemID) of
        ok ->
			ItemClass = dbItem:getClass(Item#item.itemClassID),
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {sellItem, ok, UserInfoWithDetails, ItemClass});
        {error, {Reason, Message}} ->
            sendMessage(State, {sellItem, error, {Reason, Message}})
    end,
    {noreply, State};

process({sellCar, CarID}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
	Car = dbCar:getCar(CarID),
	CarClass = dbCar:getClass(Car#car.carClassID),
	
    case dbCar:sell(UserID, CarID) of
        ok ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {sellCar, ok, UserInfoWithDetails, CarClass});
        {error, {Reason, Message}} ->
            sendMessage(State, {sellCar, error, {Reason, Message}})
    end,
    {noreply, State};

process({repaintCar, CarID, Color}, State) when ?STATES_AUTHORIZED(State) ->
    UserID = State#state.userID,
    case dbCar:repaint(UserID, CarID,Color) of
        ok ->
			Car = dbCar:getCar(CarID),
			RecolorInfo = dbCar:getRecolorInfo(Car#car.carClassID, Color),			
            User = dbUser:getRecord(id, State#state.userID),
            
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            sendMessage(State, {repaintCar, ok, UserInfoWithDetails, RecolorInfo});
        {error, {Reason, Message}} ->
            sendMessage(State, {repaintCar, error, {Reason, Message}})
    end,
    {noreply, State};

process({get, globalInfo}, State) when ?STATES_AUTHORIZED(State) ->
    GlobalInfos = dbGlobal:getAll(),
    sendMessage(State, {get, globalInfo, ok, GlobalInfos}),
    {noreply, State};
    
process({get, vkontakteInfo, ok, VkontakteInfo}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, vkontakteInfo, ok, VkontakteInfo}),
    {noreply, State};

process({get, vkontakteInfo, error, {R, M}}, State) when ?STATES_AUTHORIZED(State) ->
    sendMessage(State, {get, vkontakteInfo, error, {R, M}}),
    {noreply, State};

process({get, vkontakteInfo}, State) when ?STATES_AUTHORIZED(State) ->
    VkontakteUserID = (dbUser:getRecord(id, State#state.userID))#user.vkontakteID,
    vkontakte:getVkontakteInfo(VkontakteUserID),
    {noreply, State};
    
process({switchHomeCity}, State) when ?STATES_AUTHORIZED(State) ->
    User = dbUser:getRecord(id, State#state.userID),
    NewCity = case users:checkRole(User, dev) of
        true ->
            NewUser = dbUser:switchHomeCity(User),
            UserInfo = users:getUserInfo(NewUser),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
            ClientInfo = getClientInfo(userID, User#user.id, State#state.clientID),
            cityManager:moveClient(ClientInfo, self(), State#state.currentCity, NewUser#user.currentCity),
            sendMessage(State, {switchHomeCity, ok, UserInfoWithDetails}),
            NewUser#user.currentCity;
        _ ->
            State#state.currentCity
    end,
    
    {noreply, State#state{currentCity=NewCity}};
    
process({restartGame}, State) when ?STATES_AUTHORIZED(State) ->
    case dbUser:restartGame(State#state.userID) of
        ok ->
            sendMessage(State, {restartGame, ok}),
            {stop, normal, State};
        {error, {Reason, Message}} ->
            sendMessage(State, {restartGame, error, {Reason, Message}}),
            {noreply, State}
    end;
    
process({levelUpPrize, Level, Type}, State) when ?STATES_AUTHORIZED(State) ->
    case users:giveLevelUpPrize(State#state.userID, Level, Type) of
        ok ->
            User = dbUser:getRecord(id, State#state.userID),
            UserInfo = users:getUserInfo(User),
            UserInfoWithDetails = users:getUserDetails(UserInfo),
         
            sendMessage(State, {levelUpPrize, ok, UserInfoWithDetails});
        {error, RM} ->
            sendMessage(State, {levelUpPrize, error, RM})
    end,
    
    {noreply, State};

process({levelUpInfo, Level}, State) when ?STATES_AUTHORIZED(State) ->
	LevelRec = dbGame:getLevel(number, Level),
	if erlang:is_record(LevelRec, level) ->
		Message = LevelRec#level.message,
	    Upgrades = dbItem:getUpgradesWithMinimumLevel(Level),
	    Routes = dbGame:getRoutesWithMinimumLevel(Level),
	    Cars = dbCar:getCarsWithMinimumLevel(Level),

		sendMessage(State, {levelUpInfo, ok, Message, Upgrades, Routes, Cars});
	true ->
		sendMessage(State, {levelUpInfo, error, {noLevel, "[[databaseError]]"}})
	end,
	
	{noreply, State};
    
process({levelUpRewards, Level}, State) when ?STATES_AUTHORIZED(State) ->
    LevelRec = dbGame:getLevel(number, Level),
    if erlang:is_record(LevelRec, level) ->
        
        ItemClass = dbItem:getClass(LevelRec#level.itemID),
        NitroClass = dbItem:getClass(LevelRec#level.nitroID),
        sendMessage(State, {levelUpRewards, ok, LevelRec#level.realMoney, LevelRec#level.money, ItemClass#itemClass.name, NitroClass#itemClass.usingCount});
    true ->
        sendMessage(State, {levelUpRewards, error, {database, "[[databaseError]]"}}) 
    end,
    
    {noreply, State};
        
        
%% all states

process({ping}, State) ->
    {noreply, State};

process({skipTutorial}, State) ->
    users:setTrigger(State#state.userID,tutorialStage, 5),
    User = dbUser:getRecord(id, State#state.userID),
    UserInfo = users:getUserInfo(User),
    UserInfoWithDetails = users:getUserDetails(UserInfo),
    sendMessage(State, {skipTutorial, ok, UserInfoWithDetails}),
    {noreply, State};

process({nyGift}, State) ->
    mnesia:transaction(fun() ->
        User = dbUser:getRecord_nt(id, State#state.userID),
        case users:hasGift(User) of 
        true ->
            User2 = users:setTriggerValue_nt(User, lastGiftTime, utils:now()),
            mnesia:write(User2#user{realMoney=User2#user.realMoney+1});
        false -> ok end
    end),
    {noreply, State};

process({cheatGift}, State) ->
    mnesia:transaction(fun() ->
        User = dbUser:getRecord_nt(id, State#state.userID),
        case not users:hasGift(User) andalso users:canCheat(User) of 
        true ->
            User2 = users:setTriggerValue_nt(User, lastGiftTime, 0),
            User3 = users:setTriggerValue_nt(User2, lastCheatTime, utils:now()),
            mnesia:write(User3);
        false -> ok end
    end),
    {noreply, State};

%% unexpected event
process(UnexpectedEvent, State) ->
    log:writeMessage(debug, ?MODULE_STRING, State#state.clientID, State#state.name, UnexpectedEvent , "Unexpected event "),
    {noreply, State}.


getClientInfo(userID, UserID, ClientID) ->
    User = dbUser:getRecord(id, UserID),
    Car = mneser:getRecord(car, User#user.currentCarID),    
    #clientInfo{clientID=ClientID, userID=UserID, displayName=User#user.name, 
                homeCity=User#user.homeCity, currentCity=User#user.currentCity, carID = Car#car.carClassID,
                carFileName=dbCar:getCarNameByID(User#user.currentCarID),
                carColor=Car#car.color, bestTime=0, lastTime=0, startTime=0,
                upgrades = carUpgrades:getUpgradeInfo(UserID,ClientID),
                level=User#user.level,
                userInfo=users:getUserInfo(User)
            }.

inGroupState(lobbyGroup, StateRecord) ->
    utils:inList(StateRecord#state.name, [lobbyRacing, lobbyHall]);    
        
inGroupState(singleGroup, StateRecord) ->
    utils:inList(StateRecord#state.name, [loading, racing]);

inGroupState(_, _) ->
    false.
    
createSession(State) ->
    UserID = State#state.userID,    
    Session = dbSession:create(UserID),
    
    if State#state.socket =/= nosocket ->
        serverSocket:killSessionIfExists(UserID),
        serverSocket:registerSession(UserID, self(), State#state.clientID);
    true -> ok end,
    
    Session.

markFriendCarCleaned(FriendID, FriendInfos) ->
    lists:map(fun(FriendInfo) -> 
        if FriendInfo#friendInfo.userID =:= FriendID ->
            FriendInfo#friendInfo{isWashed=true};
        true ->
            FriendInfo
        end
    end, FriendInfos).

getUserInfoWithDetails(UserID) ->
	User = dbUser:getRecord(id, UserID),			
    UserInfo = users:getUserInfo(User),
    users:getUserDetails(UserInfo).