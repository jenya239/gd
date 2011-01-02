-module(lobby).

-behaviour(gen_server).

-export([start/3]).

-export([init/1, 
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).
    
-export([getClients/1, 
        addClient/4, 
        removeClient/2, 
        setLoadingProgress/3,
        requestLobbyInfo/2, 
        setReadiness/3, 
        checkAllLoaded/4, 
        carState/4, 
        lapTime/3, 
        chatMessage/4, 
        compareFinishTime/2, 
        checkAllQuit/1, 
        getClientsCount/1, 
        checkAllReadiness/2, 
        setAllReadiness/2,
        getState/1]).

-include("data.hrl").
-include("config.hrl").
-include("lib/eunit/include/eunit.hrl").

-define(CHECKING_CONNECTION_TIMER, 5000).

-record(state, {name, lobbyInfo, clients, finishTimerID, startTime = 0, lobbyManagerPID}).

start(LobbyInfo, CityID, LobbyManagerPID) ->
    ProcessName = list_to_atom(?MODULE_STRING ++ integer_to_list(CityID) ++ "_" ++ integer_to_list(LobbyInfo#lobbyInfo.id)),
    gen_server:start({local, ProcessName}, ?MODULE, [LobbyInfo, LobbyManagerPID], []).    

%%-----------------------------------------------------------------------------
%% Interface
%%-----------------------------------------------------------------------------
init([LobbyInfo, LobbyManagerPID]) -> 
    log:write(debug, ?MODULE_STRING, LobbyInfo#lobbyInfo.id, "process started ~n", []),
    {ok, TimerRef} = timer:send_interval(?CHECKING_CONNECTION_TIMER, self(), timerCheckingSlow),
    {ok, #state{name=hall, lobbyInfo=LobbyInfo#lobbyInfo{timerCheckingSlow = TimerRef}, clients=dict:new(), lobbyManagerPID=LobbyManagerPID}}.

%must NOT be used in game processes, use by YAWS and TESTS ONLY
getClients(LobbyPID) ->    
    gen_server:call(LobbyPID, getClients).

addClient(ClientPID, ClientInfo, LobbyPID, MessageName) ->    
    gen_server:cast(LobbyPID, {addClient, ClientPID, ClientInfo, MessageName}).

removeClient(ClientID, LobbyPID) ->
    gen_server:cast(LobbyPID, {removeClient, ClientID}).

setLoadingProgress(Progress, ClientID, LobbyPID) ->
    gen_server:cast(LobbyPID, {loadingProgress, Progress, ClientID}).

requestLobbyInfo(LobbyPID, From) ->     
    gen_server:cast(LobbyPID, {requestLobbyInfo, From}).

setReadiness(ClientID, LobbyPID, Value) ->
    gen_server:cast(LobbyPID, {readiness, ClientID, Value}).

carState(CarState, Time, ClientID, LobbyPID) ->
    gen_server:cast(LobbyPID, {carState, CarState, Time, ClientID}).

lapTime(Type, ClientID, LobbyPID) ->
    gen_server:cast(LobbyPID, {lapTime, Type, ClientID}).

chatMessage(Text, ClientID, Nick, LobbyPID) ->
    gen_server:cast(LobbyPID, {chatMessage, Text, Nick, ClientID}).

%for debug only!!!
getState(LobbyPID) ->
    gen_server:call(LobbyPID, getState). 

%%-----------------------------------------------------------------------------
%% Handle calls
%%-----------------------------------------------------------------------------
handle_call(getClients, _From, State) ->
    ClientsList = lists:reverse(dict:fold(
        fun(_Key, Value, List) -> 
            [Value | List]
        end, [], State#state.clients)),
        
    {reply, ClientsList, State};

handle_call(getState, _From, State) ->
    {reply, State, State};

handle_call(Unexpected, _From, State) ->
    log:write(warning, ?MODULE_STRING, (State#state.lobbyInfo)#lobbyInfo.id, State#state.name, "Unexpected call: ~10000p~n", [Unexpected]),

    {noreply, State}.    
   
%%-----------------------------------------------------------------------------
%% Handle casts
%%-----------------------------------------------------------------------------

handle_cast({addClient, ClientPID, _ClientInfo}, State) when State#state.name =:= racing ->
    ClientPID ! {error, {lobbyIsFull, "[[cantJoinLobbyDuringRacing]]"}},
    {noreply, State};

handle_cast({addClient, ClientPID, ClientInfo, MessageName}, State) when State#state.name =:= hall ->    
    
    {BlueCount, RedCount} = getClientCountsByCity(State#state.clients),
    PlayerMax = 
        if (State#state.lobbyInfo)#lobbyInfo.type =:= team ->
            (State#state.lobbyInfo)#lobbyInfo.playerMax div 2;
        true ->
            (State#state.lobbyInfo)#lobbyInfo.playerMax
    end,
    
    if (State#state.lobbyInfo)#lobbyInfo.league =/= 4 andalso
       ((ClientInfo#clientInfo.homeCity =:= 1 andalso BlueCount >= PlayerMax) 
           orelse (ClientInfo#clientInfo.homeCity =:= 2 andalso RedCount >= PlayerMax)) ->
        ClientPID ! {MessageName, error, {lobbyIsFull, "[[lobbyIsFull]]"}},
        {noreply, State};
    (State#state.lobbyInfo)#lobbyInfo.league =:= 4 andalso BlueCount + RedCount >= 2 ->
        ClientPID ! {MessageName, error, {lobbyIsFull, "[[lobbyIsFull]]"}},
        {noreply, State};
    true ->
        TimerLength = (State#state.lobbyInfo)#lobbyInfo.timerLength,
        ClientInfo2 = ClientInfo#clientInfo{loadingStartTimestamp=utils:now()},
        NewClients0 = dict:store(ClientInfo#clientInfo.clientID, {ClientPID, ClientInfo2}, State#state.clients),
        NewClients = setAllReadiness(none, NewClients0),
        
        TimerEnd = case canStartWaitingTimer(State#state.lobbyInfo, NewClients) of
            true ->
                timer:send_after(TimerLength, self(), timerEnd),
                utils:now() + TimerLength;
            false ->
                (State#state.lobbyInfo)#lobbyInfo.timerEnd
        end,
        
        {NewBlueCount, NewRedCount} = case ClientInfo#clientInfo.homeCity of
            1 ->
                {BlueCount+1, RedCount};
            2 ->
                {BlueCount, RedCount+1}
        end,
                
        NewLobbyInfo = (State#state.lobbyInfo)#lobbyInfo{playerCountBlue=NewBlueCount, playerCountRed=NewRedCount, timerEnd=TimerEnd},
        lobbyManager:updateLobbyInfo(State#state.lobbyManagerPID, NewLobbyInfo),
        utils:sendToAllExceptOne({addClient, lobby, ClientInfo}, State#state.clients, ClientInfo#clientInfo.clientID),
        ClientPID ! {MessageName, ok, self(), NewLobbyInfo},
        sendAllAddClientMessages(State#state.clients, ClientPID),
        {noreply, State#state{lobbyInfo=NewLobbyInfo, clients=NewClients}}
    end;

handle_cast({requestLobbyInfo, From}, State) ->    
    From ! {requestLobbyInfo, ok, State#state.lobbyInfo},
    {noreply, State};

handle_cast({loadingProgress, Progress, ClientID}, State) when State#state.name =:= hall ->
    NewClients = try
        dict:update(ClientID,
            fun({Client, ClientInfo}) ->
                {Client, ClientInfo#clientInfo{loadingProgress=Progress}}
            end, State#state.clients)
    catch error:_ ->
        State#state.clients
    end,
    utils:sendToAllExceptOne({loadingProgress, lobby, Progress, ClientID}, State#state.clients, ClientID),
    
    {noreply, State#state{clients=NewClients}};
    
handle_cast({removeClient, ClientID}, State) ->
    log:write(debug, ?MODULE_STRING, (State#state.lobbyInfo)#lobbyInfo.id, "removing ClientID = ~10000p~n", [ClientID]),    
    case dict:find(ClientID, State#state.clients) of
        {ok, {_, RemovedClientInfo}} ->
            case State#state.name of
                hall ->                    
                    NewClients =
                    if((State#state.lobbyInfo)#lobbyInfo.league =:= 4
                          andalso (State#state.lobbyInfo)#lobbyInfo.creatorClientID =:= ClientID )->
                          utils:sendToAllExceptOne({quitLobby, "[[yourOpponentHasLeftDuel]]"}, State#state.clients, ClientID),
                          dict:new();
                        true ->
                           dict:erase(ClientID, State#state.clients)
                    end;
                racing ->
                    % set ClientPID to undefined for the ClientID if quit duiring race
                     NewClients = dict:update(ClientID,
                        fun({_, ClientInfo}) ->
                            {undefined, ClientInfo}
                        end, State#state.clients),

                    AllFinished = checkAllFinished(NewClients),
                    if ( (State#state.lobbyInfo)#lobbyInfo.league =:= 4) ->
                          self() ! finishTimerEnd;
                       AllFinished ->
                           timer:cancel(State#state.finishTimerID),
                           self() ! finishTimerEnd;
                       true -> ok
                   end
            end,
            
            NewPlayerCount = getClientsCount(NewClients),
            AllQuit = checkAllQuit(NewClients),
            if 
                (NewPlayerCount =:= 0) orelse AllQuit ->                    
                    %lobbyManager:removeLobby((State#state.lobbyInfo)#lobbyInfo.id),

                    % exiting
                    {stop, normal, State};
                true ->                    
                    TimerEnd = case canStopWaitingTimer(State#state.lobbyInfo, NewClients) of
                        true ->
                            0;
                        false ->
                            (State#state.lobbyInfo)#lobbyInfo.timerEnd
                    end,
                    
                    {BlueCount, RedCount} = getClientCountsByCity(State#state.clients),
                    {NewBlueCount, NewRedCount} = case RemovedClientInfo#clientInfo.homeCity of
                        1 ->
                            {BlueCount-1, RedCount};
                        2 ->
                            {BlueCount, RedCount-1}
                    end,
                    
                    NewLobbyInfo = (State#state.lobbyInfo)#lobbyInfo{timerEnd=TimerEnd, playerCountBlue=NewBlueCount, playerCountRed=NewRedCount},                    
                    lobbyManager:updateLobbyInfo(State#state.lobbyManagerPID, NewLobbyInfo),

                    NewState = State#state{clients=NewClients, lobbyInfo=NewLobbyInfo},
                    utils:sendToAllExceptOne({removeClient, lobby, ClientID}, NewClients, ClientID),

                    AllReady = lobby:checkAllLoaded(NewClients, TimerEnd, (State#state.lobbyInfo)#lobbyInfo.playerMax, (State#state.lobbyInfo)#lobbyInfo.type),
                    NewState1 = if 
                        AllReady andalso State#state.name =:= hall ->
                            utils:sendToAll({lobbyRaceEvent, initialize, "", -1}, NewClients),
                            changeState(NewState#state{name=racing}, State);
                        true ->
                            NewState
                    end,

                    {noreply, NewState1}
            end;
        error ->            
            {noreply, State}
    end;    
        
handle_cast({readiness, ClientID, loaded}, State) when State#state.name =:= hall ->
    NewClients = try 
        dict:update(ClientID,
            fun({Client, ClientInfo}) ->
                {Client, ClientInfo#clientInfo{readiness=loaded}}
            end, State#state.clients)
    catch error:_ ->
        State#state.clients
    end,
    
    NewState = State#state{clients=NewClients},
    TimerEnd = (State#state.lobbyInfo)#lobbyInfo.timerEnd,

    AllLoaded = lobby:checkAllLoaded(NewClients, TimerEnd, (State#state.lobbyInfo)#lobbyInfo.playerMax, (State#state.lobbyInfo)#lobbyInfo.type),
    NewState1 = if 
        AllLoaded ->
            utils:sendToAll({lobbyRaceEvent, initialize, "", -1}, NewClients),
            changeState(NewState#state{name=racing}, State);
        true ->
            NewState
    end,    
    
    {noreply, NewState1};
    
handle_cast({readiness, ClientID, initialized}, State) when State#state.name =:= racing ->
    NewClients = try 
        dict:update(ClientID,
            fun({Client, ClientInfo}) ->
                {Client, ClientInfo#clientInfo{readiness=initialized}}
            end, State#state.clients)
    catch error:_ ->
        State#state.clients
    end,

    NewState = State#state{clients=NewClients},            

    AllInitialized = lobby:checkAllReadiness(initialized, NewClients),
    if 
        AllInitialized ->
            timer:send_after(1000, self(), countDown2),
            utils:sendToAll({lobbyRaceEvent, countdown, 3, -1}, NewClients);
        true ->
            ok
    end,
    
    {noreply, NewState};

handle_cast({carState, CarState, Time, ClientID}, State) when State#state.name =:= racing ->
   utils:sendToAllExceptOne({otherCarState, CarState, Time, ClientID}, State#state.clients, ClientID),                           
   {noreply, State};

handle_cast({lapTime, "finish", ClientID}, State) when State#state.name =:= racing ->    
    Now = utils:now(),
    NewClients = try 
        dict:update(ClientID,
               fun({Client, ClientInfo}) ->
                       {Client, ClientInfo#clientInfo{lastTime=Now}}
               end, State#state.clients)
    catch error:_ ->
        State#state.clients
    end,
    utils:sendMessageToClient(ClientID, {lobbyRaceEvent, otherCarFinished, Now - State#state.startTime, -1}, State#state.clients),
    utils:sendToAll({lobbyRaceEvent, otherCarFinished, Now - State#state.startTime, ClientID}, NewClients),
    AllFinished = checkAllFinished(NewClients),
    NewState =
      if
        AllFinished ->
            timer:cancel(State#state.finishTimerID),            
            self() ! finishTimerEnd,
            State;
        ((State#state.lobbyInfo)#lobbyInfo.league =:= 4) ->
            self() ! finishTimerEnd,
            State;
        true ->
            if 
                State#state.finishTimerID =:= undefined ->
                    {ok, FinishTimerID} = timer:send_after(30000, self(), finishTimerEnd),
                    utils:sendToAll({lobbyRaceEvent, finishCountDownStarted, utils:now() + 30000, -1}, NewClients),
                    State#state{finishTimerID = FinishTimerID};
                true ->
                    State
            end
    end,
    {noreply, NewState#state{clients=NewClients}}; 

handle_cast({chatMessage, Text, Nick, ClientID}, State) ->
    case dict:find(ClientID, State#state.clients) of
        {ok, {_,ClientInfo}} ->
            UserID = ClientInfo#clientInfo.userID,
            User = dbUser:getRecord(id, UserID),
            UserInfo = users:getUserInfo(User),
            utils:sendMessageToClient(ClientID, {otherChatMessage, lobby, Text, UserID, User#user.homeCity, Nick, utils:now(), UserInfo}, State#state.clients),
            utils:sendToAllExceptOne({otherChatMessage, lobby, Text, UserID, User#user.homeCity, Nick, utils:now(), UserInfo}, State#state.clients, ClientID);
        _ -> ok
    end,
    {noreply, State};
    
handle_cast(Unexpected, State) ->
    log:write(warning, ?MODULE_STRING, (State#state.lobbyInfo)#lobbyInfo.id, State#state.name, "Unexpected cast: ~10000p~n", [Unexpected]),

    {noreply, State}.    

%%-----------------------------------------------------------------------------
%% Handle info calls
%%-----------------------------------------------------------------------------
handle_info(countDown2, State) when State#state.name =:= racing ->
    timer:send_after(1000, self(), countDown1),
    utils:sendToAll({lobbyRaceEvent, countdown, 2, -1}, State#state.clients),
    
    {noreply, State};

handle_info(countDown1, State) when State#state.name =:= racing ->
    timer:send_after(1000, self(), countDownEnd),
    utils:sendToAll({lobbyRaceEvent, countdown, 1, -1}, State#state.clients),
    
    {noreply, State};
    
handle_info(countDownEnd, State) when State#state.name =:= racing ->
    Now = utils:now(),
    utils:sendToAll({lobbyRaceEvent, go, Now, -1}, State#state.clients),
    
    {noreply, State#state{startTime=Now}};    
        
handle_info(timerEnd, State) when State#state.name =:= hall ->
    TimerEnd = (State#state.lobbyInfo)#lobbyInfo.timerEnd,
    Clients = State#state.clients,

    AllReady = lobby:checkAllLoaded(Clients, TimerEnd, (State#state.lobbyInfo)#lobbyInfo.playerMax, (State#state.lobbyInfo)#lobbyInfo.type),
    NewState = if 
        AllReady ->            
            utils:sendToAll({lobbyRaceEvent, initialize, "", -1}, Clients),
            changeState(State#state{name=racing}, State);
        true ->            
            State
    end,
    
    {noreply, NewState};
    
handle_info(timerCheckingSlow, State) when State#state.name =:= hall ->
    PlayerMax = (State#state.lobbyInfo)#lobbyInfo.playerMax,
    NewState2 = if PlayerMax > 1 ->
        NewClients = kickClientsWithSlowConnection(State),
        NewState = State#state{clients=NewClients},
        TimerEnd = (NewState#state.lobbyInfo)#lobbyInfo.timerEnd,
        LobbyType = (NewState#state.lobbyInfo)#lobbyInfo.type,
        AllReady = lobby:checkAllLoaded(NewClients, TimerEnd, PlayerMax, LobbyType),
        if 
            AllReady ->
                utils:sendToAll({lobbyRaceEvent, initialize, "", -1}, NewClients),
                changeState(NewState#state{name=racing}, State);
            true ->            
                NewState
        end;
    true ->
        State
    end,
    
    {noreply, NewState2};
    
handle_info(finishTimerEnd, State) when State#state.name =:= racing ->    
    SortedClients = 
        lists:sort(
            fun({_, C1}, {_, C2}) ->
                compareFinishTime(C1, C2)
            end, dict:to_list(State#state.clients)),
    Now = utils:now(),
    {_, LobbyResults} =
        lists:foldl(
            fun({_, {_, ClientInfo}}, {Position, List}) ->
                OldRating =
                case dbUser:getRecord(id, ClientInfo#clientInfo.userID) of
                    {error, noUserID, _} ->
                        0;
                    User ->
                        User#user.rating
                end,

                {ISFinished,FinishTime} = if ClientInfo#clientInfo.lastTime =:= 0
                                 -> {false,Now};
                               true -> {true, ClientInfo#clientInfo.lastTime}
                            end,
                StartTime = if State#state.startTime > 0 -> State#state.startTime; true-> Now end,
                Result = #lobbyResult{
                    clientID = ClientInfo#clientInfo.clientID,
                    userID = ClientInfo#clientInfo.userID,
                    displayName = ClientInfo#clientInfo.displayName,
                    time = FinishTime - StartTime,
                    position = Position,
                    oldRating = OldRating,
                    money = 10,
                    finished = ISFinished,
                    city=ClientInfo#clientInfo.homeCity
                },
                {Position + 1, [Result | List]}
            end, {1, []}, SortedClients),
    
    LobbyResults2 = lists:reverse(LobbyResults),
    {BlueCount, RedCount} = getClientCountsByCity(State#state.clients),
    CountTotal = BlueCount + RedCount,
    LobbyResultsWithRating = case (State#state.lobbyInfo)#lobbyInfo.type of
        team ->
            users:calculateScores(State#state.lobbyInfo, 
                                  users:calculateMoney(State#state.lobbyInfo,
                                                       users:calculateRatings(LobbyResults2, State#state.lobbyInfo),
                                                       CountTotal),
                                  CountTotal);
        _Other ->
            users:calculateMoney(State#state.lobbyInfo, users:calculateRatings(LobbyResults2, State#state.lobbyInfo),CountTotal)
    end,    
    {NewLobbyResults, WearOut, LevelUps, BlueScore, RedScore} = users:considerLobbyResults(LobbyResultsWithRating, State#state.lobbyInfo, BlueCount, RedCount),
    users:sendLevelUpMessages(LevelUps, State#state.clients),
    Upg = carUpgrades:getUpgradesList(State#state.clients),
    addKilometers(State#state.clients, State#state.lobbyInfo),
    Message = {lobbyRaceResults, NewLobbyResults, Upg, WearOut, BlueScore, RedScore},
    utils:sendToAllDifferent(Message, State#state.clients, fun carUpgrades:transformMessage/2),
    kickClientsWithNotEnoughFuel(NewLobbyResults, State),
    NewState = changeState(State#state{name=hall}, State),
    
    {noreply, NewState};
    
handle_info(Unexpected, State) ->
    log:write(warning, ?MODULE_STRING, (State#state.lobbyInfo)#lobbyInfo.id, State#state.name, "Unexpected message: ~10000p~n", [Unexpected]),
    
    {noreply, State}.
    
%%-----------------------------------------------------------------------------
%% Code change
%%-----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% Terminate
%%-----------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.    

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
compareFinishTime({PID1,ClientInfo1}, {PID2,ClientInfo2}) ->
    Time1 = ClientInfo1#clientInfo.lastTime,
    Time2 = ClientInfo2#clientInfo.lastTime,
    if
        Time1 > 0 andalso Time2 > 0 ->
            (Time1 < Time2);
        Time1 > 0 andalso Time2 =:= 0 ->
            true;
        Time1 =:= 0 andalso Time2 > 0 ->
            false;
        Time1 =:= 0 andalso Time2 =:= 0 ->
            if
                (PID1 =:= undefined andalso PID2 =/= undefined) ->
                    false;
                (PID1 =/= undefined andalso PID2 =:= undefined) ->
                    true;
                true ->
                    true
            end    
    end.  
    
updateLobbyStatus(State) ->
    NewStatus = list_to_atom("enum_status_" ++ atom_to_list(State#state.name)),
    NewLobbyInfo2 = (State#state.lobbyInfo)#lobbyInfo{status=NewStatus},
    State#state{lobbyInfo = NewLobbyInfo2}.
        
checkAllQuit(Clients) ->
    L = dict:to_list(Clients),
    lists:all(
        fun({_, {ClientPID, _}}) -> 
            ClientPID =:= undefined 
        end, L).

getClientsCount(Clients) ->
    dict:fold(
        fun(_, {ClientPID, _}, Acc) ->
                if 
                    ClientPID =/= undefined ->
                        Acc+1; 
                    true ->
                        Acc
                end
        end, 0, Clients).
        
setAllReadiness(Value, Clients) ->
    dict:map(
        fun(_, {ClientPID, ClientInfo}) -> 
            {ClientPID, ClientInfo#clientInfo{readiness = Value}} 
        end, Clients).

checkAllReadiness(Value, Clients) ->
    L = dict:to_list(Clients),
    lists:all(
        fun({_, {_, ClientInfo}}) -> 
            ClientInfo#clientInfo.readiness =:= Value 
        end, L).
        
checkAllLoaded(Clients, TimerEnd, PlayerMax, LobbyType) ->
    IsTimerEnded = ((TimerEnd > 0) and (utils:now() >= TimerEnd-100)) or (PlayerMax =:= 1 andalso LobbyType =:= normal),
    if 
        not IsTimerEnded ->            
            false;
        true ->
            checkAllReadiness(loaded, Clients)
    end.
    
sendAllAddClientMessages(Clients, FromClientPID) ->
    dict:fold(
        fun(_ClientID, {_ClientPID, ClientInfo}, _) ->
            FromClientPID ! {addClient, lobby, ClientInfo}
        end, [], Clients).
 
checkAllFinished(Clients) ->
    L = dict:to_list(Clients),
    lists:all(
        fun({_, {ClientPID, ClientInfo}}) ->
            ClientPID =:= undefined orelse ClientInfo#clientInfo.lastTime > 0 
        end, L).
        
kickClientsWithNotEnoughFuel(LobbyResults, State) ->
    Clients = State#state.clients,
    LobbyInfo = State#state.lobbyInfo,
    
    lists:foldl(
        fun(LobbyResult, _NewClients) ->
            ClientID = LobbyResult#lobbyResult.clientID,
            {ok, {_, ClientInfo}} = dict:find(ClientID, Clients),
            _User = dbUser:getRecord(id, ClientInfo#clientInfo.userID),
            case fuel:checkFuel(LobbyInfo, ClientInfo) of
                false ->
                    %log:write("User ~p has been kicked because he hasn't got enough fuel~n", [User#user.name]),
                    utils:sendMessageToClient(ClientID, {quitLobby, "[[notEnoughFuelGoToGasStation]]"}, State#state.clients);
                true -> 
                    case dbCar:checkCarCondition(ClientInfo#clientInfo.userID) of
                        false ->
                            %log:write("User ~p has been kicked because he hasn't got enough car~n", [User#user.name]),
                             utils:sendMessageToClient(ClientID, {quitLobby, "[[carIsTooDamagedToContinueRacing]]"}, State#state.clients);
                        true ->
                            if LobbyInfo#lobbyInfo.league == 4 ->
                                    utils:sendMessageToClient(ClientID, {quitLobby, ""}, State#state.clients);
                                true -> ok
                            end
                    end
            end
        end, Clients, LobbyResults
    ).
    
kickClientsWithSlowConnection(State) ->
    Clients = State#state.clients,

    case dict:size(Clients) > 2 of
        true ->
            dict:fold(
                fun(ClientID, Client, NewClients) ->
                    {_, ClientInfo} = Client,
                    _User = dbUser:getRecord(id, ClientInfo#clientInfo.userID),
                    TimeDiff = (utils:now() - ClientInfo#clientInfo.loadingStartTimestamp) / 1000.0 + 0.1,
                    
                    if TimeDiff >= 15 andalso ClientInfo#clientInfo.readiness =:= none ->
                        LoadingSpeed = (ClientInfo#clientInfo.loadingProgress + 1) / TimeDiff,
                        
                        if LoadingSpeed < 0.5 ->
                            %log:write("User ~p has been kicked because his connection is veeery slow~n", [User#user.name]),
                            utils:sendMessageToClient(ClientID, {quitLobby, "[[connectionTooSlow]]"}, State#state.clients),
                            utils:sendToAllExceptOne({removeClient, lobby, ClientID}, State#state.clients, ClientID),
                            NewClients;
                        true ->
                            dict:store(ClientID, Client, NewClients)
                        end;
                    true->
                        dict:store(ClientID, Client, NewClients)
                    end
                end, dict:new(), Clients
            );
        false ->
            Clients
    end.

changeState(NewState, OldState) ->    
    log:write(debug, ?MODULE_STRING, (NewState#state.lobbyInfo)#lobbyInfo.id, OldState#state.name, "==>> ~w ~n", [NewState#state.name]),
    
    if 
        (OldState#state.name =:= hall) andalso (NewState#state.name =:= racing) ->
            timer:cancel((OldState#state.lobbyInfo)#lobbyInfo.timerCheckingSlow),
            NewState2 = NewState#state{lobbyInfo = (NewState#state.lobbyInfo)#lobbyInfo{timerEnd=0, timerCheckingSlow=undefined}};
        (OldState#state.name =:= racing) andalso (NewState#state.name =:= hall) ->
            FilteredClients = dict:filter(
                fun(_, {ClientPID, _}) -> 
                    ClientPID =/= undefined 
                end, NewState#state.clients),
                
            Now = utils:now(),
                
            NewClients = dict:map(
                fun(_, {ClientPID, ClientInfo}) ->
                    {ClientPID, ClientInfo#clientInfo{lastTime = 0, readiness=loaded, loadingStartTimestamp=Now}}
                end, FilteredClients),
                
            TimerLength = (NewState#state.lobbyInfo)#lobbyInfo.timerLength,
            
            case canRestartWaitingTimer(NewState#state.lobbyInfo, NewClients) of
                true ->
                    TimerEnd = utils:now() + TimerLength,
                    timer:send_after(TimerLength, self(), timerEnd);
                false ->
                    TimerEnd = (NewState#state.lobbyInfo)#lobbyInfo.timerEnd
            end,
            
            {ok, TimerCheckingSlow} = timer:send_interval(?CHECKING_CONNECTION_TIMER, self(), timerCheckingSlow),
            
            NewLobbyInfo = (NewState#state.lobbyInfo)#lobbyInfo{timerEnd=TimerEnd, timerCheckingSlow=TimerCheckingSlow},
            NewState2 = NewState#state{finishTimerID=undefined, clients=NewClients, lobbyInfo=NewLobbyInfo};
        true ->
            NewState2 = NewState
    end,

    NewState3 = updateLobbyStatus(NewState2),    
    
    lobbyManager:updateLobbyInfo(NewState3#state.lobbyManagerPID, NewState3#state.lobbyInfo),
    
    NewState3.
    
addKilometers(Clients, LobbyInfo) ->
    dict:map(fun(_Key, {ClientPID, _ClientInfo}) -> 
        if ClientPID =/= undefined ->
            ClientPID ! {addKilometers, LobbyInfo};
        true -> ok end
    end, Clients).
    
canStartWaitingTimer(LobbyInfo, Clients) ->
    case LobbyInfo#lobbyInfo.type of
        team ->
            if LobbyInfo#lobbyInfo.league =/= 4 ->
                {Blue, Red} = getClientCountsByCity(Clients),
                Blue >= 1 andalso Red >= 1 andalso LobbyInfo#lobbyInfo.timerEnd =:= 0;
            true ->
                dict:size(Clients) =:= 2
            end;
        _Other ->
            dict:size(Clients) =:= 2
    end.
    
canStopWaitingTimer(LobbyInfo, Clients) ->
    case LobbyInfo#lobbyInfo.type of
        team ->
            {Blue, Red} = getClientCountsByCity(Clients),
            (Blue < 1 orelse Red < 1) andalso LobbyInfo#lobbyInfo.timerEnd > 0;
        _Other ->
            dict:size(Clients) =:= 1
    end.
    
canRestartWaitingTimer(LobbyInfo, Clients) ->
    case LobbyInfo#lobbyInfo.type of
        team ->
            {Blue, Red} = getClientCountsByCity(Clients),
            Blue >= 1 andalso Red >= 1;
        _Other ->
            dict:size(Clients) > 1
    end.      
        
getClientCountsByCity(Clients) ->
    dict:fold(fun(_, {_, ClientInfo}, {Blue, Red}) ->
        case ClientInfo#clientInfo.homeCity of
            1 -> {Blue + 1, Red};
            2 -> {Blue, Red + 1}
        end
    end, {0, 0}, Clients).
