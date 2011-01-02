package com.turbostool.client.lobbies
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.Tracker;
import com.turbostool.client.city.ScreenSelectedCommand;
import com.turbostool.client.event.AuthorizationChangedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.LocalLapTimeEvent;
import com.turbostool.client.event.ProgressEvent2;
import com.turbostool.client.event.RouteAndCarLoadErrorEvent;
import com.turbostool.client.event.RouteAndCarLoadedEvent;
import com.turbostool.client.game.BaseRaceController;
import com.turbostool.client.game.GameEngine;
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.RouteAndCarLoader;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarLoader;
import com.turbostool.client.game.route.RouteLoader;
import com.turbostool.client.game.view.ModalManager;
import com.turbostool.client.lobbies.events.CreateLobbyCommand;
import com.turbostool.client.lobbies.events.JoinLobbyCommand;
import com.turbostool.client.lobbies.events.QuitLobbyCommand;
import com.turbostool.client.lobbies.events.RefreshLobbiesCommand;
import com.turbostool.client.lobbies.events.ScreenInitializedEvent;
import com.turbostool.client.model.ClientInfo;
import com.turbostool.client.model.LobbyInfo;
import com.turbostool.client.model.RouteInfo;
import com.turbostool.client.model.TipInfo;
import com.turbostool.client.model.UpgradeInfo;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.AddClientMessage;
import com.turbostool.client.net.messages.ChangeTriggerMessage;
import com.turbostool.client.net.messages.ChanneledMessage;
import com.turbostool.client.net.messages.CreateLobbyRequest;
import com.turbostool.client.net.messages.CreateLobbyResponse;
import com.turbostool.client.net.messages.DisconnectRaceRequest;
import com.turbostool.client.net.messages.GetLobbyRequest;
import com.turbostool.client.net.messages.GetPropertyRequest;
import com.turbostool.client.net.messages.GetPropertyResponse;
import com.turbostool.client.net.messages.JoinLobbyRequest;
import com.turbostool.client.net.messages.JoinLobbyResponse;
import com.turbostool.client.net.messages.LapTimeMessage;
import com.turbostool.client.net.messages.LoadingProgressMessage;
import com.turbostool.client.net.messages.LobbyRaceEventMessage;
import com.turbostool.client.net.messages.LobbyRaceResultsMessage;
import com.turbostool.client.net.messages.QuitLobbyMessage;
import com.turbostool.client.net.messages.QuitLobbyRequest;
import com.turbostool.client.net.messages.ReadinessRequest;
import com.turbostool.client.net.messages.ReasonedResponse;
import com.turbostool.client.net.messages.RemoveClientMessage;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.registration.SetMapMessageCommand;
import com.turbostool.client.screens.raceResults.LobbyResults;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.collections.FilteredCollection;

import flash.events.*;

import mx.collections.ArrayCollection;
import mx.controls.Alert;

public class LobbiesController extends BaseRaceController
{
    [Bindable(event="changeLobbies")]
    public var lobbies: Array;

    [Bindable]
    public var league:Number = 1;

    private var _lobbyInfo: LobbyInfo;
    private var _loaded: Boolean = false;
    private var _finishTimer: Number = 0;
    private var _needRefreshLobbies: Boolean = false;

    private var _waitingLobbiesResponse: Boolean = false;

    private var _routeAndCarLoader: RouteAndCarLoader = null;
    private var _crossedStartLine: Boolean = false;

    [Bindable]
    private var _nitroUsageCount: Number = 0;

    override public function get isRacing(): Boolean
    {
        return state == LobbyInfo.STATUS_RACING;
    }

    public function get needRefreshLobbies():Boolean
    {
        return _needRefreshLobbies;
    }

    public function set needRefreshLobbies(value:Boolean):void
    {
        _needRefreshLobbies = value;
    }

    [Bindable]
    public var showAllLobbies: Boolean = false;

    [Bindable]
    public function get waitingLobbiesResponse():Boolean
    {
        return _waitingLobbiesResponse;
    }

    public function set waitingLobbiesResponse(val:Boolean):void
    {
        _waitingLobbiesResponse = val;
    }

    override protected function onAuthorizationChanged(event: AuthorizationChangedEvent): void
    {
        if (event.authorized)
        {
            startTimers();
        }
        else
        {
            stopTimers();
        }
    }

    public function LobbiesController(modelsStorage: ModelsStorage, raceModel: RaceModel, socket: SessionSocket, gameEngine: GameEngine)
    {
        super(modelsStorage, raceModel, socket, gameEngine, EventManager.lobbyRaceChannel);

        _eventChannel.addEventListener(RefreshLobbiesCommand.REFRESH_LOBBIES_LIST, onRefreshLobbies);
        _eventChannel.addEventListener(GetPropertyResponse.GET_LOBBIES_RESPONSE, onLobbiesResponse);

        _eventChannel.addEventListener(CreateLobbyCommand.CREATE_LOBBY_COMMAND, onCreateLobbyCommand);
        _eventChannel.addEventListener(CreateLobbyResponse.CREATE_LOBBY, onCreateLobbyResponse);

        _eventChannel.addEventListener(JoinLobbyCommand.JOIN_LOBBY_COMMAND, onJoinLobbyCommand);
        _eventChannel.addEventListener(JoinLobbyResponse.JOIN_LOBBY, onJoinLobbyResponse);

        _eventChannel.addEventListener(GetPropertyResponse.GET_LOBBY_RESPONSE, onGetLobbyResponse);

        _eventChannel.addEventListener(QuitLobbyCommand.QUIT_LOBBY_COMMAND, onQuitLobbyCommand);
        _eventChannel.addEventListener(QuitLobbyMessage.QUIT_LOBBY, onQuitLobbyMessage);

        _eventChannel.addEventListener(LoadingProgressMessage.LOADING_PROGRESS, onLoadingProgressMessage);

        _eventChannel.addEventListener(LobbyRaceResultsMessage.LOBBY_RACE_RESULTS, onLobbyResults);
        _eventChannel.addEventListener(LobbyRaceEventMessage.LOBBY_RACE_EVENT, onLobbyRaceEvent);
        EventManager.instance.addEventListener("nitroUsed", onNitroUsed);

        state = LobbyInfo.STATUS_NONE;
    }

    [Bindable(event="changeTimer")]
    public function get finishTimer(): Number
    {
        var t: Number = _finishTimer - Utils.now();
        return t > 0 ? t : 0;
    }

    override protected function onFastUpdateTimer(): void
    {
        if (state == LobbyInfo.STATUS_RACING)
        {
            dispatchEvent(new Event("changeTimer"));
            dispatchEvent(new Event("changeRaceModel"));
        }
    }

    [Bindable]
    public function get lobbyInfo(): LobbyInfo
    {
        return _lobbyInfo;
    }

    [Bindable]
    override public function get state(): String
    {
        return _state;
    }

    override public function set state(val: String):void
    {
        super.state = val;
        if (state == LobbyInfo.STATUS_RACING)
        {
            _gameEngine.bindCanvas(this, "renderingCanvas");
            onStartGameplay();
            ModalManager.instance.closeAllWindows();
        } else
        if (state == LobbyInfo.STATUS_HALL)
        {
            dispatchEvent(new Event("randomTipChanged"));
        }
    }

    public function set lobbyInfo(val: LobbyInfo): void
    {
        _lobbyInfo = val;
        dispatchEvent(new Event("changeRouteImage"));
    }

    override public function set loadingProgress(value: int): void
    {
        super.loadingProgress = value;
        if (state == LobbyInfo.STATUS_HALL)
        {
            _clientInfo.loadingProgress = value;
            clients.itemUpdated(_clientInfo);
            _socket.sendMessage(new LoadingProgressMessage(value, ChanneledMessage.LOBBY_CHANNEL));
        }
    }

    [Bindable(event="changeRouteImage")]
    public function get routeImageURL(): String
    {
        if (lobbyInfo == null)
            return null;
        var routeInfo: RouteInfo = Client.instance.modelsStorage.getRouteInfoByID(lobbyInfo.routeID);
        return routeInfo.imageURL;
    }

    override protected function onScreenInitialized(event: ScreenInitializedEvent): void
    {
        super.onScreenInitialized(event);

        _socket.sendMessage(new ReadinessRequest(ReadinessRequest.INITIALIZED));
    }

    private function onLobbyRaceEvent(event: ServerResponseEvent): void
    {
        var response: LobbyRaceEventMessage = LobbyRaceEventMessage(event.response);
        if (response.type == LobbyRaceEventMessage.OTHER_CAR_FINISHED)
        {
            //todo send this from server
            //EventManager.instance.dispatchEvent(new ChatMessage("Финишировал, время: " + Utils.formatTime(event.intParam), event.clientID, ChatMessage.CHANNEL_LOBBY));
        }
        else if (response.type == LobbyRaceEventMessage.FINISH_COUNTDOWN_STARTED)
        {
            _finishTimer = response.floatParam;
        }
        else if (response.type == LobbyRaceEventMessage.GO)
        {
            var expectedRouteName: String = Client.instance.modelsStorage.getRouteInfoByID(lobbyInfo.routeID).fileNameBase;
            var actualRouteName: String = _gameEngine.raceModel.getRaceWorld().getName();
            if (expectedRouteName == actualRouteName)
            {
                EventManager.instance.carControlsEnabled = true;
                _startTime = response.floatParam;
                setFocus();
            } else
            {
                Alert.show("Ошибка: " + expectedRouteName + " " + actualRouteName + ". Пожалуйста, напишите в официальную группу - что именно вы делали перед тем, как возникла ошибка.");
                Tracker.instance.trackEvent("errors", "routeMismatch", expectedRouteName + "/" + actualRouteName);
                SessionSocket.instance.sendMessage(new QuitLobbyRequest());
            }
        }
        else if (response.type == LobbyRaceEventMessage.INITIALIZE)
        {
            EventManager.instance.carControlsEnabled = false;
            state = LobbyInfo.STATUS_RACING;
        }
    }

    private function onLobbyResults(event:ServerResponseEvent): void
    {
        var resultView: LobbyResults = new LobbyResults();
        var lobbyResultMessage: LobbyRaceResultsMessage = LobbyRaceResultsMessage(event.response);
        resultView.resultMessage = lobbyResultMessage;
        resultView.isDuel = (league == 4);
        resultView.lobbiesController = this;
        Client.instance.modelsStorage.userInfo.fuel = lobbyResultMessage.fuel;
        ModalManager.instance.addModalWindow(resultView, Event.CLOSE);

        applyAllUpgrades(LobbyRaceResultsMessage(event.response).upgrades);

        state = LobbyInfo.STATUS_HALL;
        onStopGameplay();

        SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.CITIES));

        _finishTimer = 0;
        _startTime = 0;
        _crossedStartLine = false;

        if (_modelsStorage.userInfo.tutorialStage == 1)
        {
            Utils.nextTutorialStage();
        }
        else
            if (_modelsStorage.userInfo.tutorialStage == 2)
            {
                Utils.nextTutorialStage();
            EventManager.instance.dispatchEvent(new SetMapMessageCommand("МЫ ВПЕЧАТЛЕНЫ ТВОИМИ УСПЕХАМИ, ПОЛУЧИ 2200 РУБЛЕЙ"));
            }

        if (lobbyInfo.playerMax <= 1)
        {
            EventManager.lobbyRaceChannel.dispatchEvent(new QuitLobbyCommand());
        }
        else
        {
            _socket.sendMessage(new GetLobbyRequest(lobbyInfo.id));
        }

        if (_modelsStorage.userInfo.tutorialStage <= 2)
        {
            EventManager.lobbyRaceChannel.dispatchEvent(new QuitLobbyCommand());
            EventManager.instance.dispatchEvent(new ScreenSelectedCommand("map"));
        }

        _socket.sendMessage(new GetPropertyRequest(GetPropertyRequest.USER_INFO));

        if (resultView.isDuel)
            Tracker.instance.trackEvent("duel", "finished", lobbyInfo.routeName + "/" + lobbyInfo.direction);
        else {
            if(lobbyInfo.type == LobbyInfo.LOBBY_TYPE_NORMAL) {
                Tracker.instance.trackEvent("race", "Level: " + _modelsStorage.userInfo.level, lobbyInfo.routeName + "/" + lobbyInfo.lapNumber + "/" + lobbyInfo.direction, resultView.userResult.money);
            } else if(lobbyInfo.type == LobbyInfo.LOBBY_TYPE_TEAM) {
                Tracker.instance.trackEvent("teamRace", "Level: " + _modelsStorage.userInfo.level, lobbyInfo.routeName + "/" + lobbyInfo.lapNumber + "/" + lobbyInfo.direction, resultView.userResult.money);
            }
        }
    }

    private function onNitroUsed(event: Event): void
    {
        _nitroUsageCount = _nitroUsageCount + 1;
    }

    private function applyAllUpgrades(arr:Array):void
    {
        for each(var info: UpgradeInfo in arr)
        {
            var clientIndex:int = findClientIndexInfoByID(info.userID);
            if (clientIndex == -1)
            {
                _clientInfo.upgradeInfo = info;
            }
            else
            {
                (clients[clientIndex] as ClientInfo).upgradeInfo = info;
            }
        }
    }

    override protected function onEachSecondUpdateTimer():void
    {
        super.onEachSecondUpdateTimer();
        if (lobbyInfo != null)
        {
            lobbyInfo.fireUpdateTimer();
        }
    }

    override protected function onSlowUpdateTimer():void
    {
        super.onSlowUpdateTimer();
        if (!waitingLobbiesResponse && state == LobbyInfo.STATUS_NONE && _needRefreshLobbies)
        {
            onRefreshLobbies(null);
        }
    }

    override protected function onRouteAndCarLoadError(event: RouteAndCarLoadErrorEvent): void
    {
        _routeAndCarLoader.removeEventListener(RouteAndCarLoadedEvent.ROUTE_AND_CAR_LOADED, onRouteAndCarLoaded);
        _routeAndCarLoader.removeEventListener(RouteAndCarLoadErrorEvent.ROUTE_AND_CAR_LOAD_ERROR, onRouteAndCarLoadError);
        _routeAndCarLoader = null;

        if (state == LobbyInfo.STATUS_HALL)
        {
            Alert.show(str("Ошибка загрузки трассы"));
            onQuitLobbyCommand(null);
            Tracker.instance.trackPageview("/city/arena/hall/error?message=Ошибка загрузки трассы");
        }
    }

    override protected function onRouteAndCarLoaded(event: RouteAndCarLoadedEvent): void
    {
        _routeAndCarLoader.removeEventListener(RouteAndCarLoadedEvent.ROUTE_AND_CAR_LOADED, onRouteAndCarLoaded);
        _routeAndCarLoader.removeEventListener(RouteAndCarLoadErrorEvent.ROUTE_AND_CAR_LOAD_ERROR, onRouteAndCarLoadError);
        _routeAndCarLoader = null;

        super.onRouteAndCarLoaded(event);
        if (state == LobbyInfo.STATUS_HALL)
        {
            loadingProgress = 100;
            try
            {
                setupRouteAndCar(event.raceWorld, lobbyInfo.lapNumber, lobbyInfo.isReverse, event.car.clone());
            }
            catch(e:Error)
            {
                Alert.show(e.getStackTrace());
                Tracker.instance.trackPageview('/city/arena/hall/error?message=Ошибка инициализации трассы');
            }

            _loaded = true;
            _socket.sendMessage(new ReadinessRequest(ReadinessRequest.LOADED));
        }
    }

    private function onProgress(e: ProgressEvent2):void
    {
        if (state == LobbyInfo.STATUS_HALL)
        {
            loadingProgress = e.current / e.total * 100;
        }
    }

    private function getClientInfoByID(clientID: int): ClientInfo
    {
        for each(var info: ClientInfo in clients)
        {
            if (info.id == clientID)
            {
                return info;
            }
        }

        return null;
    }

    private function addSelfToClients(upg:UpgradeInfo): void
    {
        var carID: int = _lobbyInfo.allowedCarID > 0 ? _lobbyInfo.allowedCarID : _modelsStorage.userInfo.currentCarID;
        _clientInfo = new ClientInfo(Car.LOCAL_CAR_ID, _modelsStorage.userInfo.displayName, carID, 0, upg);
        _clientInfo.carName = _modelsStorage.userInfo.carFileName;
        _clientInfo.carColor = _modelsStorage.userInfo.color;
        _clientInfo.homeCity = _modelsStorage.userInfo.homeCity;
        clients.addItem(_clientInfo);
    }

    private function onLoadingProgressMessage(event: ServerResponseEvent): void
    {
        var reponse: LoadingProgressMessage = LoadingProgressMessage(event.response);
        var info: ClientInfo = getClientInfoByID(reponse.clientID);
        if (info != null)
        {
            info.loadingProgress = reponse.progress;
            clients.itemUpdated(info);
        }
    }

    override protected function onAddClient(event: ServerResponseEvent): void
    {
        var addClientMessage: AddClientMessage = AddClientMessage(event.response);
        super.onAddClient(event);

        _socket.sendMessage(new GetLobbyRequest(lobbyInfo.id));

        if (_loaded)
            SessionSocket.instance.sendMessage(new ReadinessRequest(ReadinessRequest.LOADED));

        dispatchEvent(new Event("updatePlayers"));

    }

    override protected function onRemoveClient(event: ServerResponseEvent): void
    {
        var removeClientMessage: RemoveClientMessage = RemoveClientMessage(event.response);
        super.onRemoveClient(event);
        var car: Car = _raceModel.getRemoteCarByID(removeClientMessage.clientID);
        if (car != null)
        {
            _gameEngine.removeCar(car);
        }

        _socket.sendMessage(new GetLobbyRequest(lobbyInfo.id));

        dispatchEvent(new Event("updatePlayers"));
    }

    private function onQuitLobbyCommand(event: QuitLobbyCommand): void
    {
        _socket.sendMessage(new QuitLobbyRequest());
    }

    private function onQuitLobbyMessage(event: ServerResponseEvent): void
    {
        var message: QuitLobbyMessage = QuitLobbyMessage(event.response);
        state = LobbyInfo.STATUS_NONE;
        if (message.reason.length > 0)
        {
            Alert.show(message.reason);
        }
    }

    private function onJoinLobbyCommand(event:JoinLobbyCommand):void
    {
        _socket.sendMessage(new DisconnectRaceRequest());
        if (event.lobbyInfo != null)
        {
            trace("onJoinLobbyCommand");
            _socket.sendMessage(new JoinLobbyRequest(event.lobbyInfo.id));
        }
    }

    private function cameToHall(upg:UpgradeInfo): void
    {
        _loaded = false;
        clients.removeAll();
        addSelfToClients(upg);
        startLoading();
    }

    private function startLoading():void
    {
        var carID: int = _lobbyInfo.allowedCarID > 0 ? _lobbyInfo.allowedCarID : _modelsStorage.userInfo.currentCarID;
        if (_routeAndCarLoader != null)
        {
            _routeAndCarLoader.removeEventListener(RouteAndCarLoadedEvent.ROUTE_AND_CAR_LOADED, onRouteAndCarLoaded);
            _routeAndCarLoader.removeEventListener(RouteAndCarLoadErrorEvent.ROUTE_AND_CAR_LOAD_ERROR, onRouteAndCarLoadError);
        }
        _routeAndCarLoader = new RouteAndCarLoader(RouteLoader.instance, CarLoader.instance);
        _routeAndCarLoader.addEventListener(RouteAndCarLoadedEvent.ROUTE_AND_CAR_LOADED, onRouteAndCarLoaded);
        _routeAndCarLoader.addEventListener(RouteAndCarLoadErrorEvent.ROUTE_AND_CAR_LOAD_ERROR, onRouteAndCarLoadError);
        _routeAndCarLoader.load(Client.instance.modelsStorage.getRouteInfoByID(lobbyInfo.routeID).fileNameBase, Client.instance.modelsStorage.userInfo.racingCarName, Car.LOCAL_CAR_ID);

    }

    private function onJoinLobbyResponse(event: ServerResponseEvent):void
    {
        var response: JoinLobbyResponse = JoinLobbyResponse(event.response);
        if (JoinLobbyResponse(event.response).result == ReasonedResponse.RESULT_ERROR)
        {
            Alert.show(str("errorJoiningRace") + ": " + response.message);
            Tracker.instance.trackPageview('/city/arena/error?message=' + response.message);
        }
        else
            if (JoinLobbyResponse(event.response).result == ReasonedResponse.RESULT_OK)
            {
                lobbyInfo = response.lobbyInfo;
                state = LobbyInfo.STATUS_HALL;
                cameToHall(response.upgradeInfo);
                dispatchEvent(new Event("updatePlayers"));
            }
    }

    private function onGetLobbyResponse(event: ServerResponseEvent):void
    {
        var response: GetPropertyResponse = GetPropertyResponse(event.response);
        if (response.result == ReasonedResponse.RESULT_ERROR)
        {
            Tracker.instance.trackPageview('/city/arena/error?message=' + response.message);
        }
        else
            if (response.result == ReasonedResponse.RESULT_OK)
            {
                lobbyInfo = LobbyInfo(response.property);
            }
    }

    private function onCreateLobbyCommand(event: CreateLobbyCommand): void
    {
        _socket.sendMessage(new DisconnectRaceRequest());
        _socket.sendMessage(event.createLobbyRequest);
    }

    private function onCreateLobbyResponse(event: ServerResponseEvent):void
    {
        var response: CreateLobbyResponse = CreateLobbyResponse(event.response);
        if (response.result == CreateLobbyResponse.RESULT_ERROR)
        {
            Alert.show(str("errorCreatingRace") + ": " + response.message);
            Tracker.instance.trackPageview('/city/arena/error?message=' + response.message);
        }
        else
            if (response.result == CreateLobbyResponse.RESULT_OK)
            {
                state = LobbyInfo.STATUS_HALL;
                lobbyInfo = response.lobbyInfo;
                cameToHall(response.upgradeInfo);
                dispatchEvent(new Event("updatePlayers"));
            }
    }

    private function onLobbiesResponse(e: ServerResponseEvent): void
    {
        var response: GetPropertyResponse = GetPropertyResponse(e.response);
        waitingLobbiesResponse = false;
        function accessableFilter(lobby: LobbyInfo, index:int, array:Array):Boolean
        {
            return lobby.mayJoin;
        }

        lobbies = response.propertyAsArray;
        if (!showAllLobbies) lobbies = lobbies.filter(accessableFilter);
        function lobbiesComparator(lobby1: LobbyInfo, lobby2: LobbyInfo): Number
        {
            //первое выше, -1
            if (lobby1.mayJoin && !lobby2.mayJoin) return -1;
            if (!lobby1.mayJoin && lobby2.mayJoin) return 1;
            if (league != 4)
            {

                var secs1: int = lobby1.secsToStart;
                var secs2: int = lobby2.secsToStart;
                if ((lobby1.playerCount > 1) && ( secs1 == 0 ))
                {
                    return -1;
                }
                if ((lobby2.playerCount > 1) && ( secs2 == 0 ))
                {
                    return 1;
                }
                if (secs1 == 0)
                {
                    if (secs2 == 0) return 0;
                    if (secs2 > 0) return 1;
                }
                if (secs2 == 0)
                {
                    if (secs1 == 0) return 0;
                    if (secs1 > 0) return -1;
                }
                return ( secs1 < secs2 ) ? -1 : 1;
            }
            else
            {
                return (lobby1.creatorRating > lobby2.creatorRating) ? -1 : 1;
            }
        }

        lobbies.sort(lobbiesComparator);
        dispatchEvent(new Event("changeLobbies"));
    }

    public function onRefreshLobbies(e: Event): void
    {
        waitingLobbiesResponse = true;
        _socket.sendMessage(new GetPropertyRequest(GetPropertyRequest.LOBBIES));
    }

    override protected function onLocalLapTimeEvent(e: LocalLapTimeEvent): void
    {
        if(state == LobbyInfo.STATUS_RACING)
        {
            super.onLocalLapTimeEvent(e);
            dispatchEvent(new Event("changeLapText"));

            if(e.lapTimeMessage.type == LapTimeMessage.FINISH)
            {
                _socket.sendMessage(new LapTimeMessage(LapTimeMessage.FINISH, e.lapTimeMessage.time, Car.LOCAL_CAR_ID, ChanneledMessage.LOBBY_CHANNEL));
                _socket.sendMessage(new ChangeTriggerMessage(ChangeTriggerMessage.CLICK_ON_CAR_TIP, -1));
                _socket.sendMessage(new ChangeTriggerMessage(ChangeTriggerMessage.HOW_TO_DRIVE_TIP, -1));
            }
            else if(e.lapTimeMessage.type == LapTimeMessage.START)
            {
                _crossedStartLine = true;
            }
        }
    }

    [Bindable(event="changeLapText")]
    public function lapText():String
    {
        if(raceModel.localCar.currentLap == -1) {
            return "1/" + raceModel.lapNumber;
        } else if(raceModel.localCar.currentLap < raceModel.lapNumber) {
            return (raceModel.localCar.currentLap + 1) + "/" + raceModel.lapNumber;
        } else {
            return (raceModel.localCar.currentLap) + "/" + raceModel.lapNumber;
        }

    }

    [Bindable(event="changeTimer")]
    override public function isTipVisible(type: String):Boolean
    {
        if (type == "howToMakeTheCarGo")
        {
            return !_crossedStartLine && _modelsStorage.userInfo.hasTrigger(ChangeTriggerMessage.CLICK_ON_CAR_TIP);
        }
        else if (type == "howToDrive")
        {
            return raceTime <= 15000 && _modelsStorage.userInfo.hasTrigger(ChangeTriggerMessage.HOW_TO_DRIVE_TIP);
        }
        else if (type == "howToUseNitro")
            {
                return _nitroUsageCount < 3 && _modelsStorage.userInfo.hasTrigger(ChangeTriggerMessage.NITRO_TIP);
            }

        return true;
    }

    [Bindable(event="updatePlayers")]
    public function playersByCity(city: Number): ArrayCollection
    {
        var c: ArrayCollection = new FilteredCollection(_clients.toArray(), function (client: ClientInfo): Boolean
        {
            return client.homeCity == city;
        });

        return c;
    }

    public function createTutorialLobby():void
    {
        var createLobbyCommand: CreateLobbyCommand = new CreateLobbyCommand(new CreateLobbyRequest(_modelsStorage.globalInfo.raceNewbieID, 1, "forward", -1, 1, 0, 1, 0));

        EventManager.lobbyRaceChannel.dispatchEvent(createLobbyCommand);
    }

    public function createQuickLobby(league: Number): void
    {
        var createLobbyCommand: CreateLobbyCommand = new CreateLobbyCommand(new CreateLobbyRequest(_modelsStorage.globalInfo.raceNewbieID, 1, "forward", -1, 5, 30000, league, 0));

        EventManager.lobbyRaceChannel.dispatchEvent(createLobbyCommand);
    }

    [Bindable(event="randomTipChanged")]
    public function getRandomTip(): String
    {
        var defaultMessage: String = str("useSafityBelts");
        try
        {
            var tips: Array = Client.instance.modelsStorage.tips;
            var ind: Number;
            var tip: TipInfo;
            ind = Math.floor(Math.random() * tips.length);
            tip = tips[ind] as TipInfo;
            if (tip != null)
            {
                return tip.message;
            }
        }
        catch(e: *)
        {

        }
        return defaultMessage;
    }
}
}