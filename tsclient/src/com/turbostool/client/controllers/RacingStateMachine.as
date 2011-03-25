package com.turbostool.client.controllers
{
import com.turbostool.client.event.CarLoadErrorEvent;
import com.turbostool.client.event.CarLoadedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.LoadingCanceledEvent;
import com.turbostool.client.event.RouteLoadErrorEvent;
import com.turbostool.client.event.RouteLoadedEvent;
import com.turbostool.client.event.StateChangedEvent;
import com.turbostool.client.game.GameEngine;
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarLoader;
import com.turbostool.client.game.route.RouteLoader;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.DisconnectResponse;
import com.turbostool.client.net.messages.LoadedMessage;
import com.turbostool.client.net.messages.SetPropertyResponse;
import com.turbostool.client.utils.Utils;

import flash.events.Event;

import mx.controls.Alert;
import mx.logging.Log;

public class RacingStateMachine extends BaseRacingStateMachine
{
    public static const DISCONNECTED: String = "disconnected";
    public static const LOADING_ROUTE: String = "loadingRoute";
    public static const LOADING_CAR: String = "loadingCar";
    public static const GAMEPLAY: String = "gameplay";

    // ссылки
    private var _socket: SessionSocket;
    private var _routeLoader: RouteLoader;
    private var _carLoader: CarLoader;

    private var _raceModel: RaceModel;
    private var _gameEngine: GameEngine;

    //todo переделать ?
    // может не надо торчать переменными наружу? ведь их можно забить обнулиить или еще что.. И кто то,
    // кто хочет их прочитать должен иметь ссылку на даннйы контроллер.
    // по другому можно их как-то заворачивать в StateChangedEvent, и получатель их из евента достанет

    public function RacingStateMachine(raceModel: RaceModel, gameEngine: GameEngine)
    {
        super();
        _logger = Log.getLogger(Utils.getClassName(this));
        _socket = SessionSocket.instance;
        _routeLoader = RouteLoader.instance;
        _carLoader = CarLoader.instance;
        _raceModel = raceModel;
        _gameEngine = gameEngine;

        EventManager.instance.addEventListener(RouteLoadedEvent.ROUTE_LOADED, processEvent);
        EventManager.instance.addEventListener(RouteLoadErrorEvent.ROUTE_LOAD_ERROR, processEvent);
        EventManager.instance.addEventListener(CarLoadedEvent.CAR_LOADED, processEvent);
        EventManager.instance.addEventListener(CarLoadErrorEvent.CAR_LOAD_ERROR, processEvent);
        EventManager.instance.addEventListener(LoadingCanceledEvent.LOADING_CANCELED, processEvent);
        EventManager.singleRaceChannel.addEventListener(DisconnectResponse.DISCONNECT, processEvent);
        EventManager.singleRaceChannel.addEventListener(SetPropertyResponse.SET_CURRENT_CAR_ID_RESPONSE, processEvent);

        EventManager.instance.addEventListener(StateChangedEvent.STATE_CHANGED_EVENT, processEvent);

        _state = DISCONNECTED;
        onEnterNewState(_state);
    }

    protected override function processEvent(event: Event): void
    {
        super.processEvent(event);

        switch (_state)
                {
            case DISCONNECTED:
                if (StateChangedEvent.check(event, Client.instance.controller.connectController, ConnectController.CONNECTED))
                {
                    changeState(LOADING_ROUTE);
                }
                break;

            case LOADING_ROUTE:
                switch (event.type)
                        {
                    case RouteLoadedEvent.ROUTE_LOADED:
                        // todo: рефакторить - брать из евента? (weak reference)
                        raceWorld = _routeLoader.getRaceWorld(RouteLoadedEvent(event).routeName);
                        changeState(LOADING_CAR);
                        break;
                    case RouteLoadErrorEvent.ROUTE_LOAD_ERROR:
                        _error = RouteLoadErrorEvent(event).error;
                        showAlert();
                        changeState(DISCONNECTED);
                        break;
                    case LoadingCanceledEvent.LOADING_CANCELED:
                        sendDisconnect();
                        break;
                    case DisconnectResponse.DISCONNECT:
                        changeState(DISCONNECTED);
                        break;
                }
                break;

            case LOADING_CAR:
                switch (event.type)
                        {
                    case CarLoadedEvent.CAR_LOADED:
                        car = CarLoadedEvent(event).car;
                        car.clientID = Car.LOCAL_CAR_ID;
                        changeState(GAMEPLAY);
                        break;
                    case CarLoadErrorEvent.CAR_LOAD_ERROR:
                        _error = CarLoadErrorEvent(event).error;
                        showAlert();
                        changeState(DISCONNECTED);
                        break;
                    case LoadingCanceledEvent.LOADING_CANCELED:
                        sendDisconnect();
                        break;
                    case DisconnectResponse.DISCONNECT:
                        changeState(DISCONNECTED);
                        break;
                }
                break;

            case GAMEPLAY:
                switch (event.type)
                        {
                    case DisconnectResponse.DISCONNECT:
                        changeState(DISCONNECTED);
                        break;
                }
                break;
        }
    }

    // действия, вызываемые при входе в состояние
    protected override function onEnterNewState(state: String): void
    {
        switch (state)
                {
            case LOADING_ROUTE:
                startLoadingRoute();
                break;
            case LOADING_CAR:
                startLoadingCar();
                break;
            case GAMEPLAY:
                sendLoadedNotification();
                break;
        }
    }

    // действия
    private function showAlert(): void
    {
        Alert.show(_error.toString());
    }

    private function startLoadingRoute(): void
    {
        var routeFilename: String = Client.instance.modelsStorage.getRouteInfoByID(_routeIDToLoad).fileNameBase;
        _routeLoader.loadWorldByName(routeFilename);
    }

    private function startLoadingCar(): void
    {
        // todo refactor - we should deal directly with carID only
        var racingCarName: String = Client.instance.modelsStorage.userInfo.racingCarName;
        var carFileName:String = Client.instance.modelsStorage.userInfo.carFileName;
        _carLoader.loadCar(racingCarName, carFileName, Car.LOCAL_CAR_ID);
    }

    private function sendLoadedNotification(): void
    {
        _socket.sendMessage(new LoadedMessage());
    }

    private function sendDisconnect(): void
    {
        //todo implement
        //_socket.sendQuitRaceMessage(new RouteLoadedRequest());
    }

    public function get routeLoader():RouteLoader
    {
        return _routeLoader;
    }

}
}