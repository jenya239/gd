package com.turbostool.client.game
{
import com.turbostool.client.Config;
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.event.AuthorizationChangedEvent;
import com.turbostool.client.event.CarLoadedEvent;
import com.turbostool.client.event.ChanneledEventDispatcher;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.LocalLapTimeEvent;
import com.turbostool.client.event.ProgressEvent2;
import com.turbostool.client.event.RouteAndCarLoadErrorEvent;
import com.turbostool.client.event.RouteAndCarLoadedEvent;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarLoader;
import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.lobbies.events.ScreenInitializedEvent;
import com.turbostool.client.model.ClientInfo;
import com.turbostool.client.model.UpgradeInfo;
import com.turbostool.client.net.SessionDataEvent;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.AddClientMessage;
import com.turbostool.client.net.messages.CarStateDataMessage;
import com.turbostool.client.net.messages.LapTimeMessage;
import com.turbostool.client.net.messages.RemoveClientMessage;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.newGraphic.NGCamera;
import com.turbostool.client.screens.BaseScreen;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.HashMap;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.utils.clearInterval;
import flash.utils.setInterval;

import mx.collections.ArrayCollection;
import mx.containers.Canvas;

public class BaseRaceController extends EventDispatcher
{
    protected var _modelsStorage: ModelsStorage;
    protected var _eventChannel: ChanneledEventDispatcher;
    protected var _raceModel: RaceModel;
    protected var _clients: ArrayCollection = new ArrayCollection();
    protected var _clientInfo: ClientInfo;
    protected var _socket: SessionSocket;
    protected var _gameEngine: GameEngine;
    //protected var _routeAndCarLoader: RouteAndCarLoader;
    private var _loadingProgress: int = 0;
    protected var _renderingCanvas: Canvas;
    protected var _screen: BaseScreen;
    private var _loadingCarStates: HashMap = new HashMap();
    protected var _oldStateEvent: CarStateDataMessage = null;
    protected var _startTime: Number = 0;

    public function str(name: String, params: * = null, n: int = -1): String {
        return Client.instance.str(name, params, n);
    }

    protected function setFocus(): void
    {
        if (_screen != null)
        {
            _screen.setFocus();
        }
    }

    public function get dataService(): ModelsStorage
    {
        return ModelsStorage(_modelsStorage);
    }

    [Bindable]
    public function get renderingCanvas():Canvas
    {
        return _renderingCanvas;
    }

    public function set renderingCanvas(val:Canvas):void
    {
        _renderingCanvas = val;
    }

    protected var _state: String;

    [Bindable]
    public function get state(): String
    {
        return _state;
    }

    public function set state(val: String): void
    {
        _state = val;
    }

    [Bindable(event="changeTimer")]
    public function get raceTime(): Number
    {
        return _startTime > 0 ? Utils.now() - _startTime : 0;
    }

    public function BaseRaceController(modelsStorage: ModelsStorage, raceModel: RaceModel, socket: SessionSocket, gameEngine: GameEngine, eventChannel: ChanneledEventDispatcher)
    {
        _modelsStorage = modelsStorage;
        _eventChannel = eventChannel;
        _raceModel = raceModel;
        _socket = socket;
        _gameEngine = gameEngine;

        _eventChannel.addEventListener(AddClientMessage.ADD_CLIENT, onAddClient);
        _eventChannel.addEventListener(RemoveClientMessage.REMOVE_CLIENT, onRemoveClient);
        EventManager.globalChannel.addEventListener(LocalLapTimeEvent.LOCAL_LAP_TIME, onLocalLapTimeEvent);

        EventManager.globalChannel.addEventListener(ProgressEvent2.PROGRESS_EVENT2, onProgress);
        _eventChannel.addEventListener(ScreenInitializedEvent.SCREEN_INITIALIZED, onScreenInitialized);
        EventManager.globalChannel.addEventListener(EngineStepEvent.ENGINE_STEP, onNeedToSendState);
        EventManager.globalChannel.addEventListener(CarStateDataMessage.CAR_STATE_MESSAGE, onOtherCarStateChange);

        EventManager.globalChannel.addEventListener(AuthorizationChangedEvent.AUTHORIZATION_CHANGED, onAuthorizationChanged);
    }

    protected function onSlowUpdateTimer(): void
    {
    }

    protected function onEachSecondUpdateTimer(): void
    {
    }

    protected function onFastUpdateTimer(): void
    {
        dispatchEvent(new Event("changeTimer"));
        dispatchEvent(new Event("changeRaceModel"));
    }

    public function get isRacing(): Boolean
    {
        return false;
    }

    protected function onCarStateTimer(): void
    {
        if (isRacing)
        {
            var carState: CarStateDataMessage = createCarStateDataMessage();
            if (carState.myVelocity.length() > 0.01)
            {
                sendCarState(carState, true);
            }
        }
    }

    protected function onScreenInitialized(event: ScreenInitializedEvent): void
    {
        renderingCanvas = event.canvas;
        _screen = event.screen;
    }

    private var _slowTimerID: uint;
    private var _eachSecondTimerID: uint;
    private var _fastTimerID: uint;
    private var _carStateTimerID: uint;

    protected function startTimers(): void
    {
        _slowTimerID = setInterval(onSlowUpdateTimer, 2000);
        _slowTimerID = setInterval(onEachSecondUpdateTimer, 1000);
        _fastTimerID = setInterval(onFastUpdateTimer, 100);
        _carStateTimerID = setInterval(onCarStateTimer, Config.instance.carStateIdleInterval);
    }

    protected function stopTimers(): void
    {
        clearInterval(_slowTimerID);
        clearInterval(_eachSecondTimerID);
        clearInterval(_fastTimerID);
        clearInterval(_carStateTimerID);
    }

    protected function onAuthorizationChanged(event: AuthorizationChangedEvent): void
    {

    }

    protected function onRouteAndCarLoadError(event: RouteAndCarLoadErrorEvent): void
    {

    }

    protected function onRouteAndCarLoaded(event: RouteAndCarLoadedEvent): void
    {

    }

    protected function setupRouteAndCar(raceWorld: RaceWorld, lapNumber: int, isReverse: Boolean, car: Car): void
    {
        raceWorld.myReverse = isReverse;
        _raceModel.lapNumber = lapNumber;
        _gameEngine.registerWorld(raceWorld);
        _gameEngine.replaceLocalCar(car);

    }

    [Bindable]
    public function set loadingProgress(value: int): void
    {
        _loadingProgress = value;
    }

    public function get loadingProgress(): int
    {
        return _loadingProgress;
    }

    private function onProgress(e: ProgressEvent2):void
    {
        loadingProgress = e.current / e.total * 100;
    }

    [Bindable(event="changeRaceModel")]
    public function get raceModel(): RaceModel
    {
        return _raceModel;
    }

    public function get clients(): ArrayCollection
    {
        return _clients;
    }

    //    [Bindable(event="changeCamera")]
    public function get camera(): NGCamera
    {
        return _gameEngine.graphicsEngine.getCamera();
    }

    protected function findClientIndexInfoByID(clientID: int): int
    {
        for (var i: int; i < _clients.length; i++)
        {
            if (ClientInfo(_clients[i]).id == clientID)
                return i;
        }
        return -1;
    }

    protected function onRemoveClient(event: ServerResponseEvent):void
    {
        var removeClientMessage: RemoveClientMessage = RemoveClientMessage(event.response);
        var clientIndex: int = findClientIndexInfoByID(removeClientMessage.clientID);
        if (clientIndex > -1)
        {
            _clients.removeItemAt(clientIndex);
        }
    }

    protected function onAddClient(e: ServerResponseEvent):void
    {        
        _clients.addItem(AddClientMessage(e.response).clientInfo);
    }

    protected function onLocalLapTimeEvent(e: LocalLapTimeEvent): void
    {
        if (e.lapTimeMessage.type == LapTimeMessage.FINISH)
        {
            EventManager.instance.carControlsEnabled = false;
            _raceModel.startEmergencyStop();
        }
    }

    protected function updateLocalOrRegisterRemoteCar(carName: String, carFileName: String, displayName:String, clientID: int, color: uint, cityID: int, level: int, useColor: Boolean = false): void
    {
        function onLocalCarLoaded(cle: CarLoadedEvent): void
        {
            if (cle.carName == carName && cle.clientID == clientID)
            {
                EventManager.globalChannel.removeEventListener(CarLoadedEvent.CAR_LOADED, onLocalCarLoaded);
                var car: Car = cle.car.clone();
                car.clientID = clientID;
                car.myNGCarView.setDisplayName(displayName, cityID, level);
                var lastCarState: CarStateDataMessage = _loadingCarStates.getValue(clientID) as CarStateDataMessage;
                _loadingCarStates.remove(clientID);
                if (lastCarState != null)
                {
                    applyCarState(car, lastCarState, false);
                }
                else
                {
                    _raceModel.getRaceWorld().getStartPositionFrame().copyTo(car.carModel.myHull.myFrame);
                    car.carModel.myCarDynamicEngine.saveState();
                }
                var upgradeInfo:UpgradeInfo;
                if (!car.isLocal)
                {
                    var carOld: Car = _raceModel.getRemoteCarByID(clientID);
                    if (carOld != null)
                    {
                        _gameEngine.removeCar(carOld);
                    }
                    _gameEngine.addCar(car);
                    upgradeInfo = (_clients[findClientIndexInfoByID(clientID)] as ClientInfo).upgradeInfo;
                }
                else
                {
                    _gameEngine.replaceLocalCar(car);
                    upgradeInfo = (_clientInfo).upgradeInfo;
                }
                car.carModel.applyUpgradeInfo(upgradeInfo);
            }
        }

        EventManager.globalChannel.addEventListener(CarLoadedEvent.CAR_LOADED, onLocalCarLoaded);
        CarLoader.instance.loadCar(carName, carFileName, clientID);
        _loadingCarStates.setValue(clientID, null);
    }

    //    private function applyCarState(car: Car, carState: CarStateDataMessage): void
    //    {
    //        var carModel: CarModel = car.carModel;
    //
    //        var localFrame:LocalFrame2d = carModel.myHull.myFrame;
    //        var newFrame:LocalFrame2d = new LocalFrame2d(carState.getR(), carState.getAngle());
    //
    //        if (localFrame.myR.difference2d_shared(newFrame.myR).length() > Config.instance.carStatePosError)
    //        {
    //            newFrame.myR.copyTo(localFrame.myR);
    //        }
    //
    //        if (Math.abs(localFrame.myAngle - newFrame.myAngle) > Config.instance.carStateAngleError * Math.PI / 180.0)
    //        {
    //            localFrame.myAngle = newFrame.myAngle;
    //        }
    //
    //        carModel.myHull.myVelocity = carState.getVelocity();
    //        carModel.myAngularVelocity = carState.getAngularVelocity();
    //        carModel.myHull.saveState();
    //    }

    private function applyCarState(car: Car, carState: CarStateDataMessage, filter: Boolean = true): void
    {
        var carModel: CarModel = car.carModel;

        var localFrame:LocalFrame2d = carModel.myHull.myFrame;
        var newFrame:LocalFrame2d = new LocalFrame2d(carState.getR(), carState.getAngle());

        if (filter)
        {
            var k: Number = Config.instance.filterCoeff;
            localFrame.myR.myX = localFrame.myR.myX * (1.0 - k) + k * newFrame.myR.myX;
            localFrame.myR.myY = localFrame.myR.myY * (1.0 - k) + k * newFrame.myR.myY;
            localFrame.myAngle = newFrame.myAngle;
        }
        else
        {
            localFrame.myR.myX = newFrame.myR.myX;
            localFrame.myR.myY = newFrame.myR.myY;
            localFrame.myAngle = newFrame.myAngle;
        }

        carModel.myHull.myVelocity = carState.getVelocity();
        carModel.myAngularVelocity = carState.getAngularVelocity();
        carModel.myHull.saveState();
    }

    private function createCarStateDataMessage(): CarStateDataMessage
    {
        var carModel: CarModel = raceModel.localCar.carModel;

        var frame:LocalFrame2d = carModel.myHull.myFrame;
        var v: Vector2d = new Vector2d(carModel.myHull.myVelocity.myX, carModel.myHull.myVelocity.myY);
        var angV: Number = carModel.myAngularVelocity;
        return new CarStateDataMessage(SessionDataEvent.MAGIC_NUMBER_SELF, frame.myR, v, frame.myAngle, angV);//, latAng, latAngVel, longAng, longAngVel, rudderAngle, rudderAction, isAccelerate, engineDroselCoef, brake, brakeCoef, handBrake);

    }

    private function onNeedToSendState(e: Event): void
    {
        if (isRacing)
        {
            var newStateEvent: CarStateDataMessage = createCarStateDataMessage();
            //trace("onNeedToSendState " +  newStateEvent.myVelocity.length());
            if (newStateEvent.myVelocity.length() > 0.01) {
                sendCarState(newStateEvent, false);
            }
        }
    }

    private var _lastCarStateSend: Date = new Date();

    private function sendCarState(newStateEvent: CarStateDataMessage, forceSend: Boolean = false): void
    {
        var currentMoment: Date = new Date();
        if (clients.length > 1)
        {
            if (_oldStateEvent == null || forceSend || !newStateEvent.equals(_oldStateEvent))
            {
                if (currentMoment.time - _lastCarStateSend.time > Config.instance.carStateMinInterval)
                {
                    _socket.sendMessage(newStateEvent);
                    _lastCarStateSend = currentMoment;
                }
            }
        }
        _oldStateEvent = newStateEvent;
    }

    private function onOtherCarStateChange(event: ServerResponseEvent): void
    {
        var carDataEvent: CarStateDataMessage = CarStateDataMessage(event.response);
        var clientID: int = carDataEvent.clientID;
        var car: Car = _raceModel.getRemoteCarByID(clientID);
        if (car != null)
        {
            if (car.firstCarState)
            {
                applyCarState(car, carDataEvent, false);
                car.firstCarState = false;
            }
            else
            {
                applyCarState(car, carDataEvent, true);
            }
        }
        else
        {
            _loadingCarStates.setValue(clientID, carDataEvent);
        }
    }

    public function onStartGameplay(): void
    {
        _gameEngine.resetGame();
        removeAllRemoteCars();
        raceModel.localCar.currentLap = -1;

        for each(var client: ClientInfo in _clients)
        {
            //if (client.id != Car.LOCAL_CAR_ID)
            // warning! do not uncomment condition!
            // we should update All cars, because we can apply upgrade only for clear car
            // so we add remote cars and replace local car
            // before we apply upgrades
            var carName:String;
            var cityID: int;
            var level: int;
            if (client.id != Car.LOCAL_CAR_ID)
            {
                carName = client.racingCarName;
                cityID = client.homeCity;
                level = client.level;
            }
            else
            {
                carName = Client.instance.modelsStorage.userInfo.racingCarName;
                cityID = Client.instance.modelsStorage.userInfo.homeCity;
                level = Client.instance.modelsStorage.userInfo.level;
            }
            updateLocalOrRegisterRemoteCar(carName, client.carFileName, client.displayName, client.id, 0, cityID, level);
        }
        _gameEngine.start();
    }

    public function onStopGameplay(): void
    {
        _gameEngine.stop();
    }

    public function removeAllRemoteCars(): void
    {
        var carsCount: int = _raceModel.getCars().length();
        for (var i: int = carsCount - 1; i--; i >= 0)
        {
            var car: Car = Car(_raceModel.getCars().getItemAt(i));
            if (!car.isLocal)
            {
                _gameEngine.removeCar(car);
            }
        }
    }

    public function get eventChannel():ChanneledEventDispatcher
    {
        return _eventChannel;
    }

    [Bindable(event="changeTimer")]
    public function isTipVisible(type: String):Boolean
    {
        return false;
    }
}
}
