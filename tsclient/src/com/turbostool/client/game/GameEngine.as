package com.turbostool.client.game
{
import com.turbostool.client.Config;
import com.turbostool.client.dynamicEngine.CameraLink;
import com.turbostool.client.dynamicEngine.DynamicEngine;
import com.turbostool.client.dynamicEngine.ICameraLinkable;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.CarControlEvent;
import com.turbostool.client.game.components.car.CarDynamicEngine;
import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.newGraphic.NGCamera;
import com.turbostool.client.newGraphic.NGCarView;
import com.turbostool.client.newGraphic.NGDrawable;
import com.turbostool.client.newGraphic.NGGraphicEngine;
import com.turbostool.client.utils.Keyboard;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.Iterator;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.KeyboardEvent;

import mx.binding.utils.BindingUtils;
import mx.binding.utils.ChangeWatcher;
import mx.containers.Canvas;
import mx.controls.Alert;
import mx.controls.Label;
import mx.core.Application;

public class GameEngine extends EventDispatcher
{
    private static const GHOST_SCALE: Number = 0.25;

    //3 состояния движка
    public static const STATE_GAME: int = 0;
    public static const STATE_RECORD: int = 1;
    public static const STATE_REPLAY: int = 2;

    private var _state: int = STATE_GAME;

    private var _dynamicEngine: DynamicEngine;
    private var _carControlEventGenerator: CarControlEventGenerator = new CarControlEventGenerator();
    //private var _timer: Timer;
    private var _lastTimerMillis: Number = 0;
    private var _cameraLink: CameraLink;
    //private const MAX_ACCEPTABLE_DT: Number = 100;

    private var _graphicsEngine: NGGraphicEngine;
    //private var _gameView: GameplayComponents;
    //private var _area: Canvas;

    private var _replay: ReplayData = new ReplayData();
    private var _previousEvent: CarControlEvent;

    private var _replayGhost: NGDrawable;

    //public var timeCoef: Number = 1;
    private var _replayTime: Number = 0;
    //private var _inReplaySubstepIndex: int = 0;
    //private var _inReplaySubstepCount: int = 10;
    private var _currentReplayRecord: ReplayDataRecord;

    private var _isRunning: Boolean = false;

    private var _lastQuantTimeLocal: Number;
    private var _lastQuantTimeRemote: Number;

    private var _counter: Number = 0;
    private const SEND_EVENT_PER_TICKS: Number = 1;

    public function GameEngine(application: Application)
    {
        _dynamicEngine = new DynamicEngine();
        _graphicsEngine = new NGGraphicEngine(new NGCamera(new LocalFrame2d(new Vector2d(0, 0), 0), 20, 600));
        _cameraLink = new CameraLink();
        //_timer = new Timer(20);
        //_timer.addEventListener(TimerEvent.TIMER, onTimer);
        _isRunning = false;
        application.addEventListener(Event.ENTER_FRAME, onFrame);
    }

    public function set canvas(value: Canvas): void
    {
        setCanvas(value);
    }

    private function setCanvas(value: Canvas): void
    {
        _graphicsEngine.canvas = value;
    }

    private var _currentCanvasChangeWatcher: ChangeWatcher = null;

    public function bindCanvas(host: Object, propName: String): void
    {
        if (_currentCanvasChangeWatcher != null)
        {
            _currentCanvasChangeWatcher.unwatch();
        }

        _currentCanvasChangeWatcher = BindingUtils.bindSetter(setCanvas, host, propName);
    }


    protected function onFrame(e: Event):void
    {
        if (!_isRunning)
        {
            return;
        }

        if (_dynamicEngine.collFreq > 0.5)
        {
            backToRoad();
        }

        var controlEvent: CarControlEvent;
        var dt: Number;
        var now: Number = Utils.now();
        var addReplayRecord: Boolean = false;

        switch (_state)
                {
            case STATE_GAME:
                controlEvent = _carControlEventGenerator.checkEvents();
                dt = getDT(now);
                break;
            case STATE_RECORD:
                controlEvent = _carControlEventGenerator.checkEvents();
                dt = getDT(now);
                if (!_replay.isFull())
                {
                    addReplayRecord = true;
                    // we shoudl actually call ReplayData.add(..) later after we have uptodate car postition
                }
                else
                {
                    dispatchEvent(new Event("replay_full"));
                    this.state = STATE_GAME;
                }
                break;
            case STATE_REPLAY:
                //if( _inReplaySubstepIndex == 0 ){
                if (_replay.hasNext())
                {
                    if (!_replay.canPlay())
                    {
                        return;
                    }
                    _currentReplayRecord = _replay.getNext();
                    controlEvent = _currentReplayRecord.event;
                    dt = _currentReplayRecord.dt;
                    dispatchEvent(new Event("replayIndex_changed"));
                }
                else
                {
                    this.state = STATE_GAME;
                    _lastTimerMillis = now;
                    return;
                }
                //}
                //_inReplaySubstepIndex = (_inReplaySubstepIndex + 1) % _inReplaySubstepCount;
                //dt = _currentReplayRecord.dt / _inReplaySubstepCount;
                _replayTime += dt;
                break;
        }

        // dispatch event if needed
        if (controlEvent != null && !controlEvent.equals(_previousEvent))
        {
            //if( _state == STATE_REPLAY ){
            //	if( _inReplaySubstepIndex == 0 ){
            //		_controller.dispatchEvent(controlEvent);
            //	}
            //}else{
            EventManager.instance.dispatchEvent(controlEvent);
            _carControlEventGenerator.dispatchEvent(controlEvent);
            //}
        }

        // remember event
        _previousEvent = controlEvent;

        _lastQuantTimeLocal = _dynamicEngine.step(now, _lastQuantTimeLocal, Config.instance.dynamicEngineQuantLocal, true);
        _lastQuantTimeRemote = _dynamicEngine.step(now, _lastQuantTimeRemote, Config.instance.dynamicEngineQuantRemote, false);

        for(var i:int=0; i<_dynamicEngine.cars.length(); i++)
        {
            var car: Car = Car(_dynamicEngine.cars.getItemAt(i));
            if (!car.isLocal) car.carModel.myGeomFrame;
        }

        _cameraLink.step(dt);

        if (_counter % SEND_EVENT_PER_TICKS == 0)
        {
            EventManager.instance.dispatchEvent(new EngineStepEvent());
        }
        _counter++;

        _lastTimerMillis = now;

        if (_currentReplayRecord != null && _currentReplayRecord.posInfo != null)
        {
            _replayGhost.myFrame.myR = new Vector2d(_currentReplayRecord.posInfo.pos.myX, _currentReplayRecord.posInfo.pos.myY);
            _replayGhost.myFrame.myAngle = _currentReplayRecord.posInfo.rotation;
        }

        if (addReplayRecord)
        {
            _replay.add(dt, controlEvent, getReplayPosInfo());
        }

        _graphicsEngine.draw();

        if (raceModel != null)
        {
            raceModel.checkEvents();
        }
    }

    private function getReplayPosInfo(): ReplayPositionInfo
    {
        var car: Car = raceModel.localCar;
        var frame: LocalFrame2d = car.carModel.getHull().myGeomFrame;

        return new ReplayPositionInfo(frame.myR.clone() as Vector2d, frame.myAngle);
    }

    private function getDT(now: Number): Number
    {
        var dt: Number = (now - _lastTimerMillis) / 1000;
        //if (dt > MAX_ACCEPTABLE_DT) {
        //	dt = MAX_ACCEPTABLE_DT;
        //}
        return dt;
    }

    public function get graphicsEngine(): NGGraphicEngine
    {
        return _graphicsEngine;
    }

    public function get dynamicEngine(): DynamicEngine
    {
        return _dynamicEngine;
    }

    private function registerComponent(component:IGameComponent):void
    {
        _graphicsEngine.addDrawable(component.getNGDrawable());
    }

    private function removeComponent(component:IGameComponent):void
    {
        _graphicsEngine.removeDrawable(component.getNGDrawable());
    }

    public function registerWorld(raceWorld: RaceWorld):void
    {
        removeWorld();
        raceModel.setRaceWorld(raceWorld);
        _cameraLink.setPolygon(raceWorld.getCameraPath());
        var it:Iterator = raceWorld.getComponents().iterator();
        while (it.hasNext())
        {
            registerComponent(it.next() as IGameComponent);
        }
        it = raceWorld.getNGDrawables().iterator();
        while (it.hasNext())
        {
            _graphicsEngine.addDrawable(it.next() as NGDrawable);
        }
        if (raceModel.localCar != null)
        {
            raceWorld.getStartPositionFrame().copyTo(raceModel.localCar.carModel.myHull.myFrame);
            raceModel.localCar.carModel.myCarDynamicEngine.saveState();
        }
    }

    private function removeWorld(): void
    {
        var raceWorld: RaceWorld = raceModel.getRaceWorld();
        if (raceWorld == null) return;
        var it:Iterator = raceWorld.getComponents().iterator();
        while (it.hasNext())
        {
            removeComponent(it.next() as IGameComponent);
        }
        it = raceWorld.getNGDrawables().iterator();
        while (it.hasNext())
        {
            _graphicsEngine.removeDrawable(it.next() as NGDrawable);
        }
    }

    public function addCar(car: Car): void
    {
			trace("addCar");
			_graphicsEngine.addDrawable(car.myNGCarView);
			raceModel.getCars().addItem(car);
			_graphicsEngine.overlay.addChild(car.myNGCarView.lblUserName);
    }

    public function removeCar(car: Car): void
    {
			trace("removeCar");
			_graphicsEngine.removeDrawable(car.myNGCarView);
			raceModel.getCars().removeItem(car);
			_graphicsEngine.removeDrawable(car.myNGCarView.myShadow);
			_graphicsEngine.overlay.removeChild(car.myNGCarView.lblUserName);
    }

    public function replaceLocalCar(car: Car): void
    {
        trace("replaceLocalCar");
        var oldCar: Car = raceModel.localCar;
        if (oldCar != null)
        {
            removeCar(oldCar);
            _carControlEventGenerator.removeEventListener(CarControlEvent.CAR_CONTROL, oldCar.getController().onControl);
        }
        raceModel.getRaceWorld().getStartPositionFrame().copyTo(car.carModel.myHull.myFrame);
        car.carModel.myCarDynamicEngine.saveState();
        addCar(car);
        cameraLinkTo(car.carModel.myHull);
        _carControlEventGenerator.addEventListener(CarControlEvent.CAR_CONTROL, car.getController().onControl);
        raceModel.dispatchEvent(new Event("changeLocalCar"));
    }

    public function start():void
    {
        var now: Number = Utils.now();
        _lastQuantTimeLocal = now;
        _lastQuantTimeRemote = now;
        _lastTimerMillis = now;
        _isRunning = true;
    }

    public function stop():void
    {
        _isRunning = false;
    }

    public function cameraLinkTo(target:ICameraLinkable):void
    {
        _cameraLink.myTarget = target;
    }

    public function get replay(): ReplayData
    {
        return _replay;
    }

    public function get state(): int
    {
        return _state;
    }

    public function set state(value: int): void
    {
        var oldState: int = _state;
        _state = value;
        if (oldState != _state)
        {
            //todo refactor
            dispatchEvent(new Event("state_changed"));
            //_dynamicEngine.mySubstepCount = _inReplaySubstepCount;
            if (_state == STATE_REPLAY)
            {
                if (_replayGhost == null)
                {
                    var w: Number = raceModel.localCar.carModel.myCarDynamicEngine.myHull.myWidth;
                    var l: Number = raceModel.localCar.carModel.myCarDynamicEngine.myHull.myLength;

                    var model: CarModel = new CarModel(w * GHOST_SCALE, l * GHOST_SCALE);
                    var cv: NGCarView = new NGCarView(model);
                    cv.myDebugMode = false;
                    _replayGhost = cv;
                    cv.myColor = 0xff0000;
                }
                _graphicsEngine.addDrawable(_replayGhost);
                _replayTime = 0;
                //_inReplaySubstepIndex = 0;
                _currentReplayRecord = null;
                //_dynamicEngine.mySubstepCount = 1;
            }
            if (oldState == STATE_REPLAY)
            {
                var carView: NGCarView = _replayGhost as NGCarView;
                if (carView != null)
                {
                    _graphicsEngine.removeDrawable(carView.myShadow);
                }
                _graphicsEngine.removeDrawable(_replayGhost);
            }
        }
    }

    public function get replayTime(): Number
    {
        return _replayTime;
    }

    public function onKeyDown(event:KeyboardEvent): void
    {
        //Alert.show(event.toString());
        switch (event.keyCode)
                {
//            case Keyboard.R:
//                if (event.ctrlKey)
//                {
//                    resetGame();
//                }
//                break;
//            case Keyboard.T:
//                if (event.ctrlKey)
//                {
//                    backToRoad();
//                }
//                break;
            case Keyboard.Z:
                if (event.ctrlKey)
                {
                    _carControlEventGenerator.onAction(event);
                }
            case Keyboard.X:
                if (event.ctrlKey)
                {
                    _carControlEventGenerator.onAction(event);
                }
            default:
                _carControlEventGenerator.onAction(event);
        }
    }

    public function resetGame(): void
    {
        raceModel.resetOneCar(raceModel.localCar);
        _carControlEventGenerator.clearKeyCodes();
        _previousEvent = null;
    }

    public function onKeyUp(event:KeyboardEvent):void
    {
        _carControlEventGenerator.onAction(event);

    }

    public function get raceModel(): RaceModel
    {
        return _dynamicEngine.raceModel;
    }

    private static const VELOCITY_0: Number = 10;

    public function backToRoad(): void
    {
        var piece: Piece2d = _cameraLink.findNearestCameraPathPiece();
        var carModel:CarModel = raceModel.localCar.carModel;
        //нужно брать не просто проекцию, а ближайшую к отрезку точку
        carModel.myFrame.myR = piece.getNearest(raceModel.localCar.myR);
        carModel.myFrame.myAngle = piece.getPieceVector().getAngle() - Math.PI / 2;
        carModel.fullStop();
        carModel.myVelocity = carModel.myOrientation.numberProduct2d(VELOCITY_0);
        carModel.myHull.saveState();
    }
}
}
