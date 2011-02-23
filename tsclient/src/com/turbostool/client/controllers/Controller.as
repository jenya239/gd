package com.turbostool.client.controllers
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.event.AuthorizationChangedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.QuickConnectButtonClick;
import com.turbostool.client.event.StateChangedEvent;
import com.turbostool.client.game.GameEngine;
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.utils.Utils;

import flash.events.Event;

import mx.logging.Log;
import mx.logging.LogEventLevel;
import mx.logging.targets.TraceTarget;

public class Controller extends BaseStateMachine
{
    //состояния
    public static const INIT: String = "initialization";
    public static const QUICK_CONNECT: String = "quickConnect";
    public static const AUTH: String = "authorization";
    public static const DISCONNECTED: String = "disconnected";
    public static const CONNECTED: String = "connected";

    // логгер			
    private var _logTarget: TraceTarget;
    private const _defaultLogLevel: int = LogEventLevel.DEBUG;

    // ссылки
    private var _app: Client;

    // вложенные контроллеры (автоматы)
    public var initController: InitController;
    [Bindable]
    public var loginController: LoginController;
    public var connectController: ConnectController;
    [Bindable]
    public var racingSM: RacingStateMachine;

    private var _raceModel: RaceModel;
    private var _gameEngine: GameEngine;
    private var _modelsStorage: ModelsStorage;

		private var _auth_at: Number;

    public function Controller(raceModel: RaceModel, gameEngine: GameEngine, modelsStorage: ModelsStorage)
    {
        _raceModel = raceModel;
        _gameEngine = gameEngine;
        _modelsStorage = modelsStorage;
        _app = Client.instance;
        initLogger();
        _logger = Log.getLogger(Utils.getClassName(this));
        _state = INIT;

        start();
    }

    public function set logLevel(level: int): void
    {
        _logTarget.level = level;
    }

    // инициализация логирования
    public function initLogger(): void
    {
        _logTarget = new TraceTarget();
        _logTarget.level = _defaultLogLevel;
        _logTarget.includeDate = true;
        _logTarget.includeTime = true;
        _logTarget.includeCategory = true;
        _logTarget.includeLevel = true;
        Log.addTarget(_logTarget);
    }

    // запуск контроллера и вложенных контроллеров
    private function start(): void
    {
        _logger.debug("Starting controllers..");

        initController = new InitController();
        loginController = new LoginController(_modelsStorage);
        racingSM = new RacingStateMachine(_raceModel, _gameEngine);
        connectController = new ConnectController(racingSM, _modelsStorage);

        EventManager.instance.addEventListener(StateChangedEvent.STATE_CHANGED_EVENT, processEvent);
        EventManager.instance.addEventListener(QuickConnectButtonClick.QUICKCONNECT_BUTTON_CLICK, processEvent);

        _logger.debug("Started controllers..");
    }

    override protected function onEnterNewState(state: String):void
    {
        super.onEnterNewState(state);
        if (state == QUICK_CONNECT)
        {
            EventManager.globalChannel.dispatchEvent(new AuthorizationChangedEvent(false, true));
        }else if (state == AUTH)
        {
            _auth_at = (new Date()).time;
        }
    }

    protected override function processEvent(event: Event): void
    {
        super.processEvent(event);

        switch (_state)
                {
            case INIT:
                if (StateChangedEvent.check(event, initController, InitController.INITIALIZED))
                {
                    changeState(AUTH);
                }
                break;

            case AUTH:
                if (StateChangedEvent.check(event, loginController, LoginController.LOGGED_IN))
                {
                    changeState(DISCONNECTED);
                }
                break;

            case DISCONNECTED:
                if (StateChangedEvent.check(event, connectController, ConnectController.CONNECTED))
                {
                    changeState(CONNECTED);
                }
                else
                    if (StateChangedEvent.check(event, loginController, LoginController.LOGGED_OUT))
                    {
                        changeState(AUTH);
                    }
                break;

            case CONNECTED:
                if (StateChangedEvent.check(event, racingSM, RacingStateMachine.DISCONNECTED))
                {
                    changeState(DISCONNECTED);
                }
                else
                    if (StateChangedEvent.check(event, loginController, LoginController.LOGGED_OUT))
                    {
                        changeState(AUTH);
                    }
                break;
        }
    }

	public function get auth_at():Number {
		return _auth_at;
	}
}

}