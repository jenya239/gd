package com.turbostool.client.screens
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.BaseRaceController;
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.view.GameplayView;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.utils.collections.HashMap;

import flash.events.Event;
import flash.utils.Timer;

import mx.controls.Image;
import mx.events.FlexEvent;
import mx.logging.Log;

public class RacingScreenBase extends BaseScreen
{
    public var loadingCars: HashMap = new HashMap();

    //private var _gameEngine: GameEngine;
    protected var _raceWorld: RaceWorld = null;
    protected var _car: Car;
    protected var _app: Client;
    protected var _manager: EventManager = EventManager.instance;
    protected var _socket: SessionSocket;
    protected var _timer: Timer;
    //private var _netTimer: Timer;

    public var gameView: GameplayView;
    public var iBackground: Image;
    //public var cPreloaderScreen: LoaderScreen;
    private var _listenersAdded: Boolean = false;

    protected var _gameplayInitialized: Boolean = false;


    [Bindable]
    public var raceController: BaseRaceController;

    public function RacingScreenBase()
    {
        _logger = Log.getLogger(className);
        _app = Client.instance;
        _socket = SessionSocket.instance;
    }

    public override function onInitialize(event: FlexEvent): void
    {
        Client.instance.addEventListener(Event.ACTIVATE, onActivate);

        setFocus();

    }

    private function onActivate(event:Event):void
    {
        setFocus();
    }

    public override function setFocus(): void
    {
        if (gameView != null && gameView.cnvRender != null)
        {
            gameView.cnvRender.setFocus();
        }
    }
}
}