package com.turbostool.client.game
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.components.car.CarControlEvent;
import com.turbostool.client.game.components.car.RudderControl;
import com.turbostool.client.utils.Keyboard;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.collections.ArrayList;

import flash.events.*;
import flash.ui.Keyboard;

public class CarControlEventGenerator extends EventDispatcher
{
    private static var _instance: CarControlEventGenerator;
    private var _keyCodes:ArrayList = new ArrayList();
    private var _mouseIsDown: Boolean = false;
    private var _mouseDelta: Number = 0;
    private var _mouseDownX: int;
    private var _mouseMoveRange: int;

    public static function get instance(): CarControlEventGenerator
    {
        if (_instance == null) _instance = new CarControlEventGenerator();
        return _instance;
    }

    public function CarControlEventGenerator()
    {
        if (_instance != null)
        {
            throw new TSError("Singleton");
        }
    }

    public function get MouseMoveRange() : int
    {
        return _mouseMoveRange;
    }

    public function set MouseMoveRange(value: int) : void
    {
        _mouseMoveRange = value;
    }

    private function addKeyCode(code:int):Boolean
    {
        if (!_keyCodes.contains(code))
            return _keyCodes.addItem(code);
        else
            return false;
    }

    private function removeKeyCode(code:int):void
    {
        _keyCodes.removeItem(code);
    }

    public function checkEvents(): CarControlEvent
    {
        var controlEvent:CarControlEvent = new CarControlEvent(false, RudderControl.NONE, false, false, 0, CarControlEvent.GEAR_NONE);

        if (_keyCodes.contains(flash.ui.Keyboard.DOWN))
        {
            controlEvent.myBrake = true;
        }

        if (_keyCodes.contains(flash.ui.Keyboard.UP))
        {
            controlEvent.myAccelerate = true;
        }

        if (_keyCodes.contains(flash.ui.Keyboard.SPACE))
        {
            controlEvent.myHandBrake = true;
        }

        var key: int;
        key = com.turbostool.client.utils.Keyboard.Z;
        if (_keyCodes.contains(key))
        {
            controlEvent.myGearBoxAction = CarControlEvent.GEAR_DOWN;
            _keyCodes.removeItem(key);
        }

        key = com.turbostool.client.utils.Keyboard.X;
        if (_keyCodes.contains(key))
        {
            controlEvent.myGearBoxAction = CarControlEvent.GEAR_UP;
            _keyCodes.removeItem(key);
        }

        if (_keyCodes.contains(flash.ui.Keyboard.LEFT)) {
            controlEvent.myRudderControl = RudderControl.LEFT;
        }

        if (_keyCodes.contains(flash.ui.Keyboard.RIGHT)) {
            controlEvent.myRudderControl = RudderControl.RIGHT;
        }

        key = com.turbostool.client.utils.Keyboard.N;
        if (_keyCodes.contains(key))
        {
            trace("nitro!");
            controlEvent.myNitro = true;
            EventManager.instance.dispatchEvent(new Event("nitroUsed"));
            _keyCodes.removeItem(key);
        }


        return controlEvent;
    }

    public function onAction(event: Event) : void
    {
        switch (event.type)
                {
            case "keyDown":
                addKeyCode(KeyboardEvent(event).keyCode);
                break;
            case "keyUp":
                removeKeyCode(KeyboardEvent(event).keyCode);
                break;
            default:
                throw new TSError("Unknown action passed to GameController.onAction");
                break;
        }
    }

    public function clearKeyCodes(): void
    {
        _keyCodes.clear();
    }
}
}