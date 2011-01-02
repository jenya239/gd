package com.turbostool.client.controllers
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.StateChangedEvent;
import com.turbostool.client.utils.Assert;

import flash.events.Event;
import flash.events.EventDispatcher;

import mx.logging.ILogger;

public class BaseStateMachine extends EventDispatcher
{
    // логгер
    protected var _logger: ILogger;

    // последняя ошибка
    protected var _error: Error;

    // строковой id текущего состояния
    protected var _state: String = "";

    public function BaseStateMachine()
    {
        //_logger = Log.getLogger(Utils.getClassName(this));
    }

    [Bindable(event="stateChangedEvent")]
    public function get state(): String
    {
        return _state;
    }

    // изменяет состояние, вызывает входные/выходные действия и генерирует событие StateChangedEvent
    protected function changeState(newState: String): void
    {
        // можно вызывать только с новым значением
        _logger.debug("old state = " + _state + ", newe State = " + newState );
        Assert.assertNotEquals(_state, newState);

        var oldState: String = _state;
        onExitOldState(_state);

        _logger.debug("state changed: " + oldState + " -> " + newState);

        _state = newState;

        EventManager.instance.dispatchEvent(new StateChangedEvent(this, newState, oldState));
        dispatchEvent(new StateChangedEvent(this, newState, oldState));
        onEnterNewState(newState);
    }

    protected function processEvent(event: Event): void
    {
        if (event.type != StateChangedEvent.STATE_CHANGED_EVENT)
        {
            _logger.debug("event: " + event.type);
        }
    }

    protected function onExitOldState(state: String): void
    {

    }

    protected function onEnterNewState(state: String): void
    {

    }

    public function str(name: String, params: * = null, n: int = -1): String {
        return Client.instance.str(name, params, n);
    }

}
}