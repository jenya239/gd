package com.turbostool.client.event
{
import com.turbostool.client.controllers.BaseStateMachine;

import flash.events.Event;

public class StateChangedEvent extends Event
{
    public static const STATE_CHANGED_EVENT: String = "stateChangedEvent";

    private var _sm: BaseStateMachine;
    private var _state: String;
    private var _oldState: String;

    public function StateChangedEvent(sm: BaseStateMachine, state: String, oldState: String)
    {
        super(STATE_CHANGED_EVENT);
        _sm = sm;
        _state = state;
        _oldState = oldState;
    }

    public function get sm(): BaseStateMachine
    {
        return _sm;
    }

    public function get state(): String
    {
        return _state;
    }

    public function get oldState(): String
    {
        return _oldState;
    }

    public static function check(event: Event, targetStateMachine: BaseStateMachine, targetState: String): Boolean
    {
        if (event is StateChangedEvent)
        {
            var stateEvent: StateChangedEvent = StateChangedEvent(event);
            return stateEvent._sm == targetStateMachine && stateEvent._state == targetState;
        }
        else
        {
            return false;
        }
    }
}
}