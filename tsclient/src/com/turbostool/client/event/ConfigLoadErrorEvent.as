package com.turbostool.client.event
{
import flash.events.Event;

public class ConfigLoadErrorEvent extends Event
{
    public static const CONFIG_LOAD_ERROR: String = "configLoadError";

    private var _error: Error;

    public function ConfigLoadErrorEvent(error: Error)
    {
        super(CONFIG_LOAD_ERROR);
        _error = error;
    }

    public function get error(): Error
    {
        return _error
    }
}
}