package com.turbostool.client.event
{
import com.turbostool.client.Config;

import flash.events.Event;

public class ConfigLoadedEvent extends Event
{
    public static const CONFIG_LOADED: String = "configLoaded";

    private var _config: Config;

    public function ConfigLoadedEvent(config: Config)
    {
        super(CONFIG_LOADED);
        _config = config;
    }

    public function get config(): Config
    {
        return _config;
    }
}
}