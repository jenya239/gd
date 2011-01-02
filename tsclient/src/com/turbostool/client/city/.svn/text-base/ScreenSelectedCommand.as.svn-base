package com.turbostool.client.city
{
import com.turbostool.client.screens.BaseScreen;

import flash.events.Event;
import flash.events.Event;

public class ScreenSelectedCommand extends Event
{
    public static const SCREEN_SELECTED: String = "screenSelected";

    private var _screenName: String;

    public function ScreenSelectedCommand(screenName: String)
    {
        super(SCREEN_SELECTED);
        
        this._screenName = screenName;
    }

    public function get screenName(): String
    {
        return _screenName;
    }

    override public function clone(): Event
    {
        return new ScreenSelectedCommand(_screenName);        
    }
}
}