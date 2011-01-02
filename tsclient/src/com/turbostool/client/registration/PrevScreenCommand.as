package com.turbostool.client.registration
{
import com.turbostool.client.screens.BaseScreen;

import flash.events.Event;
import flash.events.Event;

public class PrevScreenCommand extends Event
{
    public static const PREV_SCREEN: String = "prevScreen";

    private var _screen: BaseScreen;

    public function PrevScreenCommand(screen: BaseScreen)
    {
        super(PREV_SCREEN);
        this._screen = screen;
    }

    public function get screen():BaseScreen
    {
        return _screen;
    }

    override public function clone():Event
    {
        return new PrevScreenCommand(_screen);
    }
}
}