package com.turbostool.client.registration
{
import com.turbostool.client.screens.BaseScreen;

import flash.events.Event;

public class NextScreenCommand extends Event
{
    public static const NEXT_SCREEN: String = "nextScreen";

    private var _screen: BaseScreen;

    public function NextScreenCommand(screen: BaseScreen)
    {
        super(NEXT_SCREEN);
        this._screen = screen;
    }

    public function get screen():BaseScreen
    {
        return _screen;
    }
}
}