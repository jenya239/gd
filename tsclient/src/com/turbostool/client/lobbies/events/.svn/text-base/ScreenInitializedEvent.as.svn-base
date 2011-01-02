package com.turbostool.client.lobbies.events
{
import com.turbostool.client.screens.BaseScreen;

import flash.events.Event;

import mx.containers.Canvas;

public class ScreenInitializedEvent extends Event
{
    public static const SCREEN_INITIALIZED:String = "screenInitialized";
    public var canvas: Canvas;
    public var screen: BaseScreen;

    public function ScreenInitializedEvent(canvas: Canvas, screen: BaseScreen)
    {
        super(SCREEN_INITIALIZED);
        this.canvas = canvas;
        this.screen = screen;
    }

}
}