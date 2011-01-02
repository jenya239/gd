package com.turbostool.controls
{
import flash.events.Event;

public class SmartImageLoadedEvent extends Event
{
    public static const SMART_IMAGE_LOADED: String = "smartImageLoaded";

    public function SmartImageLoadedEvent()
    {
        super(SMART_IMAGE_LOADED);
    }
}
}