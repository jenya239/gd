package com.turbostool.controls
{
import mx.containers.Canvas;
import mx.core.IToolTip;

public class CustomToolTip extends Canvas implements IToolTip
{
    public function CustomToolTip()
    {
        mouseEnabled = false;
        mouseChildren = false;
    }

    public function get text():String
    {
        return null;
    }

    public function set text(value:String):void
    {
    }
}
}