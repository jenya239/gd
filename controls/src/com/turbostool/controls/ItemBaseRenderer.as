package com.turbostool.controls
{

import mx.containers.Canvas;
import mx.controls.listClasses.ListBase;

public class ItemBaseRenderer extends Canvas
{
    private var _backgroundAlpha: Number = 0;
    private var _backgroundColor: Number = 0x0;
    private var _selectedBackgroundColor: Number = 0x0;

    public function ItemBaseRenderer()
    {
        setStyle("backgroundAlpha", _backgroundAlpha);
        setStyle("backgroundColor", _backgroundColor);
    }

    override protected function updateDisplayList(unscaledWidth: Number, unscaledHeight: Number):void
    {
        super.updateDisplayList(unscaledWidth, unscaledHeight);

        var listBase: ListBase = owner as ListBase;

        if(listBase != null && listBase.isItemSelected(data))
        {
            setStyle("backgroundAlpha", 0);
            setStyle('backgroundColor', _selectedBackgroundColor);
        }
        else
        {
            setStyle('backgroundColor', _backgroundColor);
        }
    }

    public function get backgroundAlpha():Number
    {
        return _backgroundAlpha;
    }

    public function set backgroundAlpha(value:Number):void
    {
        _backgroundAlpha = value;
    }

    public function get backgroundColor():Number
    {
        return _backgroundColor;
    }

    public function set backgroundColor(value:Number):void
    {
        _backgroundColor = value;
    }
}
}