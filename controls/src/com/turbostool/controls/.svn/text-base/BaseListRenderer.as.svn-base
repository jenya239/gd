package com.turbostool.controls
{

import mx.containers.Canvas;
import mx.controls.listClasses.ListBase;

public class BaseListRenderer extends Canvas
{
    private var _backgroundAlpha: Number = 1;
    private var _backgroundColor: Number = 0xDE8A05;
    private var _selectedBackgroundColor: Number = 0xFEAA15;

    public function BaseListRenderer()
    {        
        setStyle("backgroundColor", _backgroundColor);
        setStyle("backgroundAlpha", _backgroundAlpha);
    }

    override protected function updateDisplayList(unscaledWidth: Number, unscaledHeight: Number):void
    {
        super.updateDisplayList(unscaledWidth, unscaledHeight);

        if (ListBase(owner).isItemSelected(data))
        {
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
        setStyle("backgroundAlpha", _backgroundAlpha);
    }

    public function get backgroundColor():Number
    {
        return _backgroundColor;
    }

    public function set backgroundColor(value:Number):void
    {
        _backgroundColor = value;
    }

	public function get selectedBackgroundColor():Number {
		return _selectedBackgroundColor;
	}

	public function set selectedBackgroundColor(value:Number):void {
		_selectedBackgroundColor = value;
	}
}
}