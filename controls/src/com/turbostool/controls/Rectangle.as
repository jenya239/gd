package com.turbostool.controls
{
import flash.display.CapsStyle;
import flash.display.JointStyle;
import flash.display.LineScaleMode;

import mx.core.UIComponent;

[Style(name="borderColor2", type="uint", format="Color", inherit="no")]
[Style(name="borderAlpha", type="Number", inherit="no")]
[Style(name="borderWidth", type="uint", inherit="no")]
[Style(name="backgroundColor2", type="uint", format="Color", inherit="no")]
[Style(name="cornerRadius", type="uint", inherit="no")]
[Style(name="backgroundAlpha", type="Number", inherit="no")]

public class Rectangle extends UIComponent
{
    public function Rectangle()
    {
        super();
    }

    override protected function updateDisplayList(unscaledWidth:Number, unscaledHeight:Number):void
    {
        super.updateDisplayList(unscaledWidth, unscaledHeight);
        
        var backgroundColor: uint = getStyle("backgroundColor2");
        var borderColor: uint = getStyle("borderColor2");
        var borderAlpha: Number = getStyle("borderAlpha");
        var borderWidth: uint = getStyle("borderWidth");
        var cornerRadius: uint = getStyle("cornerRadius");
        var backgroundAlpha: Number = getStyle("backgroundAlpha");

        graphics.clear();

        graphics.beginFill(backgroundColor, backgroundAlpha*alpha);
        
        // Draw a simple border around the child components.
        if(borderWidth > 0) {
            graphics.lineStyle(borderWidth, borderColor, borderAlpha*alpha, true, LineScaleMode.NONE, CapsStyle.NONE, JointStyle.ROUND, 1.414);
        }
        
        graphics.drawRoundRect(borderWidth/2, borderWidth/2, unscaledWidth - borderWidth, unscaledHeight - borderWidth, cornerRadius, cornerRadius);

        graphics.endFill();
    }
}
}