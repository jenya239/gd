package com.turbostool.client.screens
{
import mx.core.UITextField;
import flash.display.DisplayObject;
import flash.text.TextLineMetrics;

import mx.controls.Button;
import mx.core.IFlexDisplayObject;
import mx.core.mx_internal;
use namespace mx_internal;

public class MultiLineButton extends Button
{
	
public function MultiLineButton()
{
super();
}

override protected function createChildren():void
{
if (!textField)
{
textField = new UITextField();
textField.styleName = this;
addChild(DisplayObject(textField));
}

super.createChildren();

textField.multiline = true;
textField.wordWrap = true;
textField.width = width;
(textField as UITextField).setStyle("textAlign","center");
}

override protected function measure():void
{
if (!isNaN(explicitWidth))
{
var tempIcon:IFlexDisplayObject = getCurrentIcon();
var w:Number = explicitWidth;
if (tempIcon)
w -= tempIcon.width + getStyle("horizontalGap") + getStyle("paddingLeft") + getStyle("paddingRight");
textField.width = w;
}
super.measure();

}

override public function measureText(s:String):TextLineMetrics
{
textField.text = s;
var lineMetrics:TextLineMetrics = textField.getLineMetrics(0);
lineMetrics.width = textField.textWidth + 4;
lineMetrics.height = textField.textHeight + 4;
return lineMetrics;
}
}
}
