package com.turbostool.controls
{
import flash.display.Sprite;

import mx.controls.HorizontalList;
import mx.controls.listClasses.IListItemRenderer;

public class HorizontalListEx extends HorizontalList
{
    override protected function drawSelectionIndicator(indicator:Sprite, x:Number, y:Number, width:Number, height:Number, color:uint, itemRenderer:IListItemRenderer): void {
        //super.drawSelectionIndicator(indicator, x, y, width, height, 0, itemRenderer);
    }
}
}