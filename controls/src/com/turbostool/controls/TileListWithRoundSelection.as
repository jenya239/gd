package com.turbostool.controls {
import flash.display.Graphics;
import flash.display.Sprite;

import mx.controls.*;
import mx.controls.listClasses.IListItemRenderer;

public class TileListWithRoundSelection extends TileList {
    public function TileListWithRoundSelection() {
        super();
    }

    override protected function drawSelectionIndicator(indicator:Sprite, x:Number, y:Number, width:Number, height:Number, color:uint, itemRenderer:IListItemRenderer):void
    {
        //super.drawSelectionIndicator(indicator, x, y, width, height, 0, itemRenderer);
        
        var g: Graphics = Sprite(indicator).graphics;
        
        g.clear();
        g.beginFill(color);

        g.lineStyle(0, 0xffffff);

        g.drawRoundRect(0, 0, width, height, 6, 6);

        g.endFill();

        indicator.x = x;
        indicator.y = y;
    }
}
}