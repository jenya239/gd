package com.turbostool.client.game.view
{
import flash.display.Graphics;

import mx.containers.Canvas;
import mx.controls.Text;

public class RudderView extends Canvas
{

    private var _maxOffsetRadians: Number = 0.5;
    private var _borderWidth : int = 1;
    private var _width : int = 200;
    private var _height : int = 20;
    private var _barWidth : int = 10;

    private var _turnAngleLabel:Text = new Text();

    public function RudderView() {
        super();
        //setStyle("right", 10);
        setStyle("bottom", 150);
        this.width = this._width;
        this.height = this._height
        this.setStyle("horizontalCenter", 0);
        createTurnAngleLabel();
    }

    private function createTurnAngleLabel():void {
        _turnAngleLabel.setStyle("color", 0xFFFFFF);
        _turnAngleLabel.setStyle("right", 0);
        _turnAngleLabel.setStyle("bottom", 0);
        _turnAngleLabel.setStyle("fontSize", 14);
        _turnAngleLabel.htmlText = "0";
        addChild(_turnAngleLabel);
    }

    public function draw(turnAngleRadians: Number):void {
        var gr:Graphics = this.graphics;
        gr.clear();
        gr.beginFill(0xff0000, 0.5);

        if (turnAngleRadians > _maxOffsetRadians) turnAngleRadians = _maxOffsetRadians;

        gr.drawRect(0, 0, _width, _height);

        gr.beginFill(0xffffff, 1);
        gr.drawRect(_width / 2, 0, 2, _height);
        //gr.endFill();

        gr.beginFill(0xffffff, 0.5);

        var x : int = turnAngleRadians * (_width / 2 - _barWidth / 2 - _borderWidth) / _maxOffsetRadians;

        gr.drawRect(_borderWidth + x - _barWidth / 2 + _width / 2, _borderWidth, _barWidth, _height - _borderWidth * 2);

        gr.endFill();

        _turnAngleLabel.htmlText = "" + Math.round(turnAngleRadians * 180 / Math.PI);
    }

}
}