package com.turbostool.client.newGraphic
{
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.utils.Vector2d;

import flash.display.BitmapData;

public class NGTexturedRect extends NGDrawable
{
    private var _texture: BitmapData;
    private var _width: Number;
    private var _length: Number;
    private var _transparency: Boolean;
    private var _radiusSqrCached: Number;

    private var _debugShadow: NGShadow;

    public function NGTexturedRect(frame: LocalFrame2d, height: Number, layer: Number, width: Number, length: Number, texture: BitmapData, transparent: Object = null)
    {
        super(frame, height, layer);

        _texture = convertTexture(texture, transparent);
        _transparency = transparent == null ? _texture.transparent : transparent as Boolean;

        _width = width;
        _length = length;
        updateRadiusSqr();

        _debugShadow = NGShadow.createRectShadow(this);
    }

    private function updateRadiusSqr():void
    {
        _radiusSqrCached = myRadiusSqr_slow;
    }

    override public function get myShadow(): NGShadow
    {
        return _debugShadow;
    }

    override public function get myWidth(): Number
    {
        return _width;
    }

    public function set myWidth(newWidth: Number): void
    {
        _width = newWidth;
        updateRadiusSqr();
    }

    override public function get myLength(): Number
    {
        return _length;
    }

    override public function get myRadiusSqr(): Number
    {
        return _radiusSqrCached;
    }

    public function get myRadiusSqr_slow(): Number
    {
        return myWidth * myWidth + myLength * myLength;
    }

    public function set myLength(newLength: Number): void
    {
        _length = newLength;
        updateRadiusSqr();
    }

    public function setTexture(texture: BitmapData): void
    {
        _texture = convertTexture(texture, _transparency);
    }

    override protected function drawContent(ngg: NGGraphics): void
    {
        ngg.drawTexturedRect(_texture, _width, _length);
    }

    override public function clone(): NGDrawable
    {
        var newFrame: LocalFrame2d = myFrame.clone() as LocalFrame2d;
        var newTexture: BitmapData = copyTexture(_texture);
        var result: NGTexturedRect = new NGTexturedRect(newFrame, myHeight, myLayer, myWidth, myLength, newTexture, _transparency);
        return result;
    }

    public function getLine():Piece2d
    {
        var d:Vector2d = Vector2d.createByAngle(myFrame.myAngle, _width);
        var begin:Vector2d = myFrame.myR.clone() as Vector2d;
        var end:Vector2d = myFrame.myR.clone() as Vector2d;
        begin.addMultiplied(d, 1);
        end.addMultiplied(d, -1);
        return new Piece2d(begin, end);
    }
}
}