package com.turbostool.client.newGraphic
{
import com.turbostool.client.geom.*;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Vector2d;

import flash.display.BitmapData;
import flash.geom.Matrix;
import flash.geom.Point;
import flash.utils.getQualifiedClassName;

public class NGDrawable
{
    private var myFrameProperty: LocalFrame2d;
    private var myLayerProperty: Number; // только NGGraphicEngine должна это знать?
    private var myHeightProperty: Number;

    public function NGDrawable(frame: LocalFrame2d, height: Number, layer: Number)
    {
        if (getQualifiedClassName(this) == "com.turbostool.client.newGraphic::NGDrawable")
        {
            throw new TSError("NGDrawable can't be instantiated directly");
        }
        myFrameProperty = frame;
        myHeightProperty = height;
        myLayerProperty = layer;
    }

    public final function draw(ngg: NGGraphics): void
    {
        var transform: Matrix = LocalFrame2d.getGlobalMatrix(myR, myAngle);
        ngg.pushTransform(transform);
        drawContent(ngg);
        ngg.popTransform();
    }

    protected function drawContent(ngg: NGGraphics): void
    {
        throw new TSError("NGDrawable can't be drawn directly");
    }

    public function get myAngle(): Number
    {
        return myFrame.myAngle;
    }

    public function set myAngle(newAngle: Number): void
    {
        myFrame.myAngle = newAngle;
    }

    public function get myR(): Vector2d
    {
        return myFrame.myR;
    }

    public function set myR(newR: Vector2d): void
    {
        myFrame.myR = newR;
    }

    public function get myHeight(): Number
    {
        return myHeightProperty;
    }

    public function set myHeight(newHeight: Number): void
    {
        myHeightProperty = newHeight;
    }

    public function get myLayer(): Number
    {
        return myLayerProperty;
    }

    public function set myLayer(layer: Number): void
    {
        myLayerProperty = layer;
    }

    public function get myWidth(): Number
    {
        throw new TSError("myWidth is not defined for NGDrawable");
    }

    public function get myLength(): Number
    {
        throw new TSError("myLength is not defined for NGDrawable");
    }

    public function get myFrame(): LocalFrame2d
    {
        return myFrameProperty;
    }

    public function get myShadow(): NGShadow
    {
        return null;
    }

    public function clone(): NGDrawable
    {
        throw new TSError("NGDrawable can't be cloned!");
    }

    protected function copyTexture(texture: BitmapData, clone: Boolean = false): BitmapData
    {
        var result: BitmapData;
        if (clone)
        {
            result = texture.clone();
        }
        else
        {
            result = texture;
        }
        return result;
    }

    private const _zeroPoint: Point = new Point(0, 0);

    protected function convertTexture(texture: BitmapData, transparency: Object, clone: Boolean = false): BitmapData
    {
        var result: BitmapData;
        if (transparency != null)
        {
            result = new BitmapData(texture.width, texture.height, transparency as Boolean, 0xffff00);
            result.copyPixels(texture, texture.rect, _zeroPoint);
        }
        else
        {
            result = copyTexture(texture, clone);
        }

        return result;
    }

    public function get myRadiusSqr(): Number
    {
        return Number.POSITIVE_INFINITY;
    }

    public function get myGeometricCenter(): Vector2d
    {
        return myFrame.myR;
    }
}
}