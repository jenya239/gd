package com.turbostool.client.newGraphic {
import com.turbostool.client.geom.LocalFrame2d;

import flash.geom.Matrix;

public class NGCamera {
    private var myFrameProperty: LocalFrame2d;
    private var myHeightProperty: Number;
    private var myScreenHeightProperty: Number;

    public function NGCamera(frame: LocalFrame2d, height: Number, screenHeight: Number) {
        myFrameProperty = frame;
        myHeightProperty = height;
        myScreenHeightProperty = screenHeight;
    }

//    public function getTransform(): Matrix {
//        var prescale: Matrix = new Matrix();
//        var scale: Number = myScreenHeightProperty / myHeightProperty;
//        prescale.scale(scale, scale);
//
//        var result: Matrix = myFrameProperty.getTransform();
//        result.concat(prescale);
//
//        return result;
//    }

    private var _getScaleTransformMatrix: Matrix = new Matrix();

    public function getScaleTransform(): Matrix {
        _getScaleTransformMatrix.identity();
        var scale: Number = myScreenHeightProperty / myHeightProperty;
        _getScaleTransformMatrix.scale(scale, scale);
        return _getScaleTransformMatrix;
    }

    public function getTranslateTransform(): Matrix {
        return myFrameProperty.getTransform_shared();
    }
            
//    public function getInverseTransform(): Matrix {
//        var preTransform: Matrix = new Matrix();
//        preTransform = myFrameProperty.getInverseTransform();
//        var invScale: Number = myHeightProperty / myScreenHeightProperty;
//        var result: Matrix = new Matrix();
//        result.scale(invScale, invScale);
//        result.concat(preTransform);
//
//        return result;
//    }

    public function get myScreenHeight(): Number {
        return myScreenHeightProperty;
    }

    public function set myScreenHeight(newSHeight: Number): void {
        myScreenHeightProperty = newSHeight;
    }

    public function get myFrame():LocalFrame2d {
        return myFrameProperty;
    }

    public function set myFrame(setValue: LocalFrame2d):void {
        myFrameProperty = setValue;
    }

    public function get myHeight():Number {
        return myHeightProperty;
    }

    public function set myHeight(setValue:Number):void {
        myHeightProperty = setValue;
    }
}
}