package com.turbostool.client.geom
{
import com.turbostool.client.utils.*;

import flash.geom.Matrix;

public class LocalFrame2d implements Clonable {

    public var myR:Vector2d;
    public var myAngle:Number;

    public function LocalFrame2d(r:Vector2d, angle:Number = 0) {
        myR = r;
        myAngle = angle;
    }

    public function getLocalTemp(globalV:Vector2d, temp:Vector2d):Vector2d {
        return globalV.setDifferenceTo(myR, temp).rotate(-myAngle);
    }

    public function getLocal(globalV:Vector2d):Vector2d {
        //return (globalV.difference2d(myR)).getRotated(-myAngle);
        return (globalV.difference2d_shared(myR)).rotate(-myAngle);
        /*
         var sin:Number = Math.sin(-myAngle);
         var cos:Number = Math.cos(-myAngle);
         var tx:Number = globalV.myX - myR.myX;
         var ty:Number = globalV.myY - myR.myY;
         return new Vector2d( tx * cos - ty * sin,
         tx * sin + ty * cos );*/
    }

    public function getGlobal(localV:Vector2d):Vector2d {
        //return (localV.getRotated(myAngle)).sum2d(myR);
        var v:Vector2d = (localV.getRotated(myAngle));
        v.add(myR);
        return v;
        /*
         var sin:Number = Math.sin(myAngle);
         var cos:Number = Math.cos(myAngle);
         var tx:Number = localV.myX * cos - localV.myY * sin;
         var ty:Number = localV.myX * sin + localV.myY * cos
         return new Vector2d( tx + myR.myX,
         ty + myR.myY);*/
    }

    public function getLocalNonLength(globalV:Vector2d):Vector2d {
        return globalV.getRotated(-myAngle);
    }

    public function getLocalNonLengthNonSafe(globalV:Vector2d):Vector2d {
        return globalV.rotate(-myAngle);
    }

    public function getGlobalNonLength(localV:Vector2d):Vector2d {
        return localV.getRotated(myAngle);
    }

    public function superposition(lf:LocalFrame2d):LocalFrame2d {
        var r:Vector2d = getGlobal(lf.myR);
        var res:LocalFrame2d = new LocalFrame2d(r, myAngle + lf.myAngle);
        return res;
    }

    private static var _getTransformResult: Matrix = new Matrix();

    public function getTransform_shared(): Matrix {
        _getTransformResult.identity();
        _getTransformResult.translate(- myR.myX, - myR.myY);
        _getTransformResult.rotate(- myAngle);
        return _getTransformResult;
    }

    private static var _getInverseTransformResult: Matrix = new Matrix();

    public function getInverseTransform(): Matrix {
        _getInverseTransformResult.identity();
        _getInverseTransformResult.rotate(myAngle);
        _getInverseTransformResult.translate(myR.myX, myR.myY);
        return _getInverseTransformResult;
    }

    /*public function invertion():LocalFrame2d {
     return new LocalFrame2d(myR.invertion2d(), - myAngle);
     }*/

    private static var _globalMatrix: Matrix = new Matrix();

    public static function getGlobalMatrix(r: Vector2d, angle: Number): Matrix {
        _globalMatrix.identity();
        _globalMatrix.rotate(angle);
        _globalMatrix.translate(r.myX, r.myY);
        return _globalMatrix;
    }

    public function copyTo(lf:LocalFrame2d):void {
        lf.myAngle = myAngle;
        myR.copyTo2d(lf.myR);
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is LocalFrame2d)) return false;
        var lf:LocalFrame2d = (o as LocalFrame2d);
        return (lf.myR.equals(myR) && Utils.equal(myAngle, lf.myAngle));
    }

    public function getXOrt():Vector2d {
        return new Vector2d(Math.cos(myAngle), Math.sin(myAngle));
    }

    public function getYOrt():Vector2d {
        return    new Vector2d(-Math.sin(myAngle), Math.cos(myAngle));
    }

    public static function getZero():LocalFrame2d {
        return new LocalFrame2d(Vector2d.getZero(), 0);
    }

    public function clone():Clonable {
        return new LocalFrame2d(myR.clone() as Vector2d, myAngle);
    }

    public function relativeTo(supportFrame:LocalFrame2d):LocalFrame2d {
        var r:Vector2d = supportFrame.getLocal(myR);
        var res:LocalFrame2d = new LocalFrame2d(r, myAngle - supportFrame.myAngle);
        return res;
    }

    public function relativeToTemp(supportFrame:LocalFrame2d, tempFrame:LocalFrame2d):LocalFrame2d {
        supportFrame.getLocalTemp(myR, tempFrame.myR);
        tempFrame.myAngle = myAngle - supportFrame.myAngle;
        return tempFrame;
    }

    public function toString():String {
        return '{' + myR + '; ' + myAngle + '}';
    }

    public function setXYU(x:Number, y:Number, angle:Number):LocalFrame2d {
        myR.setXY(x, y);
        myAngle = angle;
        return this;
    }
}
}