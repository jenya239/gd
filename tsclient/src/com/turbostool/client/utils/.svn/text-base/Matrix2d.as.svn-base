package com.turbostool.client.utils {

public class Matrix2d {
    public static const DIMENSION:int = 2;

    private var myElements:Array;

    public function Matrix2d(a11:Number = 0, a12:Number = 0, a21:Number = 0, a22:Number = 0) {
        myElements = new Array(2);
        for (var i:int = 0; i < DIMENSION; i++) {
            myElements[i] = new Array(2);
        }
        myElements[0][0] = a11;
        myElements[0][1] = a12;
        myElements[1][0] = a21;
        myElements[1][1] = a22;
    }

    public function get myA11():Number {
        return myElements[0][0];
    }

    public function get myA12():Number {
        return myElements[0][1];
    }

    public function get myA21():Number {
        return myElements[1][0];
    }

    public function get myA22():Number {
        return myElements[1][1];
    }

    public function set myA11(setValue:Number):void {
        myElements[0][0] = setValue;
    }

    public function set myA12(setValue:Number):void {
        myElements[0][1] = setValue;
    }

    public function set myA21(setValue:Number):void {
        myElements[1][0] = setValue;
    }

    public function set myA22(setValue:Number):void {
        myElements[1][1] = setValue;
    }

    public function determinant():Number {
        return myA11 * myA22 - myA12 * myA21;
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Matrix2d)) return false;
        var m:Matrix2d = (o as Matrix2d);
        return (Utils.equal(myA11, m.myA11) && Utils.equal(myA12, m.myA12)
                && Utils.equal(myA21, m.myA21) && Utils.equal(myA22, m.myA22));
    }

    public function toString():String {
        return '[[' + myA11 + '; ' + myA12 + '], [' + myA21 + '; ' + myA22 + ']]';
    }
}
}