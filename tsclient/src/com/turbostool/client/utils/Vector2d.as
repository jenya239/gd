package com.turbostool.client.utils
{
import flash.geom.Matrix;

/**
 *Vector2d - это Vector3d с z == 0!
 */
public class Vector2d extends Vector3d
{
    public function Vector2d(x:Number, y:Number) {
        super(x, y, 0);
        /*if ((args.length == 2)) {
         myX = args[0];
         myY = args[1];
         //myXProperty = args[0];
         //myYProperty = args[1];
         //myZProperty = 0;
         //super(args[0], args[1], 0);
         }
         else if ((args.length == 1)) {
         if (!(args[0] is Vector3d)) throw new TSError('Не корректные аргументы: ' + args);
         var v:Vector3d = args[0] as Vector3d;
         myX = v.myX;
         myY = v.myY;
         //myXProperty = args[0];
         //myYProperty = args[1];
         //myZProperty = 0;
         //super(args[0], args[1], 0);
         } else {
         throw new TSError('Не корректные аргументы: ' + args);
         }*/
    }

    override public function get myZ():Number {
        return 0;
    }

    override public function set myZ(setValue:Number):void {
        throw new TSError('Нельзя изменить Z координату Vector2d');
    }

    //		override public function sum(v:Vector3d):Vector3d {

    //	}

    public static function createFrom3d(v:Vector3d):Vector2d {
        return new Vector2d(v.myX, v.myY);
    }

    public function sum2d(v:Vector2d):Vector2d {
        return new Vector2d(v.myX + myX, v.myY + myY);
        //return createFrom3d(sum(v));
    }

    public function numberProduct2d(n:Number):Vector2d {
        //return createFrom3d(numberProduct(n));
        return new Vector2d(myX * n, myY * n);
    }


    public function scalarProductDifference(a:Vector2d, b:Vector2d):Number {
        return this.myX * (a.myX - b.myX) + this.myY * ( a.myY - b.myY );
    }

    public function multiply2d(n:Number) : Vector2d {
        this.myX *= n;
        this.myY *= n;
        return this;
    }

    public static function parseTo2dVector(s:String):Vector2d {
        var pattern:RegExp = new RegExp("^[-]*[\\d.]+$", "");
        var coordinates:Array = s.split(' ');
        if (coordinates.length != 2) throw new TSError('Impossible parse ' + s + ' to Vector2d');
        for each (var el:String in coordinates) {
            if (!pattern.test(el)) throw new TSError('Impossible parse ' + s + ' to Vector2d');
        }
        var v:Vector2d = new Vector2d(coordinates[0], coordinates[1]);
        return v;

        //Alert.show(coordinates.toString());
        //return true;
    }

    public function difference2d(v:Vector2d):Vector2d {
        return new Vector2d(myX - v.myX, myY - v.myY);
        //return createFrom3d(difference(v));
    }

    public function difference2d_result(v:Vector2d, result: Vector2d):void {
        result.myX = myX - v.myX;
        result.myY = myY - v.myY;
    }

    private static var _vectorForDifference: Vector2d = new Vector2d(0,0);

    public function difference2d_shared(v:Vector2d):Vector2d {
        difference2d_result(v, _vectorForDifference);
        return _vectorForDifference;
    }

    public function setDifferenceTo(v:Vector2d, temp:Vector2d):Vector2d {
        temp.myX = myX - v.myX;
        temp.myY = myY - v.myY;
        return temp;
    }

    public static function getZero():Vector2d {
        return new Vector2d(0, 0);
    }

    override public function add(v:Vector3d):Vector3d {
        /*if( Utils.nonZero( v.myZ ) ){
         throw new TSError('вектор на плоскости может иметь только z = 0');
         }*/
        myX += v.myX;
        myY += v.myY;
        return this;
    }

    public function getRotated(angle:Number):Vector2d {
        /*var v:Vector2d = Vector2d.getZero();
         v.myX = myX * Math.cos(angle) - myY * Math.sin(angle);
         v.myY = myX * Math.sin(angle) + myY * Math.cos(angle);
         return v;*/
        var sin:Number = Math.sin(angle);
        var cos:Number = Math.cos(angle);
        return new Vector2d(myX * cos - myY * sin, myX * sin + myY * cos);
    }

    public function rotate(angle:Number):Vector2d {
        var sin:Number = Math.sin(angle);
        var cos:Number = Math.cos(angle);
        var x:Number = myX * cos - myY * sin;
        var y:Number = myX * sin + myY * cos;
        myX = x;
        myY = y;
        return this;
    }

    public function ort2d(): Ort2d {
        var length:Number = length();
        if (Utils.equal(length, 0)) throw new TSError('невозможно вычислить орт нулевого вектора');
        return Ort2d.createFromVector2d(numberProduct2d(1 / length));
    }

    public function ortogonal2d(): Vector2d {
        return new Vector2d(-myY, myX);
    }

    public function setOrtogonal2dTo(v:Vector2d):Vector2d {
        v.myX = -myY;
        v.myY = myX;
        return v;
    }

    override public function clone(): Clonable {
        //return Vector2d.createFrom3d(super.clone() as Vector3d);
        return new Vector2d(myX, myY);
    }

    public function invertion2d(): Vector2d {
        return new Vector2d(- myX, - myY);
    }

    public static function createByAngle(angle:Number, length:Number):Vector2d {
        return new Vector2d(length * Math.cos(angle), length * Math.sin(angle));
    }

    override public function isZero():Boolean {
        return (Utils.isZero(myX) && Utils.isZero(myY));
    }

    public function getAngle():Number {
        if (isZero()) {
            throw new TSError('Невозможно вычислить угол нулевого вектора');
        }
        return Math.atan2(myY, myX);
    }

    public function getScalarOrtogonal(base:Vector2d):Number {
        //return base.ort2d().ortogonal2d().scalarProduct(this);
        var length:Number = base.length();
        if (Utils.isZero(length)) throw new TSError('невозможно вычислить getScalarOrtogonal');
        return ( - base.myY * myX + base.myX * myY ) / length;
    }

    override public function scalarProduct(v:Vector3d):Number {
        return myX * v.myX + myY * v.myY;
    }

    override public function scalarProjection(base:Vector3d):Number {
        //return base.ort3d().scalarProduct(this);
        var length:Number = base.length();
        if (Utils.equal(length, 0)) throw new TSError('невозможно вычислить проекцию на нулевой вектор');
        return ( myX * base.myX + myY * base.myY + myZ * base.myZ ) / length;
    }

    /**
     * возвращает ортогональную проекцию this на вектор base
     **/
    public function getOrtComponentBy(base:Vector2d, temp:Vector2d):Vector2d {
        base.setOrtogonal2dTo(temp);
        temp.multiply2d(1 / temp.length());
        temp.multiply2d(temp.myX * myX + temp.myY * myY);
        return temp;
    }

    public function setProjectionToVector(temp:Vector2d):Vector2d {
        temp.multiply2d(1 / temp.length());
        temp.multiply2d(temp.myX * myX + temp.myY * myY);
        return temp;
    }

    public function copyTo2d(v:Vector2d):void {
        v.myX = myX;
        v.myY = myY;
    }

    public function setProjectionToAngle(angle:Number):Vector2d {
        var sin:Number = Math.sin(angle);
        var cos:Number = Math.cos(angle);
        var l:Number = myX * cos + myY * sin;
        myX = l * cos;
        myY = l * sin;
        return this;
    }

    public function addMultiplied(v:Vector2d, n:Number):Vector2d {
        myX += v.myX * n;
        myY += v.myY * n;
        return this;
    }

    public function setZero():Vector2d {
        myX = 0;
        myY = 0;
        return this;
    }

    public function add2d(v:Vector2d):Vector2d {
        myX += v.myX;
        myY += v.myY;
        return this;
    }

    public function vectorProductZ(v:Vector2d):Number {
        return myX * v.myY - v.myX * myY;
    }

    public function setXY(x:Number, y:Number):Vector2d {
        myX = x;
        myY = y;
        return this;
    }

    public function toFixedString(countOfDigit:int, precision:int):String {
        return "( " + Utils.numberToString(myX, countOfDigit, precision) + "; " + Utils.numberToString(myY, countOfDigit, precision) + " )";
    }

    override public function length():Number {
        return Math.sqrt(myX * myX + myY * myY);
    }

    public function lengthSqr():Number {
        return myX * myX + myY * myY;
    }

    public function setLength(l:Number):Vector2d {
        var oldLength:Number = this.length();
        myX *= l / oldLength;
        myY *= l / oldLength;
        return this;
    }

    public function translateSetTo(m: Matrix): Matrix {
        m.translate(myX, myY);
        return m;
    }

    public function getProjection(v: Vector2d): Vector2d {
        return numberProduct2d(scalarProduct(v) / sqrLength());
    }

    public function getAngleTo(v: Vector2d): Number {
        return getAngle() - v.getAngle();
    }

    public function distanceTo(v: Vector2d): Number {
        return Math.sqrt(Utils.sqr(v.myX - myX) + Utils.sqr(v.myY - myY));
    }

}
}