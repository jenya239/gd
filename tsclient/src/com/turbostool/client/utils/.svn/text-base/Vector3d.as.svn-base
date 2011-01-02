package com.turbostool.client.utils
{

/**
 *
 *  +-------------->x
 *  |      |
 *  |  <--/
 *  |      a
 * \/y
 *
 */
public class Vector3d implements Clonable
{
    public static const DIMENSION:int = 3;
    /*
     private var myXProperty:Number;
     private var myYProperty:Number;*/
    private var myZProperty:Number;


    public var myX:Number;
    public var myY:Number;
    //public var myZ:Number;

    public static var shared: Vector3d = new Vector3d(0,0,0);

    public function setXYZ(x: Number, y: Number, z: Number): Vector3d
    {
        this.myX = x;
        this.myY = y;
        this.myZ = z;
        return this;
    }

    public function Vector3d(x:Number, y:Number, z:Number) {
        myX = x;
        myY = y;
        myZProperty = z;
    }

    /*public function get myX():Number {
     return myXProperty;
     }

     public function set myX(setValue:Number):void {
     myXProperty = setValue;
     }

     public function get myY():Number {
     return myYProperty;
     }

     public function set myY(setValue:Number):void {
     myYProperty = setValue;
     }
     */
    public function get myZ():Number {
        return myZProperty;
    }

    public function set myZ(setValue:Number):void {
        myZProperty = setValue;
    }

    public function add(v:Vector3d):Vector3d {
        myX += v.myX;
        myY += v.myY;
        myZ += v.myZ;
        return this;
    }

    public function sum(v:Vector3d):Vector3d {
        //return (clone() as Vector3d).add(v);
        return new Vector3d(myX + v.myX, myY + v.myY, myZ + v.myZ);
    }

    public function numberProduct(n:Number):Vector3d {
        /*var res:Vector3d = new Vector3d(0, 0, 0);
         res.myX = myX * n;
         res.myY = myY * n;
         res.myZ = myZ * n;
         return res;*/
        return new Vector3d(myX * n, myY * n, myZ * n);
    }

    public function toString():String {
        return '(' + myX + '; ' + myY + '; ' + myZ + ')';
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Vector3d)) return false;
        var v:Vector3d = (o as Vector3d);
        return (Utils.equal(v.myX, myX) && Utils.equal(v.myY, myY) && Utils.equal(v.myZ, myZ));
    }

    public function scalarProduct(v:Vector3d):Number {
        return myX * v.myX + myY * v.myY + myZ * v.myZ;
    }

    public function length():Number {
        return Math.sqrt(scalarProduct(this));
    }

    public function sqrLength():Number {
        return scalarProduct(this);
    }

    public function difference(v:Vector3d):Vector3d {
        /*var res:Vector3d = new Vector3d(0, 0, 0);
         res.myX = myX - v.myX;
         res.myY = myY - v.myY;
         res.myZ = myZ - v.myZ;
         return res;*/
        return new Vector3d(myX - v.myX, myY - v.myY, myZ - v.myZ);

    }

    public function isZero():Boolean {
        return (Utils.isZero(myX) && Utils.isZero(myY) && Utils.isZero(myZ));
    }

    public function vectorProduct(v:Vector3d):Vector3d {
        /*var res:Vector3d = new Vector3d(0, 0, 0);
         res.myX = myY * v.myZ - v.myY * myZ;
         res.myY = myZ * v.myX - v.myZ * myX;
         res.myZ = myX * v.myY - v.myX * myY;
         return res;*/
        return new Vector3d(myY * v.myZ - v.myY * myZ,
                myZ * v.myX - v.myZ * myX,
                myX * v.myY - v.myX * myY);
    }

    public static function getZero():Vector3d {
        return new Vector3d(0, 0, 0);
    }

    public function clone():Clonable {
        return new Vector3d(myX, myY, myZ);
    }

    public function inverse():Vector3d {
        return numberProduct(-1);
    }

    public function copyTo(v:Vector3d):void {
        v.myX = myX;
        v.myY = myY;
        if (! v is Vector2d) {
            v.myZ = myZ;
        }
    }

    public function getCoordinate(index:int):Number {
        switch (index) {
            case 0: return myX; break;
            case 1: return myY; break;
            case 2: return myZ; break;
            default:
                throw new TSError('В векторе 3х1 не существует элемента (' + index + ', 1)');
                break;
        }
        return 0;
    }

    public function setCoordinate(index:int, value:Number):void {
        switch (index) {
            case 0: myX = value; break;
            case 1: myY = value; break;
            case 2: myZ = value; break;
            default:
                throw new TSError('В векторе 3х1 не существует элемента (' + index + ', 0)');
                break;
        }
    }

    public function ort3d():Vector3d {
        var length:Number = length();
        if (Utils.equal(length, 0)) throw new TSError('невозможно вычислить орт нулевого вектора');
        return numberProduct(1 / length);
    }

    public function epsilonEquals(v:Vector3d, epsilon:Number):Boolean {
        return (
                (Math.abs(v.myX - myX) < epsilon) &&
                (Math.abs(v.myY - myY) < epsilon) &&
                (Math.abs(v.myZ - myZ) < epsilon)
                );
    }

    public function scalarProjection(base:Vector3d):Number {
        //return base.ort3d().scalarProduct(this);
        var length:Number = base.length();
        if (Utils.equal(length, 0)) throw new TSError('невозможно вычислить проекцию на нулевой вектор');
        return ( myX * base.myX + myY * base.myY + myZ * base.myZ ) / length;
    }

    public function projection(base:Vector3d):Vector3d {
        var ort:Vector3d = base.ort3d();
        return ort.multiply(ort.scalarProduct(this));
    }

    public function multiply(n:Number): Vector3d {
        myX *= n;
        myY *= n;
        myZ *= n;
        return this;
    }

    public function getOrtogComponentBy(base:Vector3d):Vector3d {
        return difference(projection(base));
    }

    public function rounded(countOfDigit:int = 0):Vector3d {
        return new Vector3d(
                Utils.round(myX, countOfDigit),
                Utils.round(myY, countOfDigit),
                Utils.round(myZ, countOfDigit)
                );
    }

    public static function parseTo3dVector(s:String):Vector3d {
        var pattern:RegExp = new RegExp("^[-]*[\\d.]+$", "");
        var coordinates:Array = s.split(' ');
        if (coordinates.length != 3) throw new TSError('Impossible parse ' + s + ' to Vector3d');
        for each (var el:String in coordinates) {
            if (!pattern.test(el)) throw new TSError('Impossible parse ' + s + ' to Vector3d');
        }
        var v:Vector3d = new Vector3d(coordinates[0], coordinates[1], coordinates[2]);
        return v;

        //Alert.show(coordinates.toString());
        //return true;
    }

    public function getScalarProjectionByAngle(angle:Number):Number {
        return myX * Math.cos(angle) + myY * Math.sin(angle);
    }


}
}