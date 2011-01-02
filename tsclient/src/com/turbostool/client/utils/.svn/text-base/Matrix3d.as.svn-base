package com.turbostool.client.utils
{
import flash.geom.Matrix;

public class Matrix3d {
    public static const DIMENSION:int = 3;

    private var myElements:Array;

    public function Matrix3d() {
        myElements = new Array(3);
        for (var i:int = 0; i < DIMENSION; i++) {
            myElements[i] = new Array(0.0, 0.0, 0.0);
        }
    }

    public function getElement(row:int, col:int):Number {
        if ((row < 0) || (row >= DIMENSION) || (col < 0) || (col >= DIMENSION)) {
            throw new TSError('В матрице 3х3 не существует элемента (' + row + '; ' + col + ')');
        }
        return myElements[row][col];
    }

    public function setElement(row:int, col:int, value:Number):void {
        if ((row < 0) || (row >= DIMENSION) || (col < 0) || (col >= DIMENSION)) {
            throw new TSError('В матрице 3х3 не существует элемента (' + row + '; ' + col + ')');
        }
        myElements[row][col] = value;
    }

    public function product(m:Matrix3d):Matrix3d {
        var res:Matrix3d = new Matrix3d();
        for (var i:int = 0; i < DIMENSION; i++)
            for (var j:int = 0; j < DIMENSION; j++) {
                var sum:Number = 0;
                for (var k:int = 0; k < DIMENSION; k++) {
                    sum += myElements[i][k] * m.myElements[k][j];
                }
                res.myElements[i][j] = sum;
            }
        return res;
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Matrix3d)) return false;
        var m:Matrix3d = (o as Matrix3d);
        for (var i:int = 0; i < DIMENSION; i++)
            for (var j:int = 0; j < DIMENSION; j++) {
                if (!Utils.equal(getElement(i, j), m.getElement(i, j))) return false;
            }
        return true;
    }

    public static function getZero():Matrix3d {
        return new Matrix3d();
    }

    public static function getIdentity():Matrix3d {
        return createFromFlashMatrix(new Matrix());
    }

    public function getWithUVWEqual001():Matrix {
        return new Matrix(
                getElement(0, 0), getElement(1, 0),
                getElement(0, 1), getElement(1, 1), getElement(0, 2), getElement(1, 2)
                );
    }

    public static function createFromFlashMatrix(m:Matrix):Matrix3d {
        var matrix:Matrix3d = new Matrix3d();
        matrix.setElement(0, 0, m.a);
        matrix.setElement(0, 1, m.c);
        matrix.setElement(0, 2, m.tx);
        matrix.setElement(1, 0, m.b);
        matrix.setElement(1, 1, m.d);
        matrix.setElement(1, 2, m.ty);
        matrix.setElement(2, 0, 0);
        matrix.setElement(2, 1, 0);
        matrix.setElement(2, 2, 1);
        return matrix;
    }

    public function vectorProduct(v:Vector3d):Vector3d {
        var res:Vector3d = Vector3d.getZero();
        for (var i:int = 0; i < Matrix3d.DIMENSION; i++)
            for (var j:int = 0; j < Matrix3d.DIMENSION; j++)
                res.setCoordinate(i, res.getCoordinate(i) + getElement(i, j) * v.getCoordinate(j));
        return res;
    }

    public function toString():String {
        var s:String = '';
        for (var i:int = 0; i < Matrix3d.DIMENSION; i++) {
            for (var j:int = 0; j < Matrix3d.DIMENSION; j++) s += getElement(i, j) + ' ';
            s += '\n';
        }
        return s;
    }

    public function vector2dWithZEqualOneProduct(v:Vector2d):Vector2d {
        //var v3d:Vector3d = new Vector3d(v.myX, v.myY, 1);
        //return Vector2d.createFrom3d(vectorProduct(v3d));
        return new Vector2d(v.myX * myElements[0][0] + v.myY * myElements[0][1] + myElements[0][2],
                v.myX * myElements[1][0] + v.myY * myElements[1][1] + myElements[1][2]);
    }

    public function vector2dWithZEqualOneProductQuick(v:Vector2d, result:Vector2d):Vector2d {
        result.myX = v.myX * myElements[0][0] + v.myY * myElements[0][1] + myElements[0][2];
        result.myY = v.myX * myElements[1][0] + v.myY * myElements[1][1] + myElements[1][2];
        return result;
    }

    public function transposition():Matrix3d {
        var res:Matrix3d = new Matrix3d();
        for (var i:int = 0; i < Matrix3d.DIMENSION; i++)
            for (var j:int = 0; j < Matrix3d.DIMENSION; j++)
                res.setElement(i, j, getElement(j, i));
        return res;
    }

    public function Row2dProduct(v:Vector2d):Vector2d {
        var v3d:Vector3d = Vector3d.shared.setXYZ(v.myX, v.myY, 1);
        return Vector2d.createFrom3d(transposition().vectorProduct(v3d));
    }

    public static function translation(dr:Vector2d):Matrix3d {
        var res:Matrix3d = new Matrix3d();
        res.setElement(0, 0, 1);
        res.setElement(0, 1, 0);
        res.setElement(0, 2, dr.myX);
        res.setElement(1, 0, 0);
        res.setElement(1, 1, 1);
        res.setElement(1, 2, dr.myY);
        res.setElement(2, 0, 0);
        res.setElement(2, 1, 0);
        res.setElement(2, 2, 1);
        return res;
    }

    public static function stretching(scaleX:Number, scaleY:Number):Matrix3d {
        var res:Matrix3d = new Matrix3d();
        res.setElement(0, 0, scaleX);
        res.setElement(0, 1, 0);
        res.setElement(0, 2, 0);
        res.setElement(1, 0, 0);
        res.setElement(1, 1, scaleY);
        res.setElement(1, 2, 0);
        res.setElement(2, 0, 0);
        res.setElement(2, 1, 0);
        res.setElement(2, 2, 1);
        return res;
    }

    public static function rotation(angle:Number):Matrix3d {
        var res:Matrix3d = new Matrix3d();
        res.setElement(0, 0, Math.cos(angle));
        res.setElement(0, 1, - Math.sin(angle));
        res.setElement(0, 2, 0);
        res.setElement(1, 0, Math.sin(angle));
        res.setElement(1, 1, Math.cos(angle));
        res.setElement(1, 2, 0);
        res.setElement(2, 0, 0);
        res.setElement(2, 1, 0);
        res.setElement(2, 2, 1);
        return res;
    }

}
}