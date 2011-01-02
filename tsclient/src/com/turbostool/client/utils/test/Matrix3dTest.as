package com.turbostool.client.utils.test
{
import com.turbostool.client.utils.*;

import flash.geom.Matrix;
import flash.geom.Point;

public class Matrix3dTest extends TestCase {
    private var myMatrix:Matrix3d;

    public function Matrix3dTest(methodName:String = null) {
        super(methodName);
    }

    public function testMatrix3d():void {
        var m:Matrix3d = new Matrix3d();
        assertTrue(Utils.equal(0, m.getElement(0, 0)));
        assertTrue(Utils.equal(0, m.getElement(0, 1)));
        assertTrue(Utils.equal(0, m.getElement(0, 2)));
        assertTrue(Utils.equal(0, m.getElement(1, 0)));
        assertTrue(Utils.equal(0, m.getElement(1, 1)));
        assertTrue(Utils.equal(0, m.getElement(1, 2)));
        assertTrue(Utils.equal(0, m.getElement(2, 0)));
        assertTrue(Utils.equal(0, m.getElement(2, 1)));
        assertTrue(Utils.equal(0, m.getElement(2, 2)));
        assertTrue(m.equals(Matrix3d.getZero()));
    }

    public function testProduct():void {
        var m1:Matrix3d = new Matrix3d();
        var m2:Matrix3d = new Matrix3d();
        var res:Matrix3d = new Matrix3d();

        myMatrix = m1
        myMatrix.setElement(0, 0, 1);
        myMatrix.setElement(0, 1, 1);
        myMatrix.setElement(0, 2, 1);
        myMatrix.setElement(1, 0, 1);
        myMatrix.setElement(1, 1, 1);
        myMatrix.setElement(1, 2, 1);
        myMatrix.setElement(2, 0, 1);
        myMatrix.setElement(2, 1, 1);
        myMatrix.setElement(2, 2, 1);
        myMatrix = m2
        myMatrix.setElement(0, 0, 2);
        myMatrix.setElement(0, 1, 1);
        myMatrix.setElement(0, 2, 1);
        myMatrix.setElement(1, 0, 2);
        myMatrix.setElement(1, 1, 1);
        myMatrix.setElement(1, 2, 1);
        myMatrix.setElement(2, 0, 2);
        myMatrix.setElement(2, 1, 1);
        myMatrix.setElement(2, 2, 1);
        myMatrix = res
        myMatrix.setElement(0, 0, 6);
        myMatrix.setElement(0, 1, 3);
        myMatrix.setElement(0, 2, 3);
        myMatrix.setElement(1, 0, 6);
        myMatrix.setElement(1, 1, 3);
        myMatrix.setElement(1, 2, 3);
        myMatrix.setElement(2, 0, 6);
        myMatrix.setElement(2, 1, 3);
        myMatrix.setElement(2, 2, 3);

        assertTrue(res.equals(m1.product(m2)));

        var v:Vector3d = new Vector3d(1, 2, 3);
        assertTrue(m1.vectorProduct(v).equals(new Vector3d(6, 6, 6)));

        var fm:Matrix = new Matrix(3, 4, 6, 2, 2, 8);
        var p:Point = fm.transformPoint(new Point(10, 20));
        //trace('p=============' + p);
        assertTrue(p.equals(new Point(152, 88)));
    }

    public function testConvert():void {
        var m:Matrix3d = new Matrix3d();

        myMatrix = m;
        myMatrix.setElement(0, 0, 0);
        myMatrix.setElement(0, 1, 1);
        myMatrix.setElement(0, 2, 2);
        myMatrix.setElement(1, 0, 10);
        myMatrix.setElement(1, 1, 11);
        myMatrix.setElement(1, 2, 12);
        myMatrix.setElement(2, 0, 20);
        myMatrix.setElement(2, 1, 21);
        myMatrix.setElement(2, 2, 22);

        //trace(m.getWithUVWEqual001().toString());
        //assertEquals('(a=10, b=0, c=0, d=0, tx=0, ty=0)', m.getWithUVWEqual001().toString());
        var fm:Matrix = m.getWithUVWEqual001();
        assertTrue(fm.a, Utils.equal(fm.a, 0));
        assertTrue(fm.b, Utils.equal(fm.b, 10));
        assertTrue(fm.c, Utils.equal(fm.c, 1));
        assertTrue(fm.d, Utils.equal(fm.d, 11));
        assertTrue(fm.tx, Utils.equal(fm.tx, 2));
        assertTrue(fm.ty, Utils.equal(fm.ty, 12));

        m = Matrix3d.createFromFlashMatrix(fm);
        assertTrue(m.getElement(0, 0), Utils.equal(m.getElement(0, 0), 0));
        assertTrue(m.getElement(0, 1), Utils.equal(m.getElement(0, 1), 1));
        assertTrue(m.getElement(0, 2), Utils.equal(m.getElement(0, 2), 2));
        assertTrue(m.getElement(1, 0), Utils.equal(m.getElement(1, 0), 10));
        assertTrue(m.getElement(1, 1), Utils.equal(m.getElement(1, 1), 11));
        assertTrue(m.getElement(1, 2), Utils.equal(m.getElement(1, 2), 12));
        assertTrue(m.getElement(2, 0), Utils.equal(m.getElement(2, 0), 0));
        assertTrue(m.getElement(2, 1), Utils.equal(m.getElement(2, 1), 0));
        assertTrue(m.getElement(2, 2), Utils.equal(m.getElement(2, 2), 1));

        fm = new Matrix(3, 4, 6, 2, 2, 8);
        assertTrue(fm.a, Utils.equal(fm.a, 3));
        assertTrue(fm.b, Utils.equal(fm.b, 4));
        assertTrue(fm.c, Utils.equal(fm.c, 6));
        assertTrue(fm.d, Utils.equal(fm.d, 2));
        assertTrue(fm.tx, Utils.equal(fm.tx, 2));
        assertTrue(fm.ty, Utils.equal(fm.ty, 8));

        fm = new Matrix(3, 4, 6, 2, 2, 8);
        myMatrix = m
        myMatrix.setElement(0, 0, 3);
        myMatrix.setElement(0, 1, 6);
        myMatrix.setElement(0, 2, 2);
        myMatrix.setElement(1, 0, 4);
        myMatrix.setElement(1, 1, 2);
        myMatrix.setElement(1, 2, 8);
        myMatrix.setElement(2, 0, 0);
        myMatrix.setElement(2, 1, 0);
        myMatrix.setElement(2, 2, 1);
        assertTrue(m.equals(Matrix3d.createFromFlashMatrix(fm)));

        fm = new Matrix(1, 0, 0, 1, 0, 0);
        var p:Point = fm.transformPoint(new Point(22, 33));
        //trace(fm);
        //trace(p);
        var v:Vector2d =
                Matrix3d.createFromFlashMatrix(fm).vector2dWithZEqualOneProduct(new Vector2d(22, 33));
        assertTrue(v.myX + ' ' + p.x, Utils.equal(v.myX, p.x));
        assertTrue(v.myY + ' ' + p.y, Utils.equal(v.myY, p.y));

        fm = new Matrix(1, 0, 0, 1, 100, 200);
        p = fm.transformPoint(new Point(22, 33));
        //trace(fm);
        //trace(p);
        v = Matrix3d.createFromFlashMatrix(fm).vector2dWithZEqualOneProduct(new Vector2d(22, 33));
        assertTrue(v.myX + ' ' + p.x, Utils.equal(v.myX, p.x));
        assertTrue(v.myY + ' ' + p.y, Utils.equal(v.myY, p.y));

        fm = new Matrix(1, 1, 1, 1, 100, 200);
        p = fm.transformPoint(new Point(22, 33));
        //trace(fm);
        //trace(p);
        v = Matrix3d.createFromFlashMatrix(fm).vector2dWithZEqualOneProduct(new Vector2d(22, 33));
        assertTrue(v.myX + ' ' + p.x, Utils.equal(v.myX, p.x));
        assertTrue(v.myY + ' ' + p.y, Utils.equal(v.myY, p.y));

        fm = new Matrix();
        //fm.translate(10, 20);
        fm.rotate(Math.PI / 2);
        m = Matrix3d.createFromFlashMatrix(fm);
        //fm.scale(3, 39);
        p = fm.transformPoint(new Point(22, 33));
        v = m.vector2dWithZEqualOneProduct(new Vector2d(22, 33));
        //trace(fm);
        //trace(p);
        //trace(m);
        //trace(v);
        assertTrue(v.myX + ' ' + p.x, Utils.equal(v.myX, p.x));
        assertTrue(v.myY + ' ' + p.y, Utils.equal(v.myY, p.y));

        fm = new Matrix(3, 4, 6, 2, 2, 8);
        p = fm.transformPoint(new Point(22, 33));
        //trace(fm);
        //trace(p);
        v = Matrix3d.createFromFlashMatrix(fm).vector2dWithZEqualOneProduct(new Vector2d(22, 33));
        assertTrue(v.myX + ' ' + p.x, Utils.equal(v.myX, p.x));
        assertTrue(v.myY + ' ' + p.y, Utils.equal(v.myY, p.y));

    }

    public function testTransposition():void {
        var res:Matrix3d;
        var m:Matrix3d = new Matrix3d();

        myMatrix = m
        myMatrix.setElement(0, 0, 0);
        myMatrix.setElement(0, 1, 1);
        myMatrix.setElement(0, 2, 2);
        myMatrix.setElement(1, 0, 10);
        myMatrix.setElement(1, 1, 11);
        myMatrix.setElement(1, 2, 12);
        myMatrix.setElement(2, 0, 20);
        myMatrix.setElement(2, 1, 21);
        myMatrix.setElement(2, 2, 22);

        res = m.transposition();
        assertTrue(res.getElement(0, 0), Utils.equal(res.getElement(0, 0), 0));
        assertTrue(res.getElement(0, 1), Utils.equal(res.getElement(0, 1), 10));
        assertTrue(res.getElement(0, 2), Utils.equal(res.getElement(0, 2), 20));
        assertTrue(res.getElement(1, 0), Utils.equal(res.getElement(1, 0), 1));
        assertTrue(res.getElement(1, 1), Utils.equal(res.getElement(1, 1), 11));
        assertTrue(res.getElement(1, 2), Utils.equal(res.getElement(1, 2), 21));
        assertTrue(res.getElement(2, 0), Utils.equal(res.getElement(2, 0), 2));
        assertTrue(res.getElement(2, 1), Utils.equal(res.getElement(2, 1), 12));
        assertTrue(res.getElement(2, 2), Utils.equal(res.getElement(2, 2), 22));
    }

    public function testRowProduct():void {
        var m:Matrix3d = new Matrix3d();

        myMatrix = m
        myMatrix.setElement(0, 0, 0);
        myMatrix.setElement(0, 1, 1);
        myMatrix.setElement(0, 2, 2);
        myMatrix.setElement(1, 0, 10);
        myMatrix.setElement(1, 1, 11);
        myMatrix.setElement(1, 2, 12);
        myMatrix.setElement(2, 0, 20);
        myMatrix.setElement(2, 1, 21);
        myMatrix.setElement(2, 2, 22);

        var v:Vector2d = m.Row2dProduct(new Vector2d(3, 2));
        assertTrue(v, v.equals(new Vector2d(40, 46)));

    }

    public function testTranslation():void {
        var m:Matrix3d = new Matrix3d();
        m.setElement(0, 0, 1);
        m.setElement(0, 1, 0);
        m.setElement(0, 2, 1);
        m.setElement(1, 0, 0);
        m.setElement(1, 1, 1);
        m.setElement(1, 2, 2);
        m.setElement(2, 0, 0);
        m.setElement(2, 1, 0);
        m.setElement(2, 2, 1);
        var res:Matrix3d = Matrix3d.translation(new Vector2d(1, 2));
        assertTrue(res, m.equals(res));
        var resV:Vector2d = res.vector2dWithZEqualOneProduct(new Vector2d(0, 0));
        assertTrue(resV, resV.equals(new Vector2d(1, 2)));
    }

    public function testStreching():void {
        var m:Matrix3d = new Matrix3d();
        m.setElement(0, 0, 2);
        m.setElement(0, 1, 0);
        m.setElement(0, 2, 0);
        m.setElement(1, 0, 0);
        m.setElement(1, 1, 3);
        m.setElement(1, 2, 0);
        m.setElement(2, 0, 0);
        m.setElement(2, 1, 0);
        m.setElement(2, 2, 1);
        var res:Matrix3d = Matrix3d.stretching(2, 3);
        assertTrue(res, m.equals(res));
        var resV:Vector2d = res.vector2dWithZEqualOneProduct(new Vector2d(4, 5));
        assertTrue(resV, resV.equals(new Vector2d(8, 15)));
    }

    public function testRotating():void {
        var m:Matrix3d = new Matrix3d();
        m.setElement(0, 0, 0);
        m.setElement(0, 1, -1);
        m.setElement(0, 2, 0);
        m.setElement(1, 0, 1);
        m.setElement(1, 1, 0);
        m.setElement(1, 2, 0);
        m.setElement(2, 0, 0);
        m.setElement(2, 1, 0);
        m.setElement(2, 2, 1);
        var res:Matrix3d = Matrix3d.rotation(Math.PI / 2);
        assertTrue(res, m.equals(res));
        var resV:Vector2d = res.vector2dWithZEqualOneProduct(new Vector2d(1, 0));
        assertTrue(resV, resV.equals(new Vector2d(0, 1)));
        resV = res.vector2dWithZEqualOneProduct(new Vector2d(1, 1));
        assertTrue(resV, resV.equals(new Vector2d(-1, 1)));

        var rotateMatrix:Matrix3d = Matrix3d.rotation(- (new Ort2d(1)).getAngle() + Math.PI / 2);
        var angle:Number = rotateMatrix.vector2dWithZEqualOneProduct(new Ort2d(1)).getAngle();
        assertTrue(angle, Utils.equal(
                rotateMatrix.vector2dWithZEqualOneProduct(new Ort2d(1)).getAngle(),
                Math.PI / 2
                ));
    }

    public function test2dProduct():void {
        var m:Matrix3d = new Matrix3d();
        m.setElement(0, 0, 0);
        m.setElement(0, 1, -1);
        m.setElement(0, 2, 0);
        m.setElement(1, 0, 1);
        m.setElement(1, 1, 0);
        m.setElement(1, 2, 0);
        m.setElement(2, 0, 0);
        m.setElement(2, 1, 0);
        m.setElement(2, 2, 1);
        var res:Vector2d = m.vector2dWithZEqualOneProduct(new Vector2d(1, 2));
        assertTrue(res, res.equals(new Vector2d(-2, 1)));
    }


}
}