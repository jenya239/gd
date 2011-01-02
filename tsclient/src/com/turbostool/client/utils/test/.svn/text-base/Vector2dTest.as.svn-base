package com.turbostool.client.utils.test
{
import com.turbostool.client.utils.*;

import mx.controls.Alert;

;

public class Vector2dTest extends TSTestCase
{
    public function Vector2dTest(methodName:String = null) {
        super(methodName);
    }

    public function testVector2d():void {
        var v:Vector2d, v1:Vector2d, v2:Vector2d;
        var v3d:Vector3d;
        /*
         var catched:Boolean = false;
         try {
         v = new Vector2d(1, 2, 3);
         } catch (error:TSError) {
         //Alert.show(error.toString());
         catched = true;
         }
         assertTrue(catched);


         catched = false;
         try {
         v = new Vector2d(1);
         } catch (error:TSError) {
         //Alert.show(error.toString());
         catched = true;
         }
         assertTrue(catched);
         */
        try {
            v = new Vector2d(2, 3);
            //Alert.show(v.toString());
            assertEquals(2, v.myX);
            assertEquals(3, v.myY);
            assertEquals(0, v.myZ);
        }
        catch (e:Error) {
            Alert.show(e.toString());
        }

        v3d = new Vector3d(6, 7, 8);
        v = Vector2d.createFrom3d(v3d);
        assertEquals(6, v.myX);
        assertEquals(7, v.myY);
        assertEquals(0, v.myZ);
        /*v = new Vector2d(v3d);
         assertEquals(6, v.myX);
         assertEquals(7, v.myY);
         assertEquals(0, v.myZ);
         */
        v2 = v.sum2d(v);
        //Alert.show(v2.toString());
        assertTrue(v2 is Vector2d);
        assertEquals(12, v2.myX);
        assertEquals(14, v2.myY);
        assertEquals(0, v2.myZ);

        v = new Vector2d(2, 5.5);
        v2 = new Vector2d(4, 11);
        assertTrue((v.numberProduct2d(2)).equals(v2));
        assertTrue((v.sum2d(v)).equals(v2));

    }

    public function testParseTo2dVector():void {
        var v:Vector2d;
        v = Vector2d.parseTo2dVector('22 1');
        assertTrue(v.equals(new Vector2d(22, 1)));
        //Alert.show(v.toString());
        var catched:Boolean = false;
        try {
            v = Vector2d.parseTo2dVector('2d 1');
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);

        catched = false;
        try {
            v = Vector2d.parseTo2dVector('244 1 44');
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);
    }

    public function testRotated():void {
        var v:Vector2d;
        var r:Vector2d;
        v = new Vector2d(15, 20);
        r = new Vector2d(20, -15);
        assertTrue(r.equals(v.getRotated(3 * Math.PI / 2)));
    }

    public function testAngle():void {
        var n:Number;
        n = (new Vector2d(1, 1)).getAngle();
        assertTrue(n, Utils.equal(Math.PI / 4, n));
        n = (new Vector2d(-1, -1)).getAngle();
        assertTrue(n, Utils.equal(- 3 * Math.PI / 4, n));
        n = (new Vector2d(1, 0)).getAngle();
        assertTrue(n, Utils.equal(0, n));
    }

    public function testGetScalarOrtogonal():void {
        var v:Vector2d = new Vector2d(1, -3);
        var base:Vector2d = new Vector2d(2, 0);
        assertTrue(v.getScalarOrtogonal(base), Utils.equal(-3, v.getScalarOrtogonal(base)));
    }

    public function testRotate():void {
        var v:Vector2d = new Vector2d(1, 0);
        var expected:Vector2d = new Vector2d(0, 1);
        var result :Vector2d = v.rotate(Math.PI / 2);
        assertTrue(result, expected.equals(result));

    }

    public function testGetScalarProjectionByAngle():void {
        var v:Vector2d = new Vector2d(1, 0);
        var res:Number = v.getScalarProjectionByAngle(Math.PI / 2);
        assertTrue(res, Utils.equal(0, res));
        res = v.getScalarProjectionByAngle(0);
        assertTrue(res, Utils.equal(1, res));
        res = v.getScalarProjectionByAngle(Math.PI / 4);
        assertTrue(res, Utils.equal(Math.SQRT1_2, res));
    }

    public function testSetProjectiontoAngle():void {
        var v:Vector2d = new Vector2d(1, 0);
        v.setProjectionToAngle(Math.PI / 4);
        assertTrue(v, v.equals(new Vector2d(0.5, 0.5)));
        v.setXY(1, 0);
        v.setProjectionToAngle(5 * Math.PI / 4);
        assertTrue(v, v.equals(new Vector2d(0.5, 0.5)));
        v.setXY(1, 0);
        v.setProjectionToAngle(Math.PI);
        assertTrue(v, v.equals(new Vector2d(1, 0)));
    }

    public function testAddMultiplied():void {
        assertTrue(new Vector2d(2, 5).addMultiplied(new Vector2d(3, 6), 2).equals(new Vector2d(8, 17)));
    }

    public function testGetOrtComponentBy():void {
        var temp:Vector2d = Vector2d.getZero();
        var v:Vector2d = new Vector2d(1, 0);
        var base:Vector2d = new Vector2d(0, 1);
        assertTrue("1" + v.getOrtComponentBy(base, temp), ( new Vector2d(1, 0) ).equals(v.getOrtComponentBy(base, temp)));
        temp.setZero();
        v.setXY(1, 0);
        base.setXY(1, 0);
        assertTrue("2" + v.getOrtComponentBy(base, temp), ( new Vector2d(0, 0) ).equals(v.getOrtComponentBy(base, temp)));
        temp.setZero();
        v.setXY(3, 1);
        base.setXY(-5, 0);
        assertTrue("3" + v.getOrtComponentBy(base, temp), ( new Vector2d(0, 1) ).equals(v.getOrtComponentBy(base, temp)));
        temp.setZero();
        v.setXY(3, 1);
        base.setXY(0, 5);
        assertTrue("4" + v.getOrtComponentBy(base, temp), ( new Vector2d(3, 0) ).equals(v.getOrtComponentBy(base, temp)));
    }

    public function testSetProjectionToVector():void {
        var temp:Vector2d = new Vector2d(0, 1);
        var v:Vector2d = new Vector2d(1, 0);
        var result:Vector2d = v.setProjectionToVector(temp);
        assertTrue("1" + result, result.equals(new Vector2d(0, 0)));
    }

}
}