package com.turbostool.client.utils.test
{

import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;

public class Vector3dTest extends TestCase {
    public function Vector3dTest(methodName:String = null) {
        super(methodName);
    }

    public function testVector3d():void {
        var v:Vector3d = new Vector3d(1, 2, 3);
        assertEquals(1, v.myX);
        assertEquals(2, v.myY);
        assertEquals(3, v.myZ);

        //test sum

        var v1:Vector3d = new Vector3d(1, 2, 3);
        var v2:Vector3d = new Vector3d(6, 7, 8);
        v = v1.sum(v2);
        assertEquals('expected 7 but found ' + v.myX, 7, v.myX);
        assertEquals(9, v.myY);
        assertEquals(11, v.myZ);

        //test numberProduct

        v1 = new Vector3d(1, 2, 3);
        var n:Number = 3;
        v = v1.numberProduct(n);
        assertEquals(3, v.myX);
        assertEquals(6, v.myY);
        assertEquals(9, v.myZ);

        //test scalarProduct

        v1 = new Vector3d(1, 2, 3);
        v2 = new Vector3d(6, 7, 8);
        n = 44;
        assertEquals(n, v1.scalarProduct(v2));

        //test length

        v1 = new Vector3d(1, 2, 3);
        assertTrue(Utils.equal(v1.length(), 3.7416573867739413855837487323165));

        //test difference

        v1 = new Vector3d(1, 2, 3);
        v2 = new Vector3d(6, 7, 8);
        v = v1.difference(v2);
        assertEquals(-5, v.myX);
        assertEquals(-5, v.myY);
        assertEquals(-5, v.myZ);

        //test vectorProduct

        v1 = new Vector3d(1, 2, 3);
        v2 = new Vector3d(6, 7, 8);
        var res:Vector3d = new Vector3d(-5, 10, -5);
        assertTrue(res.equals(v1.vectorProduct(v2)), res.toString() + ' <> ' + v1.vectorProduct(v2));

        assertTrue(Utils.equal(6, v2.getCoordinate(0)));
        assertTrue(Utils.equal(7, v2.getCoordinate(1)));
        assertTrue(Utils.equal(8, v2.getCoordinate(2)));
        v2.setCoordinate(0, 9);
        v2.setCoordinate(1, 10);
        v2.setCoordinate(2, 11);
        assertTrue(Utils.equal(9, v2.getCoordinate(0)));
        assertTrue(Utils.equal(10, v2.getCoordinate(1)));
        assertTrue(Utils.equal(11, v2.getCoordinate(2)));
    }

    public function testEquals():void {
        var v:Vector3d;
        var v1:Vector3d;
        var v2:Vector3d;

        v = new Vector3d(1, 2, 3);
        v1 = new Vector3d(1, 2, 3);
        v2 = new Vector3d(1, 2, 3 + Utils.EPSILON / 2);
        assertTrue(v.equals(v2));
        v2 = new Vector3d(1, 2, 3 + Utils.EPSILON * 2);
        assertFalse(v.equals(v2));
        assertTrue(v.equals(v1));
        assertFalse(v.equals(null));
        assertFalse(v.equals(this));
        assertFalse(v.equals(v.sum(v1)));
    }

    public function testProjection():void {
        var v:Vector3d;
        v = (new Vector3d(1, 2, 3)).projection(new Vector3d(0, -10, 0));
        assertTrue(v, v.equals(new Vector3d(0, 2, 0)));
        v = (new Vector3d(1, 2, 3)).getOrtogComponentBy(new Vector3d(0, -10, 0));
        assertTrue(v, v.equals(new Vector3d(1, 0, 3)));
        var n:Number = (new Vector3d(1, 2, 3)).scalarProjection(new Vector3d(0, -10, 0));
        assertTrue(n, Utils.equal(-2, n));
    }

    public function testMultiply():void {
        var v:Vector3d = new Vector3d(1, 2, 3);
        assertTrue(v.multiply(2).equals(new Vector3d(2, 4, 6)));
    }

}
}