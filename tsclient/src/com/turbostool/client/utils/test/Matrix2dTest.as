package com.turbostool.client.utils.test {

import com.turbostool.client.utils.Matrix2d;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;

public class Matrix2dTest extends TestCase     {
    public function Matrix2dTest(methodName:String = null) {
        super(methodName);
    }

    public function testMatrix2d():void {
        var m:Matrix2d = new Matrix2d(1, 2, 3, 4);
        assertEquals(1, m.myA11);
        assertEquals(2, m.myA12);
        assertEquals(3, m.myA21);
        assertEquals(4, m.myA22);
        assertEquals(-2, m.determinant());
    }

    public function testEquals():void {
        var m1:Matrix2d = new Matrix2d(1, 2, 3, 4);
        var m2:Matrix2d = new Matrix2d();
        m2.myA11 = 1;
        m2.myA12 = 2;
        m2.myA21 = 3;
        m2.myA22 = 4;
        assertTrue(m1.equals(m2));
        assertFalse(m1.equals(new Matrix2d()));
        assertFalse(m1.equals(new Vector2d(1, 2)));
        assertFalse(m1.equals(null));
    }

}
}