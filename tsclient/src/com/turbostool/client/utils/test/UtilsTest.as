package com.turbostool.client.utils.test
{

import com.turbostool.client.utils.Matrix2d;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;

public class UtilsTest extends TestCase {
    public function UtilsTest(methodName: String = null) {
        super(methodName);
    }

    public function testGetNear(): void {
        var res:Number = Utils.getNear(10, 8, 13);
        assertTrue(res, Utils.equal(10, res));
        res = Utils.getNear(10, 13, 8);
        assertTrue(res, Utils.equal(10, res));
        res = Utils.getNear(-10, -8, -13);
        assertTrue(res, Utils.equal(-10, res));
        res = Utils.getNear(-10, -13, -8);
        assertTrue(res, Utils.equal(-10, res));
        res = Utils.getNear(10, 11, 13);
        assertTrue(res, Utils.equal(11, res));
        res = Utils.getNear(10, 8, 9);
        assertTrue(res, Utils.equal(9, res));
    }

    public function testSolve2d(): void {
        try {
            Utils.solveSystem2d(new Matrix2d(2, 3, 4, 6), new Vector2d(8, 9));
            fail();
        } catch (e:Error) {
            assertTrue(e is TSError);
        }
        var res:Vector2d = Utils.solveSystem2d(new Matrix2d(4, 6, -1, 3), new Vector2d(160, 50));
        assertTrue(res, res.equals(new Vector2d(10, 20)));
    }

    public function testIsTurning(): void {
        assertTrue(Utils.isTurning(10, 8, 11, -1));
        assertTrue(Utils.isTurning(6, 8, 1, 10));
        assertTrue(Utils.isTurning(10, 10, 11, -1));
        assertFalse(Utils.isTurning(10, 10, 10, -1));
        assertFalse(Utils.isTurning(10, 10, 10, 1.11));
        assertTrue(Utils.isTurning(10, 8, 10, -1));

    }

    public function testScalarProduct(): void {
        assertEquals(32, Utils.scalarProduct(new Array(1, 2, 3), new Array(4, 5, 6)));
    }

    public function testPow2():void {
        assertEquals(1, Utils.pow2(0));
        assertEquals(2, Utils.pow2(1));
        assertEquals(4, Utils.pow2(2));
        assertEquals(8, Utils.pow2(3));
        assertEquals(16, Utils.pow2(4));
    }

    public function testGiperIntersects(): void {
        assertTrue(Utils.giperIntersects([2], 4, [3]));
        assertFalse(Utils.giperIntersects([2], 4, [1]));
        assertTrue(Utils.giperIntersects([2], 4, [2]));

        assertTrue(Utils.giperIntersects([6, 3], 9, [1, 2]));
        assertTrue(Utils.giperIntersects([-6, 3], 9, [1, 2]));
        assertTrue(Utils.giperIntersects([6, -3], 9, [1, 2]));
        assertTrue(Utils.giperIntersects([-6, -3], 9, [1, 2]));
        assertTrue(Utils.giperIntersects([6, 3], -9, [1, 2]));
        assertTrue(Utils.giperIntersects([-6, 3], -9, [1, 2]));
        assertTrue(Utils.giperIntersects([6, -3], -9, [1, 2]));
        assertTrue(Utils.giperIntersects([-6, -3], -9, [1, 2]));

        assertTrue(Utils.giperIntersects([0, 4], -8, [1, 2]));
        assertTrue(Utils.giperIntersects([1, 4], 0, [1, 2]));
        assertTrue(Utils.giperIntersects([0, 0], 0, [1, 2]));
        assertFalse(Utils.giperIntersects([0, 4], -9, [1, 2]));
        assertFalse(Utils.giperIntersects([1, 2], -88, [1, 2]));
        assertFalse(Utils.giperIntersects([0.01, 1], 2.1, [1, 2]));

        assertTrue(Utils.giperIntersects([0, 0, 0], 0, [1, 2, 3]));
        assertTrue(Utils.giperIntersects([20, 30, 60], 0, [1, 2, 3]));
        assertTrue(Utils.giperIntersects([0, 0, 1], 2, [1, 2, 3]));
        assertFalse(Utils.giperIntersects([0, 0, 1], 4, [1, 2, 3]));
        assertFalse(Utils.giperIntersects([-1, 1.1, 1], 10, [1, 2, 3]));
    }

    public function testBumpless(): void {
        var res:Array;
        res = Utils.getBumpless(2, 3, 4, 5, 5);
        assertTrue(Utils.equal(3, res[0]));
        assertTrue(Utils.equal(0, res[1]));
    }

    public function testStringStartsWith(): void {
        assertTrue(Utils.stringStartsWith("abcd", ""));
        assertTrue(Utils.stringStartsWith("abcd", "a"));
        assertTrue(Utils.stringStartsWith("abcd", "ab"));
        assertTrue(Utils.stringStartsWith("abcd", "abcd"));

        assertFalse(Utils.stringStartsWith("abcd", "bcd"));
        assertFalse(Utils.stringStartsWith("abcd", "abcde"));
    }

    public function testStringFormat(): void {
        assertEquals("", Utils.stringFormat(""));
        assertEquals("a", Utils.stringFormat("a"));
        assertEquals("abc", Utils.stringFormat("a{0}c", "b"));
        assertEquals("abcde", Utils.stringFormat("a{0}c{1}e", "b", "d"));
        assertEquals("abcde", Utils.stringFormat("a{1}c{0}e", "d", "b"));
        assertEquals("abc{1}e", Utils.stringFormat("a{0}c{1}e", "b"));
    }

}
}