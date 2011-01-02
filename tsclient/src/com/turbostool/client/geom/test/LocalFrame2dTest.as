package com.turbostool.client.geom.test
{

import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;

;

public class LocalFrame2dTest extends TestCase
{
    public function LocalFrame2dTest(methodName:String = null) {
        super(methodName);
    }

    public function testLocalFrame2d():void {
        var global:Vector2d = new Vector2d(10, 20);
        var frameR:Vector2d = new Vector2d(-5, 30);
        var local:Vector2d = new Vector2d(15, -10);
        var lf:LocalFrame2d = new LocalFrame2d(frameR);
        assertTrue(global.equals(lf.getGlobal(local)));
        assertTrue(local.equals(lf.getLocal(global)));

        lf.myAngle = Math.PI / 2;
        local = new Vector2d(4, 8);
        global = new Vector2d(-13, 34);
        assertTrue(global.equals(lf.getGlobal(local)));
        assertTrue(local.equals(lf.getLocal(global)));
    }

    public function testSuperposition():void {
        var lf1:LocalFrame2d = new LocalFrame2d(new Vector2d(10, 9), Math.PI);
        var lf2:LocalFrame2d = new LocalFrame2d(new Vector2d(-10, 3), -Math.PI);
        var res:LocalFrame2d = lf1.superposition(lf2);
        assertTrue(res.myR.equals(new Vector2d(20, 6)));
        assertTrue(Utils.equal(res.myAngle, 0));
    }

    public function testCopyTo():void {
        var lf1:LocalFrame2d = new LocalFrame2d(new Vector2d(10, 2), 3);
        var lf2:LocalFrame2d = new LocalFrame2d(new Vector2d(102, 2), 32);
        assertFalse(Utils.equal(lf1.myAngle, lf2.myAngle));
        assertFalse(lf1.myR.equals(lf2.myR));
        lf1.copyTo(lf2);
        assertTrue(Utils.equal(lf1.myAngle, lf2.myAngle));
        assertTrue(lf1.myR.equals(lf2.myR));
    }

    public function testEquals():void {
        var lf1:LocalFrame2d = new LocalFrame2d(new Vector2d(10, 2), 3);
        var lf2:LocalFrame2d = new LocalFrame2d(new Vector2d(10, 2), 32);
        var lf3:LocalFrame2d = new LocalFrame2d(new Vector2d(102, 2), 3);
        var lf4:LocalFrame2d = new LocalFrame2d(new Vector2d(102, 2), 32);
        var lf5:LocalFrame2d = new LocalFrame2d(new Vector2d(10, 2), 3);
        assertFalse(lf1.equals(lf2));
        assertFalse(lf1.equals(lf3));
        assertFalse(lf1.equals(lf4));
        assertTrue(lf1.equals(lf1));
        lf1.copyTo(lf2);
        assertTrue(lf1.equals(lf5));
        assertTrue(lf5.equals(lf1));
    }

    public function testOrt():void {
        var lf:LocalFrame2d = new LocalFrame2d(new Vector2d(33, 44), Math.PI / 2);
        assertTrue(lf.getXOrt().equals(new Vector3d(0, 1, 0)));
        assertTrue(lf.getYOrt().equals(new Vector3d(-1, 0, 0)));
    }

    public function testRelative():void {
        var lf1:LocalFrame2d = new LocalFrame2d(new Vector2d(10, 9), Math.PI / 2);
        var lf2:LocalFrame2d = new LocalFrame2d(new Vector2d(-10, 3), -Math.PI);
        var res:LocalFrame2d = lf2.relativeTo(lf1);
        assertTrue(res.myR, res.myR.equals(new Vector2d(-6, 20)));
        assertTrue(res.myAngle, Utils.equal(res.myAngle, - 3 * Math.PI / 2));
        assertTrue(lf2.equals(lf1.superposition(res)));
    }


}
}