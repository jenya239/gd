package com.turbostool.client.geom.test
{

import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;

public class PolygonTest extends TestCase
{
    public function PolygonTest(methodName:String = null) {
        super(methodName);
    }

    public function testPolygon():void {
        var p:Polygon = new Polygon();
        assertTrue((p as Vector2dSequence) is Polygon);
        p.myInsideActive = false;
        var pc:Polygon = p.clone() as Polygon;
        assertFalse(pc.myInsideActive);
    }

    public function testInside():void {
        var r1:Vector2d = new Vector2d(10, 10);
        var r2:Vector2d = new Vector2d(10, -20);
        var r3:Vector2d = new Vector2d(-30, -20);
        var r4:Vector2d = new Vector2d(-30, 10);
        var pol:Polygon = new Polygon();
        pol.addVertex(r1);
        pol.addVertex(r2);
        pol.addVertex(r3);
        pol.addVertex(r4);

        assertTrue(pol.isInside(new Vector2d(0, 0)));
        assertTrue(pol.isInside(new Vector2d(10, 10)));
        assertFalse(pol.isInside(new Vector2d(-100, -100)));
        assertFalse(pol.isInside(pol.getOutsidePoint()));
    }

    public function testParallel():void {
        var pol1:Polygon = new Polygon();
        pol1.addVertex(new Vector2d(0, 0));
        pol1.addVertex(new Vector2d(10, 0));
        pol1.addVertex(new Vector2d(10, -10));
        pol1.addVertex(new Vector2d(0, -10));

        var pol2:Polygon = new Polygon();
        pol2.addVertex(new Vector2d(-5, 5));
        pol2.addVertex(new Vector2d(15, 5));
        pol2.addVertex(new Vector2d(15, -15));
        pol2.addVertex(new Vector2d(-5, -15));

        assertTrue(pol2.equals(pol1.createParallel(5)));
    }

    public function testEquals():void {
        var pol1:Polygon = new Polygon();
        var pol2:Polygon = new Polygon(false);
        var pol3:Polygon = new Polygon(false);
        var pol4:Polygon = new Polygon();
        pol1.addVertex(new Vector2d(10, 20));
        pol2.addVertex(new Vector2d(10, 20));
        pol3.addVertex(new Vector2d(11, 20));

        assertFalse(pol1.equals(pol2));
        assertFalse(pol1.equals(pol3));
        assertFalse(pol1.equals(pol4));
        assertFalse(pol2.equals(pol3));
        assertFalse(pol2.equals(pol4));
        pol2.copyTo(pol4);
        assertTrue(pol4.equals(pol2));
    }

    public function testGlobal():void {
        var pol:Polygon = new Polygon(false);
        pol.addVertex(new Vector2d(10, 9));
        pol.addVertex(new Vector2d(100, 90));
        pol.addVertex(new Vector2d(200, 100));
        var frame:LocalFrame2d = new LocalFrame2d(new Vector2d(9, 9), Math.PI / 2);
        var test:Polygon = new Polygon(false);
        test.addVertex(new Vector2d(0, 19));
        test.addVertex(new Vector2d(-81, 109));
        test.addVertex(new Vector2d(-91, 209));
        assertTrue(pol.createGlobal(frame) is Polygon);
        var res:Polygon = pol.createGlobal(frame) as Polygon;
        assertFalse(pol.equals(res));
        assertTrue(res, res.equals(test));
    }


}
}