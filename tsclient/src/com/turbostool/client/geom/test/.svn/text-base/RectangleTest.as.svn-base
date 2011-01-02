package com.turbostool.client.geom.test
{

import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.geom.Rectangle;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.TSError;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class RectangleTest extends TestCase
{
    public function RectangleTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new RectangleTest("testRectangle"));
        ts.addTest(new RectangleTest("testParallel"));
        ts.addTest(new RectangleTest("testEquals"));
        ts.addTest(new RectangleTest("testGlobal"));
        return ts;
    }

    public function testRectangle():void {
        var rect:Rectangle = new Rectangle(new Vector2d(5, 6), 2, 3, false);
        assertTrue(rect is Polygon);
        assertFalse(rect.myInsideActive);
        assertTrue(rect.myR, rect.myR.equals(new Vector2d(5, 6)));
        assertTrue(rect.myWidth, Utils.equal(rect.myWidth, 2));
        assertTrue(rect.myHeight, Utils.equal(rect.myHeight, 3));
        var catched:Boolean;
        catched = false;
        try {
            rect.addVertex(new Vector2d(0, 2));
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);
        assertEquals(4, rect.getVertices().length());
        assertTrue(rect.getVertices().getItemAt(0),
                (rect.getVertices().getItemAt(0) as Vector2d).equals(new Vector2d(4, 4.5))
                );
        assertTrue(rect.getVertices().getItemAt(1),
                (rect.getVertices().getItemAt(1) as Vector2d).equals(new Vector2d(6, 4.5))
                );
        assertTrue(rect.getVertices().getItemAt(2),
                (rect.getVertices().getItemAt(2) as Vector2d).equals(new Vector2d(6, 7.5))
                );
        assertTrue(rect.getVertices().getItemAt(3),
                (rect.getVertices().getItemAt(3) as Vector2d).equals(new Vector2d(4, 7.5))
                );
        catched = false;
        try {
            rect.removeAllVertices();
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);
        assertTrue(rect.myR, rect.myR.equals(new Vector2d(5, 6)));
        assertTrue(rect.myWidth, Utils.equal(rect.myWidth, 2));
        assertTrue(rect.myHeight, Utils.equal(rect.myHeight, 3));

        rect.myR = new Vector2d(15, 26);
        assertEquals(4, rect.getVertices().length());
        assertTrue(rect.getVertices().getItemAt(0),
                (rect.getVertices().getItemAt(0) as Vector2d).equals(new Vector2d(14, 24.5))
                );
        assertTrue(rect.getVertices().getItemAt(1),
                (rect.getVertices().getItemAt(1) as Vector2d).equals(new Vector2d(16, 24.5))
                );
        assertTrue(rect.getVertices().getItemAt(2),
                (rect.getVertices().getItemAt(2) as Vector2d).equals(new Vector2d(16, 27.5))
                );
        assertTrue(rect.getVertices().getItemAt(3),
                (rect.getVertices().getItemAt(3) as Vector2d).equals(new Vector2d(14, 27.5))
                );
        assertTrue(rect.myR, rect.myR.equals(new Vector2d(15, 26)));
        assertTrue(rect.myWidth, Utils.equal(rect.myWidth, 2));
        assertTrue(rect.myHeight, Utils.equal(rect.myHeight, 3));

        rect.myR = new Vector2d(5, 6);
        rect.myWidth = 22
        assertEquals(4, rect.getVertices().length());
        assertTrue(rect.getVertices().getItemAt(0),
                (rect.getVertices().getItemAt(0) as Vector2d).equals(new Vector2d(-6, 4.5))
                );
        assertTrue(rect.getVertices().getItemAt(1),
                (rect.getVertices().getItemAt(1) as Vector2d).equals(new Vector2d(16, 4.5))
                );
        assertTrue(rect.getVertices().getItemAt(2),
                (rect.getVertices().getItemAt(2) as Vector2d).equals(new Vector2d(16, 7.5))
                );
        assertTrue(rect.getVertices().getItemAt(3),
                (rect.getVertices().getItemAt(3) as Vector2d).equals(new Vector2d(-6, 7.5))
                );
        assertTrue(rect.myR, rect.myR.equals(new Vector2d(5, 6)));
        assertTrue(rect.myWidth, Utils.equal(rect.myWidth, 22));
        assertTrue(rect.myHeight, Utils.equal(rect.myHeight, 3));

        rect.myR = new Vector2d(5, 6);
        rect.myWidth = 2
        rect.myHeight = 43
        assertEquals(4, rect.getVertices().length());
        assertTrue(rect.getVertices().getItemAt(0),
                (rect.getVertices().getItemAt(0) as Vector2d).equals(new Vector2d(4, -15.5))
                );
        assertTrue(rect.getVertices().getItemAt(1),
                (rect.getVertices().getItemAt(1) as Vector2d).equals(new Vector2d(6, -15.5))
                );
        assertTrue(rect.getVertices().getItemAt(2),
                (rect.getVertices().getItemAt(2) as Vector2d).equals(new Vector2d(6, 27.5))
                );
        assertTrue(rect.getVertices().getItemAt(3),
                (rect.getVertices().getItemAt(3) as Vector2d).equals(new Vector2d(4, 27.5))
                );
    }

    public function testEquals():void {
        var rect1:Rectangle = new Rectangle(new Vector2d(10, 20), 30, 40);
        var rect2:Rectangle = new Rectangle(new Vector2d(10, 20), 30, 40);
        var pol:Polygon = new Polygon();
        for (var i:int = 0; i < 4; i++) {
            pol.addVertex(rect1.getVertices().getItemAt(i) as Vector2d);
        }
        assertFalse(rect1.equals(pol));
        assertTrue(pol.equals(rect1));/////////?????????????????????????
        assertTrue(rect1.equals(rect2));
        rect2 = new Rectangle(new Vector2d(10, 20), 30, 40, false);
        assertFalse(rect1.equals(rect2));
        rect1.myInsideActive = false;
        assertTrue(rect1.equals(rect2));
        rect1 = new Rectangle(new Vector2d(10, 20), 30, 420);
        rect2 = new Rectangle(new Vector2d(10, 20), 30, 40);
        assertFalse(rect1.equals(rect2));
        rect1 = new Rectangle(new Vector2d(10, 20), 320, 40);
        rect2 = new Rectangle(new Vector2d(10, 20), 30, 40);
        assertFalse(rect1.equals(rect2));
        rect1 = new Rectangle(new Vector2d(10, 210), 30, 40);
        rect2 = new Rectangle(new Vector2d(10, 20), 30, 40);
        assertFalse(rect1.equals(rect2));
        assertTrue(rect1.equals(rect1.clone()));
        assertTrue(rect1.equals(rect1));
        rect1.copyTo(rect2);
        assertTrue(rect1.equals(rect2));
        assertFalse(rect1 == rect2);
    }

    public function testParallel():void {
        var rect1:Rectangle = new Rectangle(new Vector2d(10, 20), 30, 40, false);
        var rect2:Rectangle = rect1.createParallel(10) as Rectangle;
        assertTrue(rect2, rect2.equals(new Rectangle(new Vector2d(10, 20), 50, 60, false)));
    }

    public function testGlobal():void {
        var rect:Rectangle = new Rectangle(new Vector2d(10, 20), 40, 30, false);
        var pol:Polygon = new Polygon(false);
        pol.addVertex(new Vector2d(4, -1));
        pol.addVertex(new Vector2d(4, 39));
        pol.addVertex(new Vector2d(-26, 39));
        pol.addVertex(new Vector2d(-26, -1));
        var frame:LocalFrame2d = new LocalFrame2d(new Vector2d(9, 9), Math.PI / 2);
        assertTrue(rect.createGlobal(frame) is Polygon);
        var res:Polygon = rect.createGlobal(frame) as Polygon;
        assertFalse(rect.equals(res));
        assertTrue(res, res.equals(pol));
    }


}
}