package com.turbostool.client.dynamicEngine.test
{

import com.turbostool.client.dynamicEngine.BodyBorder;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class BodyBorderTest extends TestCase {
    private var myBorder:BodyBorder;
    private var myPiece:Piece2d;

    public function BodyBorderTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new BodyBorderTest("testCanIntersect"));
        return ts;
    }

    override public function setUp():void {
        myBorder = new BodyBorder();
        myPiece = new Piece2d(new Vector2d(0, 0), new Vector2d(2, 2));
    }

    private function checkIntersect(minX:Number, maxX:Number, minY:Number, maxY:Number, intersects:Boolean):void {
        myBorder.myMinX = minX;
        myBorder.myMaxX = maxX;
        myBorder.myMinY = minY;
        myBorder.myMaxY = maxY;
        var msg:String = myBorder.toString();
        msg += (intersects)
                ? 'not'
                : 'can';
        msg += ' intersect ' + myPiece;
        assertEquals(msg, intersects, myBorder.canIntersect(myPiece));
    }

    public function testCanIntersect():void {
        checkIntersect(1, 1, 0, 0, true);
        checkIntersect(3, 3, 3, 3, false);
        checkIntersect(-1, 1, 0, 2, true);
        checkIntersect(-1, 3, -1, 3, true);
        checkIntersect(1, 3, 0, 2, true);
        checkIntersect(3, 5, -3, -1, false);
        checkIntersect(3, 3, 1, 1, false);
        checkIntersect(1, 1, -1, -1, false);
        //checkIntersect(, , , , );
    }

}
}