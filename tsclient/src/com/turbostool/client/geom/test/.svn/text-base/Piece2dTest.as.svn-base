package com.turbostool.client.geom.test
{
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.geom.Vector2dKeeper;
import com.turbostool.client.utils.*;

public class Piece2dTest extends TSTestCase
{
    public function Piece2dTest(methodName:String = null) {
        super(methodName);
    }

    public function testPiece2d():void {
        var b:Vector2d = new Vector2d(33, 44);
        var e:Vector2d = new Vector2d(32, 4);
        var piece:Piece2d = new Piece2d(b, e);
        assertTrue(b.equals(piece.myBegin));
        assertTrue(e.equals(piece.myEnd));

        b = new Vector2d(3233, 424);
        e = new Vector2d(332, 224);
        piece.myBegin = b;
        piece.myEnd = e;
        assertTrue(b.equals(piece.myBegin));
        assertTrue(e.equals(piece.myEnd));

    }

    public function testIntersect():void {
        var piece1:Piece2d = new Piece2d(new Vector2d(-1, 1), new Vector2d(1, 1));
        var piece2:Piece2d = new Piece2d(new Vector2d(0, 3), new Vector2d(0, -2));
        var intersect:Vector2d = Vector2d.getZero();
        var vectorKeeper:Vector2dKeeper = new Vector2dKeeper();
        var cross:Boolean;
        cross = piece1.intersectsOld(piece2);
        assertTrue("1old", cross);
        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertTrue("1new", cross);
        //assertTrue(intersect.equals(new Vector2d(0, 1)));

        piece1 = new Piece2d(new Vector2d(0, -100), new Vector2d(10, 0));
        piece2 = new Piece2d(new Vector2d(-10, 100), new Vector2d(20, -200));
        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertTrue("2", cross);
        //assertTrue(intersect.equals(new Vector2d(5, -50)));

        piece1 = new Piece2d(new Vector2d(-10, 1.1), new Vector2d(10, 0));
        piece2 = new Piece2d(new Vector2d(-10, 1.1), new Vector2d(20, -200));
        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertTrue("3", cross);
        //assertTrue(intersect.equals(new Vector2d(-10, 1.1)));

        piece1 = new Piece2d(new Vector2d(-10, 1.1), new Vector2d(-10, -10));
        piece2 = new Piece2d(new Vector2d(-10, 1.2), new Vector2d(-10, 200));
        cross = piece1.intersectsOld(piece2, null);
        //trace(intersect.toString());
        assertFalse("4", cross);
        //assertTrue(intersect.equals(new Vector2d(-10, 1.1)));

        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertFalse("4-new", cross);

        piece1 = new Piece2d(new Vector2d(3, 3), new Vector2d(3, 3));
        piece2 = new Piece2d(new Vector2d(3, 3), new Vector2d(3, 3));
        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertTrue("5", cross);
        //assertTrue(intersect.equals(new Vector2d(3, 3)));

        piece1 = new Piece2d(new Vector2d(4, 4), new Vector2d(4, 4));
        piece2 = new Piece2d(new Vector2d(3, 3), new Vector2d(3, 3));
        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertFalse("6", cross);
        //assertTrue(intersect.equals(new Vector2d(3, 3)));

        piece1 = new Piece2d(new Vector2d(3, 3), new Vector2d(5, 5));
        piece2 = new Piece2d(new Vector2d(3, 4), new Vector2d(5, 5));
        cross = piece1.intersects(piece2, null, vectorKeeper);
        //trace(intersect.toString());
        assertTrue("7", cross);
        //assertTrue(intersect.equals(new Vector2d(5, 5)));

    }

    public function testLine():void {
        var piece2:Piece2d;
        var piece1:Piece2d = new Piece2d(new Vector2d(1, 0), new Vector2d(5, 2));
        var c:Array = piece1.getEquationCoefficients();
        //trace(c);
        assertTrue(Utils.equal(c[0], 0.5));
        assertTrue(Utils.equal(c[1], -0.5));

        var catched:Boolean = false;
        try {
            piece1 = new Piece2d(new Vector2d(1, 0), new Vector2d(1, 2));
            c = piece1.getEquationCoefficients();
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);

        piece1 = new Piece2d(new Vector2d(-1, 1), new Vector2d(1, 1));
        piece2 = new Piece2d(new Vector2d(0, 3), new Vector2d(5, -2));
        var intersect:Vector2d = Vector2d.getZero();
        var cross:Boolean = piece1.lineIntersects(piece2, intersect);
        assertTrue(cross);
        assertTrue(intersect.equals(new Vector2d(2, 1)));

        piece1 = new Piece2d(new Vector2d(-1, 1), new Vector2d(1, 1));
        piece2 = new Piece2d(new Vector2d(0, 3), new Vector2d(5, 3));
        cross = piece1.lineIntersects(piece2, intersect);
        assertFalse(cross);

        piece1 = new Piece2d(new Vector2d(-1, 1), new Vector2d(-1, 1));
        piece2 = new Piece2d(new Vector2d(0, 3), new Vector2d(5, 3));
        catched = false;
        try {
            cross = piece1.lineIntersects(piece2, intersect);
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);

        piece1 = new Piece2d(new Vector2d(-1, 1), new Vector2d(-1, 2));
        piece2 = new Piece2d(new Vector2d(0, 3), new Vector2d(5, 3));
        cross = piece1.lineIntersects(piece2, intersect);
        //trace(intersect);
        assertTrue(cross);
        assertTrue(intersect.equals(new Vector2d(-1, 3)));

        piece1 = new Piece2d(new Vector2d(0, 3), new Vector2d(5, 3));
        piece2 = new Piece2d(new Vector2d(-1, 1), new Vector2d(-1, 2));
        cross = piece1.lineIntersects(piece2, intersect);
        //trace(intersect);
        assertTrue(cross);
        assertTrue(intersect.equals(new Vector2d(-1, 3)));

        piece1 = new Piece2d(new Vector2d(6, 1), new Vector2d(6, 2));
        piece2 = new Piece2d(new Vector2d(6, 3), new Vector2d(6, 4));
        cross = piece1.lineIntersects(piece2, intersect);
        //trace(intersect);
        assertTrue(cross);
        assertTrue(intersect.equals(new Vector2d(6, 1)));

        piece1 = new Piece2d(new Vector2d(6, 1), new Vector2d(6, 2));
        piece2 = new Piece2d(new Vector2d(9, 3), new Vector2d(9, 4));
        cross = piece1.lineIntersects(piece2, intersect);
        assertFalse(cross);
    }

    public function testDistancetoLine():void {
        var line:Piece2d = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 0));
        var point:Vector2d = new Vector2d(0, 0);
        assertEquals(0, line.distanceToLine(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 1));
        point = new Vector2d(1, 0);
        assertEquals(-1, line.distanceToLine(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 1));
        point = new Vector2d(-1, 0);
        assertEquals(1, line.distanceToLine(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 1));
        point = new Vector2d(-3, -4);
        assertEquals(3, line.distanceToLine(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 2));
        point = new Vector2d(0, 1);
        assertEquals(0, line.distanceToLine(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 2));
        point = new Vector2d(0, -1);
        assertEquals(0, line.distanceToLine(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 2));
        point = new Vector2d(0, 3);
        assertEquals(0, line.distanceToLine(point));
    }

    public function testDistanceToPiece():void {
        var line:Piece2d = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 0));
        var point:Vector2d = new Vector2d(0, 0);
        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 1));
        point = new Vector2d(-3, -4);
        assertEquals(5, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 2));
        point = new Vector2d(0, 1);
        assertEquals(0, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 2));
        point = new Vector2d(1, 1);
        assertEquals(1, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 2));
        point = new Vector2d(-1, 1);
        assertEquals(1, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 1), new Vector2d(0, 2));
        point = new Vector2d(0, 0);
        assertEquals(1, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 1), new Vector2d(0, 2));
        point = new Vector2d(0, 3);
        assertEquals(1, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 1), new Vector2d(0, 2));
        point = new Vector2d(1, 3);
        assertEquals(Math.SQRT2, line.distanceTo(point));

        line = new Piece2d(new Vector2d(0, 1), new Vector2d(0, 2));
        point = new Vector2d(-1, 3);
        assertEquals(Math.SQRT2, line.distanceTo(point));
    }

    public function testProjection(): void {
        checkVector(2, 1, new Piece2d(new Vector2d(1, 1), new Vector2d(3, 1)).getProjection(new Vector2d(2, 10)), "proj");
    }

    public function testNearest(): void {
        checkVector(2, 1, new Piece2d(new Vector2d(1, 1), new Vector2d(3, 1)).getNearest(new Vector2d(2, 10)), "proj");
        checkVector(1, 1, new Piece2d(new Vector2d(1, 1), new Vector2d(3, 1)).getNearest(new Vector2d(0, 10)), "proj");
    }

}
}