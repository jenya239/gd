package com.turbostool.client.geom.test
{
import com.turbostool.client.geom.*;
import com.turbostool.client.utils.*;

;

public class FramedGeomTest extends TestCase
{
    public function FramedGeomTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new FramedGeomTest("testEquals"));
        //ts.addTest( new FramedGeomTest( "testCollision" ) );
        ts.addTest(new FramedGeomTest("testCheckCollision"));
        ts.addTest(new FramedGeomTest("testGlobal"));
        return ts;
    }

    public function testEquals():void {
        var seq:Vector2dSequence = new Vector2dSequence();
        seq.addVertex(new Vector2d(10, 9));
        seq.addVertex(new Vector2d(100, 90));
        seq.addVertex(new Vector2d(200, 100));
        seq.addVertex(new Vector2d(10, 50));
        seq.addVertex(new Vector2d(10, 90));
        var seq2:Vector2dSequence = new Vector2dSequence();
        seq2.addVertex(new Vector2d(10, 9));
        seq2.addVertex(new Vector2d(100, 90));
        seq2.addVertex(new Vector2d(200, 100));
        seq2.addVertex(new Vector2d(10, 50));
        seq2.addVertex(new Vector2d(10, 90));
        var fg1:FramedGeom = new FramedGeom(
                new LocalFrame2d(new Vector2d(5, 6), Math.PI / 2), seq
                );
        var fg2:FramedGeom = new FramedGeom(
                new LocalFrame2d(new Vector2d(5, 6), Math.PI / 2), seq2
                );
        assertTrue(fg1.equals(fg2));
        assertTrue(fg1.equals(fg1));
        assertTrue(fg2.equals(fg1));
        assertTrue(fg1.equals(fg2.clone()));
        assertTrue(fg1.equals(fg1.clone()));
        fg2.copyTo(fg1);
        assertTrue(fg1.equals(fg2));
        assertTrue(fg1.equals(fg1));
        assertTrue(fg2.equals(fg1));
        fg1.myFrame.myAngle = 2;
        assertFalse(fg1.equals(fg2));
        seq2.addVertex(new Vector2d(10, 90));
        assertFalse(fg1.equals(fg2));
        fg2.copyTo(fg1);
        assertTrue(fg1.equals(fg2));
        assertTrue(fg1.equals(fg1));
        assertTrue(fg2.equals(fg1));
        seq.addVertex(new Vector2d(100, 90));
        assertFalse(seq.equals(seq2));
        assertFalse(seq.equals(new Vector2d(100, 90)));
    }

    public function testCollision():void {
        var catched:Boolean;
        var seq:Vector2dSequence = new Vector2dSequence();
        //  |_
        //_/|_\__
        seq.addVertex(new Vector2d(-5, 0));
        seq.addVertex(new Vector2d(0, 5));
        seq.addVertex(new Vector2d(10, 5));
        seq.addVertex(new Vector2d(15, 0));

        var point1:PointGeomParticle = new PointGeomParticle(new Vector2d(5, 10));
        var point2:PointGeomParticle = new PointGeomParticle(new Vector2d(5, 0));
        var seq2:Vector2dSequence = new Vector2dSequence();

        var collisionPoint:Vector2d = new Vector2d(0, 0);
        var normal:Ort2d = new Ort2d(0);

        var fg11:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(0, 0), 0), seq);
        var fg12:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(0, 0), 0), seq2);
        var fg21:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(0, 0), 0), point1);
        var fg22:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(0, 0), 0), point2);

        fg11.calcCollisionVectors(fg12, fg21, fg22, collisionPoint, normal);
        assertTrue(collisionPoint.equals(new Vector2d(5, 5)));
        assertTrue(normal.equals(new Ort2d(3 * Math.PI / 2)));

        collisionPoint = new Vector2d(0, 0);
        normal = new Ort2d(0);
        fg11.calcCollisionVectors(fg12, point1, point2, collisionPoint, normal);
        assertTrue(collisionPoint.equals(new Vector2d(5, 5)));
        assertTrue(normal.equals(new Ort2d(3 * Math.PI / 2)));

        collisionPoint = new Vector2d(0, 0);
        normal = new Ort2d(0);
        point1.calcCollisionVectors(point2, fg11, fg12, collisionPoint, normal);
        assertTrue(collisionPoint.equals(new Vector2d(5, 5)));
        assertTrue(normal.equals(new Ort2d(3 * Math.PI / 2)));

        catched = false;
        fg11.myGeom = seq;
        fg12.myGeom = seq2;
        fg21.myGeom = point1;
        fg22.myGeom = point2;
        fg11.myFrame = new LocalFrame2d(new Vector2d(20, 30), Math.PI / 2);
        fg12.myFrame = new LocalFrame2d(new Vector2d(20, 30), Math.PI / 2);
        fg21.myFrame = new LocalFrame2d(new Vector2d(20, 30), Math.PI / 2);
        fg22.myFrame = new LocalFrame2d(new Vector2d(20, 30), Math.PI / 2);
        fg11.calcCollisionVectors(fg12, fg21, fg22, collisionPoint, normal);
        assertTrue(collisionPoint, collisionPoint.equals(new Vector2d(15, 35)));
        assertTrue(normal, normal.equals(new Ort2d(4 * Math.PI / 2)));
    }

    public function testCheckCollision():void {
        var pol:Polygon = new Polygon();
        pol.addVertex(new Vector2d(10, 0));
        pol.addVertex(new Vector2d(10, 10));
        pol.addVertex(new Vector2d(0, 10));
        pol.addVertex(new Vector2d(0, 0));
        var point:PointGeomParticle = new PointGeomParticle(new Vector2d(7, 2));
        var fg:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(0, 0), Math.PI / 2), pol);
        assertFalse(fg.checkCollision(point));
        fg.myFrame.myAngle = 0;
        assertTrue(fg.checkCollision(point));
    }

    public function testGlobal():void {
        var point:PointGeomParticle = new PointGeomParticle(new Vector2d(7, 2));
        var fg:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(7, 2), Math.PI / 2), point);
        var test:FramedGeom = new FramedGeom(new LocalFrame2d(new Vector2d(-5, 1), 3 * Math.PI / 2), new PointGeomParticle(new Vector2d(7, 2)));
        var res:FramedGeom = fg.createGlobal(new LocalFrame2d(new Vector2d(2, 3), Math.PI)) as FramedGeom;
        assertTrue(res.myFrame, res.equals(test));
        assertTrue(res.getGlobalParticle().equals(new PointGeomParticle(new Vector2d(-3, -6))));
    }


}
}