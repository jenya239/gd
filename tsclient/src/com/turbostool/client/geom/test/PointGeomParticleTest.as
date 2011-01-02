package com.turbostool.client.geom.test
{
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.PointGeomParticle;
import com.turbostool.client.utils.*;

;

public class PointGeomParticleTest extends TestCase
{
    public function PointGeomParticleTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new PointGeomParticleTest("testPointGeomParticle"));
        ts.addTest(new PointGeomParticleTest("testEquals"));
        ts.addTest(new PointGeomParticleTest("testGlobal"));
        return ts;
    }

    public function testPointGeomParticle():void {
        var p1:PointGeomParticle = new PointGeomParticle(new Vector2d(10, 20));
        var p2:PointGeomParticle = new PointGeomParticle(new Vector2d(30, 400));
        assertFalse(p1.checkCollision(p1));
        assertFalse(p1.checkCollision(p2));
    }

    public function testEquals():void {
        var p1:PointGeomParticle = new PointGeomParticle(new Vector2d(10, 20));
        var p2:PointGeomParticle = new PointGeomParticle(new Vector2d(30, 400));
        assertTrue(p1.equals(p1));
        assertTrue(p1.equals(p1.clone()));
        assertFalse(p1.equals(p2));
        assertFalse(p1.equals(p2.clone()));
        p1.copyTo(p2);
        assertTrue(p1.equals(p2));
    }

    public function testGlobal():void {
        var p1:PointGeomParticle = new PointGeomParticle(new Vector2d(10, 20));
        var frame:LocalFrame2d = new LocalFrame2d(new Vector2d(9, 9), Math.PI / 2);
        assertTrue(p1.createGlobal(frame) is PointGeomParticle);
        var res:PointGeomParticle = p1.createGlobal(frame) as PointGeomParticle;
        assertFalse(p1.equals(res));
        assertTrue(res, res.equals(new PointGeomParticle(new Vector2d(-11, 19))));
    }

}
}