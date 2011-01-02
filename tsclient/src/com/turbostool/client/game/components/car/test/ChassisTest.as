package com.turbostool.client.game.components.car.test {
import com.turbostool.client.game.components.car.*;
import com.turbostool.client.geom.*;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.Iterator;

public class ChassisTest extends TSTestCase {
    private var myChassis:Chassis;
    private var myHull:Hull;

    public function ChassisTest(methodName:String = null) {
        super(methodName);
    }

    override public function setUp():void {
        myHull = new Hull(750, 300, 1, 2);
        myChassis = new Chassis(myHull, 2, 4, 0);
    }

    protected function checkVector2(v:Vector2d, x:Number, y:Number):void {
        assertTrue('x=' + v.myX + ', а не ' + x + ' (v=' + v + ')', Utils.equal(x, v.myX));
        assertTrue('y=' + v.myY + ', а не ' + y + ' (v=' + v + ')', Utils.equal(y, v.myY));
    }

    protected function checkFrame(frame:LocalFrame2d, x:Number, y:Number, angle:Number):void {
        assertTrue('x=' + frame.myR.myX + ', а не ' + x + ' (frame=' + frame + ')', Utils.equal(x, frame.myR.myX));
        assertTrue('y=' + frame.myR.myY + ', а не ' + y + ' (frame=' + frame + ')', Utils.equal(y, frame.myR.myY));
        assertTrue('angle=' + frame.myR.myX + ', а не ' + angle + ' (frame=' + frame + ')', Utils.equal(angle, frame.myAngle));
    }

    public function testUpdateWheelVelocity():void {
        myChassis.myLength = 8;
        myChassis.myRearPinWidth = 6;
        myChassis.myFrontPinWidth = 6;
        myHull.myVelocity.setXY(5, 5);
        myHull.myAngularVelocity = 8;

        myChassis.updateWheelVelocity();
        checkVector2(myChassis.myRearLeft.myVelocity, -27, -19);
        checkVector2(myChassis.myFrontRight.myVelocity, 37, 29);
        checkVector2(myChassis.myRearRight.myVelocity, -27, 29);
        checkVector2(myChassis.myFrontLeft.myVelocity, 37, -19);
    }

    public function testUpdateWheelVelocityAndAngularVelocity():void {
        myHull.myVelocity.setXY(0, 5);
        var it:Iterator = myChassis.getWheels().iterator();
        var wheel:Wheel;
        while (it.hasNext()) {
            wheel = it.next() as Wheel;
            wheel.setWeight(100);
            wheel.myStaticFriction = 1;

        }
        myChassis.updateWheelVelocity();
        it = myChassis.getWheels().iterator();
        while (it.hasNext()) {
            wheel = it.next() as Wheel;
            assertTrue("" + wheel.myPinAngularVelocity, Utils.equal(wheel.myPinAngularVelocity, 5 / 0.22));

        }
    }

    public function testVelocityRails():void {
        myChassis.myRudderAngle = Math.atan(1.5);
        myChassis.myLength = 6;
        myHull.myForce.setXY(0, -100);
        myHull.myMass = 2;
        myChassis.myRearLeft.setGrip();
        myChassis.myFrontLeft.setGrip();
        checkVector2(myChassis.getTangForce(), 48, -64);
        myChassis.calcVelocity(0.01);
        checkVector2(myHull.myVelocity, 48 / ( 100 * 2 ), -64 / ( 100 * 2 ));
        assertTrue(myHull.myAngularVelocity, Utils.equal(myHull.myAngularVelocity, 40 * 0.01 / 5));
    }

    public function testGetTurnVector():void {
        myChassis.myRudderAngle = Math.atan(1.5);
        myChassis.myLength = 6;
        checkVector2(myChassis.getTurnVector(), -4, -3);
    }

    public function testVelocityDrift():void {
        myChassis.myRudderAngle = Math.atan(1.5);
        myChassis.myLength = 6;
        myHull.myForce.setXY(0, -100);
        myHull.myMass = 2;
        myHull.myMomentOfForce = 556;
        myChassis.myRearLeft.setSliding();
        myChassis.myRearRight.setSliding();
        myChassis.myFrontLeft.setSliding();
        myChassis.myFrontRight.setSliding();
        myChassis.calcVelocity(0.01);
        checkVector2(myHull.myVelocity, 0, -100 / ( 100 * 2 ));
        assertTrue(myHull.myAngularVelocity, Utils.equal(myHull.myAngularVelocity, 556 / 30000));
    }

    public function testTurn():void {
        var ch:Chassis = new Chassis(new Hull(100, 100, 2, 4), 1, 2, 0);
        ch.getHull().myFrame.setXYU(10, 30, Math.PI / 2);
        ch.myRudderAngle = Math.atan(4 / 3);
        var v:Vector2d = ch.getTurnVector();
        assertTrue("1 " + v, v.equals(new Vector2d(1, -1.5)));
        ch.myRudderAngle = Math.atan(-4 / 3);
        v = ch.getTurnVector();
        assertTrue("2 " + v, v.equals(new Vector2d(1, 1.5)));
        ch.getHull().myForce.setXY(2, 0);
        ch.myRudderAngle = Math.atan(4 / 3);
        ch.getHull().myVelocity = new Vector2d(3, 2);
        var angle:Number = Math.atan(2 / 3);
        var rc:Number = Math.sqrt(1 + 1.5 * 1.5);
        var fn:Number = 2 * Math.sin(angle);
        var ft:Number = 2 * Math.cos(angle);
        var m:Number = ch.getHull().myMass;
        var res:Number = ch.getTotalPinForce();
        assertTrue("3 " + res, Utils.equal(- m * 13 / rc - fn, res));
        res = ch.getAngularAcceleration();
        assertTrue("4 " + res, Utils.equal(ft / (rc * m), res));

        ch.myRudderAngle = Math.atan(-4 / 3);
        ch.getHull().myFrame.setXYU(10, 30, 2 * Math.atan(2 / 3) - Math.PI);
        ch.getHull().myForce.setXY(30, 40);
        //angle = Math.PI / 2 - Math.atan(2/3) - Math.atan(1/2);
        //var norm:Number = 50 * Math.cos(angle);
        //var tang:Number = 50 * Math.sin(angle);

        assertTrue("6 " + ch.getTurnVector(), ch.getTurnVector().equals(new Vector2d(-1.5, -1)));
        assertTrue("7 " + ch.getNormForce().sum2d(ch.getTangForce()).equals(ch.getHull().myForce));
        //assertTrue(ch.getNormForce() + ' ' + ch.getTangForce() + ' ' + norm + ' != ' + ch.getNormForce().length(), Utils.equal(norm, ch.getNormForce().length()));
        //assertTrue(ch.getTangForce() + ' ' + tang, Utils.equal(tang, ch.getTangForce().length()));
        assertTrue("8 " + ch.getNormForce(), ch.getNormForce().equals(new Vector2d(39.23076923076923, 26.153846153846146)));
        assertTrue("9 " + ch.getTangForce(), ch.getTangForce().equals(new Vector2d(-9.230769230769232, 13.846153846153852)));
        assertTrue("10 " + Utils.isZero(ch.getTangForce().scalarProduct(ch.getTurnVector())));

        ch.myRudderAngle = 0;
        ch.getHull().myFrame.setXYU(10, 30, Math.PI / 2);
        assertTrue("11 " + ch.getNormForce(), ch.getNormForce().equals(new Vector2d(0, 40)));
        assertTrue("12 " + ch.getTangForce(), ch.getTangForce().equals(new Vector2d(30, 0)));
    }

    public function testWidthLength():void {
        assertTrue(Utils.equal(2, myChassis.myRearPinWidth));
        assertTrue(Utils.equal(2, myChassis.myFrontPinWidth));
        assertTrue(Utils.equal(4, myChassis.myLength));

        myChassis.myLength = 13;
        assertTrue(myChassis.myLength, Utils.equal(13, myChassis.myLength));
        checkFrame(myChassis.myRearRight.myFrame, 1, 6.5, 0);

        myChassis.myRearPinWidth = 25;
        myChassis.myFrontPinWidth = 25;
        assertTrue(myChassis.myRearPinWidth, Utils.equal(25, myChassis.myRearPinWidth));
        assertTrue(myChassis.myFrontPinWidth, Utils.equal(25, myChassis.myFrontPinWidth));
        checkFrame(myChassis.myRearRight.myFrame, 12.5, 6.5, 0);
    }

    public function testGetTurnVectorAngle():void {
        myChassis.myRudderAngle = Math.PI / 4;
        assertTrue("0, " + myChassis.getTurnVectorAngle() + " expected =  " + myChassis.getTurnVector().getAngle(), checkAnglesEqual(myChassis.getTurnVector().getAngle(), myChassis.getTurnVectorAngle()));
        myChassis.myRudderAngle = Math.PI / 12;
        assertTrue("1, " + myChassis.getTurnVectorAngle() + " expected =  " + myChassis.getTurnVector().getAngle(), checkAnglesEqual(myChassis.getTurnVector().getAngle(), myChassis.getTurnVectorAngle()));
        myChassis.myRudderAngle = Math.PI / 6;
        assertTrue("2, " + myChassis.getTurnVectorAngle() + " expected =  " + myChassis.getTurnVector().getAngle(), checkAnglesEqual(myChassis.getTurnVector().getAngle(), myChassis.getTurnVectorAngle()));
        myHull.myFrame.setXYU(34, 56, 77);
        myChassis.updateWheelFrames();
        myChassis.updateWheelVelocity();
        myChassis.myRudderAngle = Math.PI / 6;
        assertTrue("3, " + myChassis.getTurnVectorAngle() + " expected =  " + myChassis.getTurnVector().getAngle(), checkAnglesEqual(myChassis.getTurnVector().getAngle(), myChassis.getTurnVectorAngle()));
        myHull.myFrame.setXYU(100 * Math.random(), 100 * Math.random(), Math.random() * Math.PI * 2);
        myChassis.updateWheelFrames();
        myChassis.updateWheelVelocity();
        myChassis.myRudderAngle = Math.PI / 6;
        assertTrue("4, " + myChassis.getTurnVectorAngle() + " expected =  " + myChassis.getTurnVector().getAngle(), checkAnglesEqual(myChassis.getTurnVector().getAngle(), myChassis.getTurnVectorAngle()));
        myHull.myFrame.setXYU(100 * Math.random(), 100 * Math.random(), Math.random() * Math.PI * 2);
        myChassis.updateWheelFrames();
        myChassis.updateWheelVelocity();
        myChassis.myRudderAngle = Math.PI / 6;
        assertTrue("5, " + myChassis.getTurnVectorAngle() + " expected =  " + myChassis.getTurnVector().getAngle(), checkAnglesEqual(myChassis.getTurnVector().getAngle(), myChassis.getTurnVectorAngle()));
    }

    private function checkAnglesEqual(a:Number, b:Number):Boolean {
        return Utils.equal(Math.sin(a), Math.sin(b)) && Utils.equal(Math.cos(a), Math.cos(b));
    }

    public function testWheelAngles():void {
        var hull:Hull = new Hull(1000, 1000, 2, 4);
        var che: Chassis;
        che = new Chassis(hull, 2, 4, 1);
        che.myRudderAngle = Math.atan2(4, 1);
        che.updateWheelFrames();
        checkNumber(Math.atan2(4, 2), che.myFrontLeft.myAngle, "frontLeft");
        checkNumber(Math.PI / 2, che.myFrontRight.myAngle, "frontRight");

        che.myRudderAngle = - Math.atan2(4, 1);
        che.updateWheelFrames();
        checkNumber(- Math.PI / 2, che.myFrontLeft.myAngle, "frontLeft -Math.atan2(4, 1)");
        checkNumber(- Math.atan2(4, 2), che.myFrontRight.myAngle, "frontRight -Math.atan2(4, 1)");
    }


}
}