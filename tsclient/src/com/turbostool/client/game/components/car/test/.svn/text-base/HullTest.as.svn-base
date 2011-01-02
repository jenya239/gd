package com.turbostool.client.game.components.car.test    {

import com.turbostool.client.dynamicEngine.*;
import com.turbostool.client.game.components.car.*;
import com.turbostool.client.geom.*;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.Iterator;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class HullTest extends TestCase
{
    private var myChassis:Chassis;
    private var myHull:Hull;

    public function HullTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new HullTest("testHull"));
        ts.addTest(new HullTest("testCalcCoordinates"));
        ts.addTest(new HullTest("testNullForces"));
        ts.addTest(new HullTest("testCalcForces"));
        ts.addTest(new HullTest("testCalcVelocity"));
        ts.addTest(new HullTest("testStates"));
        ts.addTest(new HullTest("testBorder"));
        return ts;
    }

    override public function setUp():void {
        myHull = new Hull(750, 300, 1, 2);
        myChassis = new Chassis(myHull, 2, 4, 0);
    }

    public function testCalcCoordinates():void {
        myHull.myVelocity.setXY(2, 4);
        myHull.myAngularVelocity = 3;
        myHull.calcCoordinates(2);
        assertTrue(myHull.myFrame.equals(new LocalFrame2d(new Vector2d(4, 8), 6)));
    }

    public function testNullForces():void {
        myHull.nullForces();
        assertTrue(Utils.equal(myHull.myWeight, 750 * Utils.G));
        myHull.myVelocity.setXY(3, 4);
        myHull.nullForces();
        var temp:Number = - 5 * myHull.myAreaOfCut * Utils.AIR_DENSITY;
        assertTrue(new Vector2d(3 * temp, 4 * temp).equals(myHull.myForce));
        myHull.myPressingCoef = 5;
        myHull.nullForces();
        temp = myHull.myVelocity.sqrLength() * 5 * Utils.AIR_DENSITY;
        assertTrue(Utils.equal(myHull.myWeight, 750 * Utils.G + temp));
    }

    public function testCalcForces():void {
        var it:Iterator = myChassis.getWheels().iterator();
        while (it.hasNext()) {
            var wheel:Wheel = it.next() as Wheel;
            wheel.mySlidingFriction = 1;
            wheel.setWeight(1);
            wheel.setSliding();
        }
        myHull.myVelocity.setXY(100, 0);
        myChassis.updateWheelVelocity();
        myChassis.calcForces();
        assertTrue(myHull.myForce, myHull.myForce.equals(new Vector2d(-4, 0)));
        assertTrue(myHull.myMomentOfForce, Utils.equal(myHull.myMomentOfForce, 0));
    }

    public function testCalcVelocity():void {
        myHull.myForce.setXY(2, 4);
        myHull.myMomentOfForce = 2;
        myHull.calcVelocity(3);
        assertTrue(myHull.myAngularVelocity, Utils.equal(myHull.myAngularVelocity, 6 / 300));
        assertTrue(myHull.myVelocity, myHull.myVelocity.equals(new Vector2d(6 / 750, 12 / 750)));
    }

    public function testStates():void {
        myHull.myFrame.setXYU(1, 2, 3);
        myChassis.updateWheelFrames();
        myHull.rollback();
        assertTrue(myHull.myFrame.equals(new LocalFrame2d(new Vector2d(0, 0), 0)));
        assertTrue(myChassis.myFrontRight.myFrame, myChassis.myFrontRight.myFrame.equals(new LocalFrame2d(new Vector2d(1, -2), 0)));
    }

    public function testHull():void {
        var ch:Hull = new Hull(750, 300, 1, 2);
        var chassis:Chassis = new Chassis(ch, 2, 4, 0);
        assertNotNull(ch.myFrame.myR);
        assertNotNull(ch.myVelocity);
        assertNotNull(ch.myForce);
        ch.rollback();
        assertNotNull(ch.myFrame.myR);
        assertNotNull(ch.myVelocity);
        assertNotNull(ch.myForce);
        ch.myMass = 10;
        ch.myFrame.myR.setXY(1, 2);
        ch.myVelocity.setXY(4, 5);
        ch.myForce.setXY(8, 8);

        ch.myFrame.myAngle = 1;//
        ch.myAngularVelocity = 4.4;
        ch.myMomentOfForce = 9;
        ch.myMomentOfInertia = 4;//

        ch.saveState();
        assertTrue(ch.myForce.equals(new Vector2d(8, 8)));
        assertTrue(ch.myVelocity, ch.myVelocity.equals(new Vector2d(4, 5)));
        assertTrue(ch.myFrame.myR, ch.myFrame.myR.equals(new Vector2d(1, 2)));
        assertTrue(Utils.equal(10, ch.myMass));
        chassis.calcForces();
        ch.calcVelocity(3);
        ch.calcCoordinates(3);
        assertTrue(ch.myForce.equals(new Vector2d(8, 8)));
        assertTrue(ch.myVelocity, ch.myVelocity.equals(new Vector2d(6.4, 7.4)));
        assertTrue(ch.myFrame.myR, ch.myFrame.myR.equals(new Vector2d(20.2, 24.2)));

        assertTrue(ch.myAngularVelocity, Utils.equal(11.15, ch.myAngularVelocity));
        assertTrue(Utils.equal(34.45, ch.myFrame.myAngle));
        assertTrue(Utils.equal(9, ch.myMomentOfForce));
        ch.rollback();

    }

    public function testBorder():void {
        myHull = new Hull(750, 300, 2, 4);
        myChassis = new Chassis(myHull, 2, 4, 0);

        myChassis.myMassCenterLongShift = 1;
        //myHull.getRectangle().myWidth = 2;
        //myHull.getRectangle().myHeight = 4;
        myHull.myFrame.setXYU(0, -1, 0);
        myHull.saveState();
        myHull.myFrame.setXYU(1, -3, 0);
        var etalonBorder:BodyBorder = new BodyBorder();
        etalonBorder.myMaxX = 2;
        etalonBorder.myMaxY = 2;
        etalonBorder.myMinX = -1;
        etalonBorder.myMinY = -4;
        var border:BodyBorder = new BodyBorder();
        myHull.updateBorder();
        assertTrue("1 " + myHull.getBorder(), etalonBorder.equals(myHull.getBorder()));

        myHull.myFrame.setXYU(0, -1, 0);
        myHull.saveState();
        myHull.myFrame.setXYU(4, 3, Math.PI / 2);
        etalonBorder.myMaxX = 5;
        etalonBorder.myMaxY = 4;
        etalonBorder.myMinX = -1;
        etalonBorder.myMinY = -2;
        myHull.updateBorder();
        assertTrue("2 " + myHull.getBorder(), etalonBorder.equals(myHull.getBorder()));
    }
}
}