package com.turbostool.client.game.components.car.test         {

import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class RudderControlTest extends TestCase {
    private var myChassis:Chassis;
    private var myControl:RudderControl;

    public function RudderControlTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new RudderControlTest("testAngle"));
        ts.addTest(new RudderControlTest("testVelocity"));
        return ts;
    }

    override public function setUp():void {
        myChassis = new Chassis(new Hull(100, 100, 2, 4), 1, 2, 0);
        myControl = new RudderControl(myChassis, 2, 0.1);
    }

    public function testAngle():void {
        myControl.myAngle = 3;
        assertTrue(myControl.myAngle, Utils.equal(myControl.myAngle, 2));
        myControl.myAngle = -4;
        assertTrue(myControl.myAngle, Utils.equal(myControl.myAngle, -2));
        myControl.myAngle = 1.5;
        /*var frame:LocalFrame2d = myChassis.getWheelFrame(WheelPosition.FRONT_LEFT);
         assertTrue(frame, frame.equals(new LocalFrame2d(new Vector2d(-0.5, -1), 1.5)));
         frame = myChassis.getWheelFrame(WheelPosition.FRONT_RIGHT);
         assertTrue(frame, frame.equals(new LocalFrame2d(new Vector2d(0.5, -1), 1.5)));
         frame = myChassis.getWheelFrame(WheelPosition.REAR_LEFT);
         assertTrue(frame, frame.equals(new LocalFrame2d(new Vector2d(-0.5, 1), 0)));
         frame = myChassis.getWheelFrame(WheelPosition.REAR_RIGHT);
         assertTrue(frame, frame.equals(new LocalFrame2d(new Vector2d(0.5, 1), 0)));*/
    }

    public function testVelocity():void {
        myControl.myMaxAngle = Number.MAX_VALUE;
        myControl.myAngle = -1.9;
        myControl.myAction = RudderControl.RIGHT;
        myControl.myBaseVelocity = 0.5;
        myControl.myBaseReturnVelocity = 0.5;
        myControl.calcVelocity(999);
        myControl.calcCoordinates(2);
        assertTrue(myControl.myAngle, Utils.equal(- 0.9, myControl.myAngle));
        myChassis.saveState();
        myControl.calcCoordinates(2);
        assertTrue(myControl.myAngle, Utils.equal(0, myControl.myAngle));
        myControl.calcCoordinates(2);
        assertTrue(myControl.myAngle, Utils.equal(1, myControl.myAngle));
        myChassis.rollback();
        assertTrue(myControl.myAngle, Utils.equal(- 0.9, myControl.myAngle));
        myControl.calcCoordinates(2);
        myControl.calcCoordinates(2);
        myControl.myAction = RudderControl.NONE;
        myControl.calcVelocity(999);
        myControl.calcCoordinates(1.6);
        assertTrue(myControl.myAngle, Utils.equal(0.2, myControl.myAngle));
        myControl.calcCoordinates(1.6);
        assertTrue(myControl.myAngle, Utils.equal(0, myControl.myAngle));
        myControl.calcVelocity(999);
        myControl.calcCoordinates(1.6);
        assertTrue(myControl.myAngle, Utils.equal(0, myControl.myAngle));
        myControl.calcCoordinates(1.6);
        assertTrue(myControl.myAngle, Utils.equal(0, myControl.myAngle));
        myControl.myAction = RudderControl.LEFT;
        myControl.calcVelocity(999);
        myControl.calcCoordinates(1.6);
        assertTrue(myControl.myAngle, Utils.equal(- 0.8, myControl.myAngle));
        myControl.calcVelocity(999);
        myControl.calcCoordinates(2);
        assertTrue(myControl.myAngle, Utils.equal(- 1.8, myControl.myAngle));
        myControl.calcCoordinates(2);
        assertTrue(myControl.myAngle, Utils.equal(- 2.8, myControl.myAngle));
        myControl.myAction = RudderControl.NONE;
        myControl.calcVelocity(999);
        myControl.calcCoordinates(6);
        assertTrue(myControl.myAngle, Utils.equal(0, myControl.myAngle));
        myControl.calcCoordinates(6);
        assertTrue(myControl.myAngle, Utils.equal(3, myControl.myAngle));
    }


}
}