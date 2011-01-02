package com.turbostool.client.game.components.car.test {

import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class DifferentialTest extends TestCase {
    private var myWheels:Chassis;
    private var myDiff:Differential;

    public function DifferentialTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new DifferentialTest("testAverage"));
        ts.addTest(new DifferentialTest("testTorque"));
        return ts;
    }

    override public function setUp():void {
        myWheels = new Chassis(new Hull(100, 100, 2, 4), 1, 2, 0);
        myDiff = new Differential(myWheels, Differential.FRONT_DRIVE);
    }

    public function testAverage():void {
        var ws:Chassis = myWheels;
        ws.myFrontLeft.myPinAngularVelocity = 3;
        ws.myFrontRight.myPinAngularVelocity = 5;
        ws.myRearLeft.myPinAngularVelocity = 2;
        ws.myRearRight.myPinAngularVelocity = 4;
        var res:Number = myDiff.getAverageAngularVelocity();
        assertTrue(res, Utils.equal(4, res));
        myDiff.myRearRelativeTorque = Differential.REAR_DRIVE;
        res = myDiff.getAverageAngularVelocity();
        assertTrue(res, Utils.equal(3, res));
        myDiff.myRearRelativeTorque = Differential.FULL_DRIVE;
        res = myDiff.getAverageAngularVelocity();
        assertTrue(res, Utils.equal(3.5, res));
    }

    private function assertTorque(wheel:Wheel, expectedTorque:Number):void {
        var tempTorque:Number = wheel.myEngineTorque;
        assertTrue('expected = ' + expectedTorque + ' real= ' + tempTorque, Utils.equal(expectedTorque, tempTorque));
    }

    public function testTorque():void {
        var ws:Chassis = myWheels;
        myDiff.setTorque(150);
        assertTorque(ws.myFrontLeft, 75);
        assertTorque(ws.myFrontRight, 75);
        assertTorque(ws.myRearLeft, 0);
        assertTorque(ws.myRearRight, 0);
        myDiff.myRearRelativeTorque = Differential.REAR_DRIVE;
        myDiff.setTorque(150);
        assertTorque(ws.myFrontLeft, 0);
        assertTorque(ws.myFrontRight, 0);
        assertTorque(ws.myRearLeft, 75);
        assertTorque(ws.myRearRight, 75);
        myDiff.myRearRelativeTorque = Differential.FULL_DRIVE;
        myDiff.setTorque(200);
        assertTorque(ws.myFrontLeft, 50);
        assertTorque(ws.myFrontRight, 50);
        assertTorque(ws.myRearLeft, 50);
        assertTorque(ws.myRearRight, 50);
    }

}
}