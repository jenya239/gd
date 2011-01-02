package com.turbostool.client.game.components.car.test  {

import com.turbostool.client.game.components.car.*;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class CarBrakesTest extends TestCase {

    public function CarBrakesTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new CarBrakesTest("testCarBrakes"));
        return ts;
    }

    public function testCarBrakes():void {
        /*
         var w1:Wheel = new Wheel();
         var w2:Wheel = new Wheel();
         var ws:ArrayList = new ArrayList();
         ws.addItem(new Wheel());
         ws.addItem(w1);
         ws.addItem(w2);
         ws.addItem(new Wheel());
         */
        var hull:Hull = new Hull(100, 100, 2, 4);
        hull.myVelocity.setXY(9, 9);
        var ch:Chassis = new Chassis(hull, 2, 4);
        var brakes:Brakes = new Brakes(ch, 1500);
        brakes.myActive = false;
        brakes.calcForces();
        assertEquals(0, ch.myRearLeft.myRetardingTorque);
        assertEquals(0, ch.myRearRight.myRetardingTorque);
        brakes.myActive = true;
        brakes.calcForces();
        assertEquals(0, ch.myRearLeft.myRetardingTorque);
        assertEquals(0, ch.myRearRight.myRetardingTorque);
        ch.myRearLeft.myPinAngularVelocity = 10;
        ch.myRearRight.myPinAngularVelocity = 10;
        brakes.myActive = true;
        brakes.calcVelocity(100);
        brakes.calcCoordinates(100);
        brakes.calcForces();
        assertEquals(- 1500, ch.myRearLeft.myRetardingTorque);
        assertEquals(- 1500, ch.myRearRight.myRetardingTorque);
        brakes.myHandBrake = false;
        assertEquals(10, ch.myRearLeft.myPinAngularVelocity);
        assertEquals(10, ch.myRearRight.myPinAngularVelocity);
        brakes.myHandBrake = true;
    }

}
}