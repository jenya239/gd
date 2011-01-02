package com.turbostool.client.game.components.car.test
{

import com.turbostool.client.dynamicEngine.*;
import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class SuspensionTest extends TestCase {
    private var mySuspension:Suspension;
    private var myEngine:DynamicEngine;
    private var myChassis:Chassis;

    public function SuspensionTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new SuspensionTest("testNullForces"));
        ts.addTest(new SuspensionTest("testNullForces2"));
        //ts.addTest( new SuspensionTest( "testSuspension" ) );
        ts.addTest(new SuspensionTest("testEqMatrix"));
        ts.addTest(new SuspensionTest("testEquilibrium"));
        return ts;
    }

    override public function setUp():void {
        var ch:Chassis = new Chassis(new Hull(100, 100, 2, 4), 1, 2, 0);
        mySuspension = new Suspension(ch, ch.getHull(), 0.5, 5000, 10000, 500, 300);
        myChassis = ch;
    }

    /*private function initialize():void {
     myEngine = new DynamicEngine();
     myEngine.initialize();
     myEngine.getDynamics().addItem(mySuspension);
     var road:CarpetLayerModel = new CarpetLayerModel(
     new Rectangle(new Vector2d(5, 5), 10, 10),
     new RoadCarpet(2, 2, 2)
     );
     myEngine.getInteractables().addItem(mySuspension.myHull);
     myEngine.getInteractables().addCollection(mySuspension.myChassis.getWheels());
     myEngine.getInteractables().addItem(road);
     myEngine.getInteractables().addItem(new Gravitation(new Vector3d(0, 0, 10)));
     mySuspension.myChassis.getHull().myFrame.setXYU(5, 5, Math.PI / 2);
     mySuspension.myChassis.getHull().myVelocity = new Vector2d(3, 4);
     myEngine.run();
     }*/

    public function testNullForces():void {
        var n:Number;
        //initialize();
        //mySuspension.myHull.myForce.myZ = 100;
        mySuspension.myHull.myMass = 100 / Utils.G;
        mySuspension.myHull.nullForces();
        myChassis.calcForces();
        mySuspension.calcForces();
        mySuspension.setLateralAngle(0.001);
        mySuspension.setLongAngle(0.001);
        mySuspension.nullForces();
        n = myChassis.myFrontLeft.myWeight;
        assertTrue(n, Utils.equal(22.5, n));
        n = myChassis.myFrontRight.myWeight;
        assertTrue(n, Utils.equal(17.5, n));
        n = myChassis.myRearLeft.myWeight;
        assertTrue(n, Utils.equal(32.5, n));
        n = myChassis.myRearRight.myWeight;
        assertTrue(n, Utils.equal(27.5, n));
        n = mySuspension.getLateralMoment();
        assertTrue(n, Utils.equal(-5, n));
        n = mySuspension.getLongMoment();
        assertTrue(n, Utils.equal(-20, n));
    }

    private function checkWheelWeight(wheel:Wheel, weight:Number):void {
        assertTrue("wheel.myWeight = " + wheel.myWeight + ", but expected " + weight, Utils.equal(weight, wheel.myWeight));
    }

    public function testNullForces2():void {
        var n:Number;
        myChassis.myMassCenterLongShift = 0.5;
        mySuspension.nullForces();
        var weight:Number = mySuspension.myHull.myWeight;
        assertTrue(weight > 0);
        checkWheelWeight(myChassis.myFrontRight, weight * 3 / 8);
        checkWheelWeight(myChassis.myFrontLeft, weight * 3 / 8);
        checkWheelWeight(myChassis.myRearLeft, weight * 1 / 8);
        checkWheelWeight(myChassis.myRearRight, weight * 1 / 8);
    }

    /*public function testSuspension():void {
     //initialize();
     var fw:Vector2d = new Vector2d(- 300, - 400);
     var moment:Vector3d = new Vector3d(1, 0.5, 0.5).vectorProduct(fw);
     moment = moment.sum(new Vector3d(- 1, 0.5, 0.5).vectorProduct(fw));
     moment = moment.sum(new Vector3d(- 1, - 0.5, 0.5).vectorProduct(fw));
     moment = moment.sum(new Vector3d(1, - 0.5, 0.5).vectorProduct(fw));
     myEngine.step(0.002);
     myEngine.step(0.002);
     myEngine.step(0.002);
     //myEngine.step(0.002);
     var yAngle:Number = - moment.myX * 0.002 * 0.002 / 300;
     assertTrue(yAngle + ' ' + mySuspension.getLateralAngle(),
     Utils.equal(yAngle, mySuspension.getLateralAngle())
     );
     var xAngle:Number = moment.myY * 0.002 * 0.002 / 500;
     assertTrue(xAngle + ' ' + mySuspension.getLongAngle(),
     Utils.equal(xAngle, mySuspension.getLongAngle())
     );
     }*/

    public function testEqMatrix():void {
        var m:Matrix2d = new Matrix2d(
                0, - 4,
                1, 0
                );
        assertTrue(mySuspension.getEqMatrix(), mySuspension.getEqMatrix().equals(m));
        mySuspension.myChassis.myRudderAngle = -1;
        assertTrue(mySuspension.getEqMatrix(), mySuspension.getEqMatrix().equals(m));
        mySuspension.myChassis.myRudderAngle = 1;
    }

    public function testEquilibrium():void {
        /*myChassis.getWheel(WheelPosition.FRONT_LEFT).nullForces();
         //myChassis.getWheel(WheelPosition.FRONT_LEFT).addContactForce(new Vector2d(1, 2));
         myChassis.getWheel(WheelPosition.FRONT_RIGHT).nullForces();
         //myChassis.getWheel(WheelPosition.FRONT_RIGHT).addContactForce(new Vector2d(2, 3));
         myChassis.getWheel(WheelPosition.REAR_LEFT).nullForces();
         //myChassis.getWheel(WheelPosition.REAR_LEFT).addContactForce(new Vector2d(4, 5));
         myChassis.getWheel(WheelPosition.REAR_RIGHT).nullForces();
         //myChassis.getWheel(WheelPosition.REAR_RIGHT).addContactForce(new Vector2d(6, 7));
         mySuspension.calcForces();
         mySuspension.calcEquilibrium();
         var msx:Number = 8.5;
         var msy:Number = - 6.5;
         var v:Vector2d = Utils.solveSystem2d(mySuspension.getEqMatrix(), new Vector2d(msx / 5000, - msy / 5000));
         assertTrue(v.myX + ' ' + mySuspension.getLateralEquilibrium(),
         Utils.equal(v.myX, mySuspension.getLateralEquilibrium())
         );
         assertTrue(v.myY + ' ' + mySuspension.getLongEquilibrium(),
         Utils.equal(v.myY, mySuspension.getLongEquilibrium())
         );*/
    }
}
}