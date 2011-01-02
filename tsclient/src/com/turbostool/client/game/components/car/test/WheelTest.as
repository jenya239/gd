package com.turbostool.client.game.components.car.test
{
import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

public class WheelTest extends TSTestCase {

    private var myWheel:Wheel;

    public function WheelTest(methodName:String = null) {
        super(methodName);
    }

    override public function setUp():void {
        var wheel:Wheel = new Wheel(2);
        wheel.setWeight(20);
        wheel.myRetardingTorque = 4;
        wheel.myEngineTorque = 6;
        wheel.myStaticFriction = 2;
        wheel.mySlidingFriction = 1.3;
        wheel.myPinAngularVelocity = 0;
        wheel.myFrame.myR = new Vector2d(20, 30);
        wheel.myFrame.myAngle = Math.PI / 2;
        myWheel = wheel;
    }


    public function testGripFx():void {
        // 	|/1575
        myWheel.setGrip();
        assertTrue(myWheel.getMaxFXWhenRoadGrip(), Utils.equal(Math.sqrt(1575), myWheel.getMaxFXWhenRoadGrip()));
    }

    public function testForceY():void {
        myWheel.myPinAngularVelocity = 100;
        myWheel.myVelocity = new Vector2d(- 200, 0);
        myWheel.setGrip();
        assertTrue(myWheel.getForceYWhenRoadGrip(Vector2d.getZero()), ( new Vector2d(- 5, 0)).equals(myWheel.getForceYWhenRoadGrip(Vector2d.getZero())));
    }

    public function testSlidingForce():void {
        myWheel.myVelocity = new Vector2d(-1000, 0);
        myWheel.myPinAngularVelocity = 100;
        myWheel.mySlidingFriction = 10;
        myWheel.myEngineTorque = 666000;
        myWheel.setSliding();
        assertTrue('1' + myWheel.getSumRoadForceWhenSliding(), myWheel.getSumRoadForceWhenSliding().equals(new Vector2d(200, 0)));
        myWheel.myEngineTorque = 4;
        myWheel.myRetardingTorque = 4;
        myWheel.myVelocity = new Vector2d(0, 10);
        myWheel.myPinAngularVelocity = 100;
        myWheel.setWeight(1.25);
        myWheel.mySlidingFriction = 4;
        var result:Vector2d;
        /*try{
         result = myWheel.getSumRoadForceWhenSliding();
         fail('2');
         }catch(e:StateDataError){	}*/
        myWheel.myPinAngularVelocity = 0;
        result = myWheel.getSumRoadForceWhenSliding();
        assertTrue('3' + result, result.equals(new Vector2d(-4, -3)));
        ////////////////////////////////////////////////////////////////////
        myWheel.myVelocity = Vector2d.getZero();
        myWheel.myPinAngularVelocity = 0;
        myWheel.myEngineTorque = 1000;
        myWheel.myRetardingTorque = 0;
        myWheel.mySlidingFriction = 4;
        myWheel.setWeight(10);
        var res:Vector2d = myWheel.getSumRoadForceWhenSliding();
        assertTrue(res, res.equals(new Vector2d(- 40, 0)));
    }

    public function testDefineAngularVelocity():void {
        myWheel.myVelocity = new Vector2d(2, 2);
        myWheel.tryDefinePinAngular();
        assertTrue(myWheel.myPinAngularVelocity, Utils.equal(myWheel.myPinAngularVelocity, -1));
    }

    public function testCalcVelocity():void {
        // road grip
        myWheel.myEngineTorque = 10000;
        myWheel.myRetardingTorque = -9000;
        myWheel.myVelocity = new Vector2d(2, 0);
        myWheel.myPinAngularVelocity = 0;
        myWheel.myPinMomentOfInertia = 4;
        myWheel.mySlidingFriction = 5;
        myWheel.setSliding();
        myWheel.calcForces();
        myWheel.setSliding();
        myWheel.calcVelocity(2);
        assertTrue(myWheel.myPinAngularVelocity, Utils.equal(400, myWheel.myPinAngularVelocity));
    }

    public function testCalcForce():void {
        //road grip
        myWheel.myEngineTorque = 1;
        myWheel.myRetardingTorque = 0;
        myWheel.myVelocity = new Vector2d(2, 0);
        myWheel.myPinMomentOfInertia = 4;
        myWheel.mySlidingFriction = 5;
        myWheel.myPinMomentOfForce = 100;
        myWheel.setGrip();
        myWheel.calcForces();
        assertTrue(myWheel.myPinMomentOfForce, Utils.isZero(myWheel.myPinMomentOfForce));
        //not road grip
        myWheel.myEngineTorque = 10000;
        myWheel.myRetardingTorque = -9000;
        myWheel.myVelocity = new Vector2d(2, 0);
        myWheel.myPinAngularVelocity = 0;
        myWheel.myPinMomentOfInertia = 4;
        myWheel.mySlidingFriction = 5;
        myWheel.setSliding();
        myWheel.calcForces();
        assertTrue(myWheel.myPinMomentOfForce, Utils.equal(myWheel.myPinMomentOfForce, 800));
    }

    public function testMyVelocity():void {
        myWheel.myEngineTorque = 0;
        myWheel.myRetardingTorque = 0;
        myWheel.myPinAngularVelocity = 0;
        myWheel.myVelocity = new Vector2d(2, 0);
        assertTrue(myWheel.myPinAngularVelocity, Utils.equal(-1, myWheel.myPinAngularVelocity));
    }

    public function testMaxPinForceLength(): void {
        checkNumber(Math.sqrt(1600 - 25), myWheel.getMaxPinForceLength(), "roadGrip");
        myWheel.setSliding();
        checkNumber(Math.sqrt(26 * 26 - 25), myWheel.getMaxPinForceLength(), "sliding");
        myWheel.myEngineTorque = 96;
        checkNumber(0, myWheel.getMaxPinForceLength(), "bigMoment zero velocity");
        myWheel.myVelocity = new Vector2d(1, 1);
        checkNumber(26 * Math.SQRT1_2, myWheel.getMaxPinForceLength(), "bigMoment velocity");
    }

    public function testSumRoadForceWhenSliding(): void {
        myWheel.setSliding();
        checkVector(-5, 0, myWheel.getSumRoadForceWhenSliding(), "nonVelocityForce");
        myWheel.myVelocity.myY = 1;
        checkVector(-5, -Math.sqrt(26 * 26 - 25), myWheel.getSumRoadForceWhenSliding(), "withVelocityForce");
        myWheel.myEngineTorque = 96;
        checkVector(0, -26, myWheel.getSumRoadForceWhenSliding(), "bigMomentForce");
    }
}
}