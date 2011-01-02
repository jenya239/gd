package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Utils;

public class Brakes{
    private static const MIN_VELOCITY: Number = 0.1;
    private var myChassis:Chassis
    private var myTorqueProperty:Number;
    private var myHandTorque:Number;

    private var myActiveProperty:Boolean;
    private var myHandBrakeProperty:Boolean;

    private var myCoef:Number;//0-1
    private var myVelocity:Number;
    public  var myBaseVelocity:Number;

    public function Brakes(chassis:Chassis, brakeTorque:Number, handTorque:Number = 1000) {
        myTorqueProperty = brakeTorque;
        myHandTorque = handTorque;
        myActiveProperty = false;
        myChassis = chassis;
        myCoef = 0;
        myVelocity = 0;
        myBaseVelocity = 0.5;
    }

    public function calcForces():void {
        var wheel:Wheel;
        myChassis.myRearLeft.myRetardingTorque = 0;
        myChassis.myRearRight.myRetardingTorque = 0;
        myChassis.myFrontLeft.myRetardingTorque = 0;
        myChassis.myFrontRight.myRetardingTorque = 0;
        if (myHandBrake || myActive) {
            if (myChassis.getHull().myVelocity.length() < 0.1) {
                myChassis.fullStop();
            } else {
                if (myHandBrake) {
                    setTorqueToWheel(myChassis.myRearLeft, myHandTorque);
                    setTorqueToWheel(myChassis.myRearRight, myHandTorque);
                }
                if (myActive) {
                    setTorqueToWheel(myChassis.myRearLeft, myTorque);
                    setTorqueToWheel(myChassis.myRearRight, myTorque);
                    setTorqueToWheel(myChassis.myFrontLeft, myTorque);
                    setTorqueToWheel(myChassis.myFrontRight, myTorque);
                }
            }
        }
    }

    public function calcVelocity(dt:Number):void {
        myVelocity = myActive ? myBaseVelocity : - 2 * myBaseVelocity;
    }

    public function calcCoordinates(dt:Number):void {
        myCoef += myVelocity * dt;
        myCoef = Utils.getNear(myCoef, 0, 1);
    }

    private function setTorqueToWheel(wheel:Wheel, torque:Number):void {
        wheel.myRetardingTorque = - Utils.sign(wheel.myPinAngularVelocity) * torque;
    }

    public function get myBrakeMoment():Number {
        return myTorqueProperty;
    }

    public function set myBrakeMoment(torque:Number):void {
        myTorqueProperty = torque;
    }

    public function getCoef(): Number {
        return myCoef;
    }

    public function setCoef(value: Number): void {
        myCoef = value;
    }

    private function get myTorque():Number {
        return myTorqueProperty * myCoef;
    }

    public function get myHandMoment():Number {
        return myHandTorque;
    }

    public function set myHandMoment(torque:Number):void {
        myHandTorque = torque;
    }

    public function get myActive():Boolean {
        return myActiveProperty;
    }

    public function set myActive(setValue:Boolean):void {
        myActiveProperty = setValue;
    }

    public function get myHandBrake():Boolean {
        return myHandBrakeProperty;
    }

    public function set myHandBrake(setValue:Boolean):void {
        myHandBrakeProperty = setValue;
    }
}
}