package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

public class Differential    {
    public static const REAR_DRIVE:Number = 1;
    public static const FRONT_DRIVE:Number = 0;
    public static const FULL_DRIVE:Number = 0.5;

    private var myChassis:Chassis;
    private var myRearRelativeTorqueProperty:Number; //[0, 1]

    public function Differential(wheels:Chassis, rearRelativeTorque:Number) {
        myChassis = wheels;
        myRearRelativeTorque = rearRelativeTorque;
    }

    public function get myRearRelativeTorque():Number {
        return myRearRelativeTorqueProperty;
    }

    public function set myRearRelativeTorque(t:Number):void {
        if (Utils.nonEqual(t, REAR_DRIVE) && Utils.nonEqual(t, FRONT_DRIVE) && Utils.nonEqual(t, FULL_DRIVE)) {
            throw new TSError('непонятный тип привода ' + t);
        }
        myRearRelativeTorqueProperty = t;
    }

    public function getAverageAngularVelocity():Number {
        var front:Number = myChassis.myFrontLeft.myPinAngularVelocity
                + myChassis.myFrontRight.myPinAngularVelocity;
        var rear:Number = myChassis.myRearLeft.myPinAngularVelocity
                + myChassis.myRearRight.myPinAngularVelocity;
        return (rear * myRearRelativeTorque + front * (1 - myRearRelativeTorque)) / 2;
    }

    public function setTorque(torque:Number):void {
        var front:Number = (1 - myRearRelativeTorque) * torque;
        var rear:Number = myRearRelativeTorque * torque;
        myChassis.myFrontLeft.myEngineTorque = front / 2;
        myChassis.myFrontRight.myEngineTorque = front / 2;
        myChassis.myRearRight.myEngineTorque = rear / 2;
        myChassis.myRearLeft.myEngineTorque = rear / 2;
    }
}
}