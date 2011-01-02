package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

public class RudderControl{

    public static const LEFT:int = -1;
    public static const RIGHT:int = 1;
    public static const NONE:int = 0;
    public static const ANGLE: int = 2;

    private var myChassis:Chassis;
    private var myBaseVelocityProperty:Number;
    private var myAngularVelocity:Number;
    private var myMaxAngleProperty:Number;
    private var myActionProperty:int;
    private var myBaseReturnVelocityProperty:Number;

    public function RudderControl(chassis:Chassis, maxAngle:Number, baseVelocity:Number) {
        myChassis = chassis;
        myAngularVelocity = 0;
        myAngle = 0;
        myActionProperty = RudderControl.NONE;
        myBaseVelocity = baseVelocity;
        myMaxAngle = maxAngle;
        myBaseReturnVelocityProperty = baseVelocity;
    }

    private function getVelocity():Number {
        return myBaseVelocity * ( 1 - Math.abs(myAngle) / myMaxAngleProperty  );
    }

    public function calcVelocity(dt:Number):void {
        switch (myAction) {
            case RudderControl.LEFT:
                if (myAngle > 0) {
                    myAngularVelocity = - Utils.sign(myAngle) * myBaseReturnVelocityProperty;
                } else {
                    myAngularVelocity = - getVelocity();
                }
                break;
            case RudderControl.RIGHT:
                if (myAngle < 0) {
                    myAngularVelocity = - Utils.sign(myAngle) * myBaseReturnVelocityProperty;
                } else {
                    myAngularVelocity = getVelocity();
                }
                break;
            case RudderControl.NONE:
                myAngularVelocity = - Utils.sign(myAngle) * myBaseReturnVelocityProperty;
                break;
            case RudderControl.ANGLE:
                myAngularVelocity = 0;
                break;
        }
    }

    public function calcCoordinates(dt:Number):void {
        var da:Number = myAngularVelocity * dt;
        if ((Math.abs(myAngle) < Math.abs(da)) && (myAngle * da < 0)) {
            myAngle = 0;
        } else {
            myAngle += da;
        }
    }

    public function get myAngle():Number {
        return myChassis.myRudderAngle;
    }

    public function set myAngle(setValue:Number):void {
        myChassis.myRudderAngle = Utils.getNear(setValue, - myMaxAngle, myMaxAngle);
    }

    public function get myBaseVelocity():Number {
        return myBaseVelocityProperty;
    }

    public function set myBaseVelocity(setValue:Number):void {
        if (setValue < 0) throw new TSError('baseVelocity должна быть больше нуля');
        myBaseVelocityProperty = setValue;
    }

    public function get myBaseReturnVelocity():Number {
        return myBaseReturnVelocityProperty;
    }

    public function set myBaseReturnVelocity(setValue:Number):void {
        if (setValue < 0) throw new TSError('baseReturnVelocity должна быть больше нуля');
        myBaseReturnVelocityProperty = setValue;
    }

    public function get myMaxAngle():Number {
        return myMaxAngleProperty;
    }

    public function set myMaxAngle(setValue:Number):void {
        if (setValue < 0) throw new TSError('maxAngle должен быть больше нуля');
        myMaxAngleProperty = setValue;
        myAngle = myAngle;
    }

    public function get myAction():int {
        return myActionProperty;
    }

    public function set myAction(setValue:int):void {
        myActionProperty = setValue;
    }

}
}