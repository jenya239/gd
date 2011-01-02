package com.turbostool.client.net.messages
{
import com.turbostool.client.game.components.car.RudderControl;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

public class CarStateDataMessage extends ServerRequest
{
    public static const CAR_STATE_MESSAGE: String = "carState";

    //TODO: MAKE PRIVATE
    public var clientID: Number;
    public var myR: Vector2d;
    public var myVelocity: Vector2d;
    public var myAngle: Number;
    public var myAngularVelocity: Number;
//    public var myLateralAngle: Number;
//    public var myLateralAngularVelocity: Number;
//    public var myLongAngle: Number;
//    public var myLongAngularVelocity: Number;
//    public var myRudderAngle: Number = 0.0;
//    public var myRudderAction: int = RudderControl.NONE;
//    public var myIsAccelerate: Boolean;
//    public var myEngineDroselCoef: Number;
//    public var myIsBrake: Boolean;
//    public var myBrakeCoef: Number;
//    public var myIsHandBrake: Boolean;

    public function CarStateDataMessage(clientID: int, r: Vector2d, v: Vector2d, ang: Number, angVel: Number): void//, latAng:Number, latAngVel: Number, longAng: Number, longAngVel: Number, rudderAngle: Number, rudderAction: int, isAccelerate: Boolean, engineDroselCoef: Number, isBrake: Boolean, brakeCoef: Number, isHandBrake: Boolean)
    {

        //super(CAR_STATE_MESSAGE, sessionId);
        super(CAR_STATE_MESSAGE);

        this.clientID = clientID;
        myR = r;
        myVelocity = v;
        myAngle = ang;
        myAngularVelocity = angVel;
//        myLateralAngle = latAng;
//        myLateralAngularVelocity = latAngVel;
//        myLongAngle = longAng;
//        myLongAngularVelocity = longAngVel;
//        this.myRudderAngle = rudderAngle;
//        this.myRudderAction = rudderAction;
//        this.myIsAccelerate = isAccelerate;
//        this.myEngineDroselCoef = engineDroselCoef;
//        this.myIsBrake = isBrake;
//        this.myBrakeCoef = brakeCoef;
//        this.myIsHandBrake = isHandBrake;
    }

    override public function toString(): String
    {
        return CAR_STATE_MESSAGE + ': ' + 'myR = ' + myR + '; ' + 'myVelocity = ' + myVelocity + '; ' + 'myAngle = ' + myAngle + '; ' + 'myAngularVelocity = ' + myAngularVelocity + '; '; //+ 'myLateralAngle = ' + myLateralAngle + '; ' + 'myLateralAngularVelocity = ' + myLateralAngularVelocity + '; ' + 'myLongAngle = ' + myLongAngle + '; ' + 'myLongAngularVelocity = ' + myLongAngularVelocity + ';' + 'myRudderAngle = ' + myRudderAngle + ';' + 'myRrudderAction = ' + myRudderAction + ';' + 'myIsAccelerate = ' + myIsAccelerate + ';' + 'myEngineDroselCoef = ' + myEngineDroselCoef + ';' + 'myIsBrake = ' + myIsBrake + ';' + 'myBrakeCoef = ' + myBrakeCoef + ';' + 'myIsHandBrake = ' + myIsHandBrake + '.';
    }

    public function getR(): Vector2d
    {
        return myR;
    }

    public function getVelocity(): Vector2d
    {
        return myVelocity;
    }

    public function getAngle(): Number
    {
        return myAngle;
    }

    public function getAngularVelocity(): Number
    {
        return myAngularVelocity;
    }

//    public function getLateralAngle(): Number
//    {
//        return myLateralAngle;
//    }
//
//    public function getLateralAngularVelocity(): Number
//    {
//        return myLateralAngularVelocity;
//    }
//
//    public function getLongAngle(): Number
//    {
//        return myLongAngle;
//    }
//
//    public function getLongAngularVelocity(): Number
//    {
//        return myLongAngularVelocity;
//    }
//
//    public function getRudderAngle(): Number
//    {
//        return myRudderAngle;
//    }
//
//    public function getRudderAction(): int
//    {
//        return myRudderAction;
//    }
//
//    public function getIsAccelerate(): Boolean
//    {
//        return myIsAccelerate;
//    }
//
//    public function getEngineDroselCoef(): Number
//    {
//        return myEngineDroselCoef;
//    }
//
//    public function getIsBrake(): Boolean
//    {
//        return myIsBrake;
//    }
//
//    public function getBrakeCoef(): Number
//    {
//        return myBrakeCoef;
//    }
//
//    public function getIsHandBrake(): Boolean
//    {
//        return myIsHandBrake;
//    }

    public function equals(obj: CarStateDataMessage): Boolean
    {
        if (obj == this)
            return true;

        var other: CarStateDataMessage = obj as CarStateDataMessage;
        if (other == null)
            return false;

        return this.myR.equals(other.myR )
                && this.myVelocity.equals(other.myVelocity)
                && Utils.equal(this.myAngle, other.myAngle)
                && Utils.equal(this.myAngularVelocity, other.myAngularVelocity);
//                && Utils.equal(this.myLateralAngle, other.myLateralAngle)
//                && Utils.equal(this.myLateralAngularVelocity, other.myLateralAngularVelocity)
//                && Utils.equal(this.myLongAngle, other.myLongAngle)
//                && Utils.equal(this.myLongAngularVelocity, other.myLongAngularVelocity)
//                && Utils.equal(this.myRudderAngle, other.myRudderAngle)
//                && this.myRudderAction == other.myRudderAction;
    }

}
}