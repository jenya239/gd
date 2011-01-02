package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Utils;

import flash.events.Event;

public class CarControlEvent extends Event {
    public static const CAR_CONTROL: String = 'carControl';
    private static const DELIMETER: String = ',';

    public static const GEAR_DOWN: int = -1;
    public static const GEAR_NONE: int = 0;
    public static const GEAR_UP: int = 1;

    public var myAccelerate:Boolean;
    public var myRudderControl:int;
    public var myBrake:Boolean;
    public var myHandBrake:Boolean;
    public var myRudderAngle: Number;
    public var myGearBoxAction: int;
    public var myClutchChange: Boolean;
    public var myNitro: Boolean;

    public function CarControlEvent(accelerate:Boolean, rudderAction:int, brake:Boolean, handBrake:Boolean, rudderAngle:Number, gearBoxAction: int, clutch: Boolean = false, nitro: Boolean = false) {
        super(CAR_CONTROL);
        myAccelerate = accelerate;
        myRudderControl = rudderAction;
        myBrake = brake;
        myHandBrake = handBrake;
        myRudderAngle = rudderAngle;
        myGearBoxAction = gearBoxAction;
        myClutchChange = clutch;
        myNitro = nitro;
    }

    public function encode() : String {
        return Utils.boolToFixedString(myAccelerate)
                + DELIMETER + myRudderControl
                + DELIMETER + Utils.boolToFixedString(myBrake)
                + DELIMETER + Utils.boolToFixedString(myHandBrake)
                + DELIMETER + myRudderAngle
                + DELIMETER + myGearBoxAction
                + DELIMETER + Utils.boolToFixedString(myClutchChange)
                + DELIMETER + Utils.boolToFixedString(myNitro);
    }

    public static function decode(s: String): CarControlEvent {
        var tokens: Array = s.split(DELIMETER);
        return new CarControlEvent(
                Utils.str2bool(tokens[0]),
                parseInt(tokens[1]),
                Utils.str2bool(tokens[2]),
                Utils.str2bool(tokens[3]),
                parseFloat(tokens[4]),
                parseInt(tokens[5]),
                Utils.str2bool(tokens[6]),
                Utils.str2bool(tokens[7])
                );
    }

    public function equals(o: Object): Boolean {
        if (o == null) return false;
        if (this == o) return true;
        if (!(o is CarControlEvent)) return false;
        var e: CarControlEvent = o as CarControlEvent;
        return myAccelerate == e.myAccelerate
                && myBrake == e.myBrake
                && myHandBrake == e.myHandBrake
                && myRudderControl == e.myRudderControl
                && Utils.equal(myRudderAngle, e.myRudderAngle)
                && myGearBoxAction == e.myGearBoxAction
                && myClutchChange == e.myClutchChange
                && myNitro == e.myNitro;
    }

    public static function equals2(o1: CarControlEvent, o2: CarControlEvent): Boolean {
        if (o1 == null && o2 == null) return true;
        if (o1 != null)
            return o1.equals(o2);
        else
            return o2.equals(o1);
    }

    override public function clone(): Event {
        return new CarControlEvent(myAccelerate, myRudderControl, myBrake, myHandBrake, myRudderAngle, myGearBoxAction, myClutchChange, myNitro);
    }
}
}