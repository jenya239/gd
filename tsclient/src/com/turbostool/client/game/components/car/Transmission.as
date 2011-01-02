package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.TSError;

public class Transmission {
    private var myEngine:CarEngine;
    private var myNumberProperty:int;
    private var myCoefs:Array;
    public  var myBaseGear:Number;
    public var myAuto: Boolean = true;

    public function Transmission(engine:CarEngine, wheelCoefs:Array, baseGear:Number = 1) {
        if (wheelCoefs.length < 2) {
            throw new TSError('должно существовать по крайней мере две передачи');
        }
        myEngine = engine;
        myCoefs = wheelCoefs;
        myNumberProperty = 1;
        myBaseGear = baseGear;
    }

    public function get myNumber():int {
        return myNumberProperty;
    }

    public function validGearNumber(value: int): Boolean {
        return value >= 0 && value < myCoefs.length;
    }

    public function set myNumber(setValue:int):void {
        if (!validGearNumber(setValue)) {
            throw new TSError(setValue + ' передачи нет');
        }
        myNumberProperty = setValue;
    }

    public function get myMaxNumber():int {
        return myCoefs.length - 1;
    }

    public function checkGear():void {
        if (!myAuto) return;
        if ((myEngine.myAngularVelocity < myEngine.myParameters.myStartMaxMomentVelocity) && (myNumber > 1)) {
            myNumber--;
        } else if ((myEngine.myAngularVelocity > myEngine.myParameters.myEndMaxMomentVelocity) && (myNumber < myMaxNumber) && myNumber != 0) {
            myNumber++;
        }
    }

    public function getRollerTorque():Number {
        return myEngine.getExternalMoment() * getCoef();
    }

    public function setEngineVelocity(rollerVelocity:Number):void {
        myEngine.myAngularVelocity = rollerVelocity * getCoef();
    }

    public function getCoef():Number {
        return myBaseGear * myCoefs[myNumber];
    }

    public function get maxCoef(): Number {
        return myBaseGear * myCoefs[myMaxNumber];
    }

    public function getEngine():CarEngine {
        return myEngine;
    }

    public function setEngine(value: CarEngine): void {
        myEngine = value;
    }

    public function get myTransmissionCoefs():Array {
        return myCoefs;
    }

    public function set myTransmissionCoefs(coefs:Array):void {
        myCoefs = coefs;
    }
}
}