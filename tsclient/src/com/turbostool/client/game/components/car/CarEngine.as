package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Utils;

public class CarEngine {
    public  var myAngularVelocity: Number = 0;
    private var myEngineCoefProperty:Number = 0;
    public  var myParameters:EngineParameters = new EngineParameters();
    private var myMomentOfInertiaProperty: Number = 0.5;
    private var myFrictionCoef: Number = 0.001;

    public function getMaxAngularVelocity():Number {
        return myParameters.myMaxVelocity;
    }

    public function get myMomentOfInertia():Number {
        return myMomentOfInertiaProperty;
    }

    public function get myEngineCoef():Number {
        return myEngineCoefProperty;
    }

    public function set myEngineCoef(setValue:Number):void {
        myEngineCoefProperty = setValue;
    }

    public function getExternalMoment():Number {
        return myEngineCoef * myParameters.getMoment(myAngularVelocity);
    }

    public function step(dt: Number): void {
        var dw: Number = getExternalMoment() * dt / myMomentOfInertia - myFrictionCoef * myAngularVelocity;
        if (Utils.inRange(0, myAngularVelocity, myAngularVelocity + dw)) {
            myAngularVelocity = 0;
        } else {
            myAngularVelocity += dw;
        }
    }

}
}