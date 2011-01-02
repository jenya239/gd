package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Utils;

[Bindable]
public class EngineParameters {
    public var myStartTorque:Number = 150;
    public var myMaxTorque:Number = 320;
    public var myEndTorque:Number = 200;

    public var myStartMaxMomentVelocity:Number = 200;
    public var myEndMaxMomentVelocity:Number = 1400;
    public var myMaxVelocity:Number = 1500;

    public function getMoment(w:Number):Number {
        if (w < - Utils.EPSILON) {
            return 0;
        }
        if (w < myStartMaxMomentVelocity) {
            return ( myStartTorque * (myStartMaxMomentVelocity - w) + myMaxTorque * w  ) / myStartMaxMomentVelocity;
        }
        if (w <= myEndMaxMomentVelocity) {
            return myMaxTorque;
        }
        if (w < myMaxVelocity) {
            return ( myMaxTorque * (myMaxVelocity - w) + myEndTorque * (w - myEndMaxMomentVelocity)  )
                    /
                   ( myMaxVelocity - myEndMaxMomentVelocity );
        }
        return 0;
    }

}
}