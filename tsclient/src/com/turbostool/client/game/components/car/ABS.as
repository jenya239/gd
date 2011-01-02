package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

public class ABS {
    private var myWheels:Collection;
    private var myCoef:Number = 0.99;

    public function ABS(wheels:Collection) {
        myWheels = wheels;
    }

    public function correctBrakeTorque():void {
        var it:Iterator = myWheels.iterator();
        while (it.hasNext()) {
            var wheel:Wheel = it.next() as Wheel;
            var maxFriction:Number = wheel.myWeight * wheel.myStaticFriction;
            if (Math.abs(wheel.myRetardingTorque) > maxFriction * myCoef * wheel.myRadius) {
                wheel.myRetardingTorque = maxFriction * myCoef * Utils.sign(wheel.myRetardingTorque) * wheel.myRadius;
            }
        }
    }
}
}