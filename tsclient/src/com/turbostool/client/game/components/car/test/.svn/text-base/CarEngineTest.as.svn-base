package com.turbostool.client.game.components.car.test {

import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;

public class CarEngineTest extends TestCase {
    private var myEngine:CarEngine;

    public function CarEngineTest(methodName:String = null) {
        super(methodName);
    }

    override public function setUp():void {
        myEngine = new CarEngine();
    }

    public function testCarEngine():void {
        myEngine.myEngineCoef = 0.5;
        myEngine.myAngularVelocity = 400;
        assertTrue(myEngine.getExternalMoment(),
                Utils.equal(160, myEngine.getExternalMoment())
                );
    }


}
}