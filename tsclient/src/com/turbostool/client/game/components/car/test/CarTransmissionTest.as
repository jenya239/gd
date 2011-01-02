package com.turbostool.client.game.components.car.test
{

import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class CarTransmissionTest extends TestCase {
    private var myTransmission:Transmission;
    private var myEngine:CarEngine;

    public function CarTransmissionTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new CarTransmissionTest("testCarTransmission"));
        ts.addTest(new CarTransmissionTest("testSetVelocity"));
        ts.addTest(new CarTransmissionTest("testGetRollerTorque"));
        return ts;
    }

    override public function setUp():void {
        myEngine = new CarEngine();
        myTransmission = new Transmission(myEngine,
                new Array(10, 5, 1 / 0.3), 2
                );
    }

    public function testCarTransmission():void {
        myTransmission.myNumber = 2;
        assertTrue(myTransmission.getCoef(), Utils.equal(2 / 0.3, myTransmission.getCoef()));
    }

    public function testSetVelocity():void {
        myTransmission.myNumber = 1;
        myTransmission.setEngineVelocity(10);
        assertTrue(myEngine.myAngularVelocity, Utils.equal(100, myEngine.myAngularVelocity));
    }

    public function testGetRollerTorque():void {
        myTransmission.myNumber = 1;
        myEngine.myEngineCoef = 1;
        assertTrue(myTransmission.getRollerTorque(), Utils.equal(1500, myTransmission.getRollerTorque()));
    }

}
}