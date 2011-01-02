package com.turbostool.client.game.components.car.test   {

import com.turbostool.client.game.components.car.EngineParameters;
import com.turbostool.client.utils.Utils;

import flexunit.framework.TestCase;

public class EngineParametersTest extends TestCase
{
    private var myParm:EngineParameters;

    public function EngineParametersTest(methodName:String = null)
    {
        super(methodName);
    }

    public function testGetMoment():void {
        var parm:EngineParameters = new EngineParameters();
        parm.myEndMaxMomentVelocity = 2;
        parm.myEndTorque = 0;
        parm.myMaxTorque = 2;
        parm.myMaxVelocity = 3;
        parm.myStartMaxMomentVelocity = 1;
        parm.myStartTorque = 1;
        myParm = parm;
        checkMoment(-1, 0);
        checkMoment(-Utils.EPSILON / 2, 1);
        checkMoment(0, 1);
        checkMoment(0.5, 1.5);
        checkMoment(1, 2);
        checkMoment(1.2, 2);
        checkMoment(1.78, 2);
        checkMoment(2, 2);
        checkMoment(2.5, 1);
        checkMoment(3, 0);
        checkMoment(4, 0);
    }

    private function checkMoment(w:Number, exp:Number):void {
        var real:Number = myParm.getMoment(w);
        assertTrue(' check moment w = ' + w + ' moment = ' + real + 'expexted ' + exp, Utils.equal(exp, real));
    }
}
}