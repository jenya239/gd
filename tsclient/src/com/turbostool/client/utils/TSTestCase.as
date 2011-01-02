package com.turbostool.client.utils
{

import flexunit.framework.TestCase;

public class TSTestCase extends TestCase
{
    public function TSTestCase(methodName:String = null)
    {
        super(methodName);
    }

    protected function checkNumber(expected:Number, value:Number, name:String):void {
        assertTrue(name + ' = ' + value + ' а не ' + expected, Utils.equal(expected, value));
    }

    protected function checkVector(expectedX: Number, expectedY: Number, value: Vector2d, name: String): void {
        checkNumber(expectedX, value.myX, name + ".x");
        checkNumber(expectedY, value.myY, name + ".y");
    }

}
}