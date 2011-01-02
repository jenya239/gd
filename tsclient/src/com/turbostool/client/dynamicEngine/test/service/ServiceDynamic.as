package com.turbostool.client.dynamicEngine.test.service
{
import com.turbostool.client.utils.CallChecker;

public class ServiceDynamic extends CallChecker implements IDynamic
{
    public function calcForces():void
    {
        check('calcForces');
    }

    public function calcVelocity(dt:Number):void
    {
        check('calcVelocity');
    }

    public function rollback():void
    {
        check('rollback');
    }

    public function saveState():void
    {
        check('saveState');
    }

    public function step(dt:Number):void
    {
        check('step');
    }

    public function calcCoordinates(dt:Number):void
    {
        check('calcCoordinates');
    }

    public function nullForces():void
    {
        check('nullForces');
    }

}
}