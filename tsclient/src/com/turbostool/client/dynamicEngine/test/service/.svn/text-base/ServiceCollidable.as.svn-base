package com.turbostool.client.dynamicEngine.test.service
{
import com.turbostool.client.utils.CallChecker;

public class ServiceCollidable extends CallChecker implements ICollidable
{
    public var collision:Boolean = false;

    public function rollback():void
    {
        check('rollback');
    }

    public function tryCollision(collidable:ICollidable):Boolean
    {
        check('tryCollision');
        return collision;
    }

}
}