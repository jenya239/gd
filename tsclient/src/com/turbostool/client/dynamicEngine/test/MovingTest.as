package com.turbostool.client.dynamicEngine.test
{
import com.turbostool.client.utils.*;

public class MovingTest extends TestCase
{
    private var myMoving:IMoving;

    public function MovingTest(m:IMoving) {
        myMoving = m;
    }

    public function testIMoving():void {
        myMoving.myMass = 4.4;
        assertTrue(Utils.equal(4.4, myMoving.myMass));
        myMoving.myForce = new Vector3d(5.5, 3, 3.2);
        assertTrue(Utils.equal(4.4, myMoving.myMass));
        assertTrue(myMoving.myForce.equals(new Vector3d(5.5, 3, 3.2)));
        myMoving.setR(new Vector3d(5.5, 3, 3));
        assertTrue(Utils.equal(4.4, myMoving.myMass));
        assertTrue(myMoving.myForce.equals(new Vector3d(5.5, 3, 3.2)));
        assertTrue(myMoving.myR.equals(new Vector3d(5.5, 3, 3)));
        myMoving.myVelocity = new Vector3d(5, 1, 3);
        assertTrue(Utils.equal(4.4, myMoving.myMass));
        assertTrue(myMoving.myForce.equals(new Vector3d(5.5, 3, 3.2)));
        assertTrue(myMoving.myR.equals(new Vector3d(5.5, 3, 3)));
        assertTrue(myMoving.myVelocity.equals(new Vector3d(5, 1, 3)));
    }


}
}