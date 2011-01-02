package com.turbostool.client.utils.test
{
import com.turbostool.client.utils.TSError;

public class TSErrorTest extends TestCase {
    public function TSErrorTest(methodName:String = null) {
        super(methodName);
    }

    public function testTSError():void {
        var catched:Boolean = false;
        try {
            throw new TSError('hell');
        } catch (error:TSError) {
            catched = true;
        }
        assertTrue(catched);
    }


}
}