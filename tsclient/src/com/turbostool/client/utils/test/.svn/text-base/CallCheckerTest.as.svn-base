package com.turbostool.client.utils.test
{
import com.turbostool.client.utils.CallChecker;

public class CallCheckerTest extends TestCase {
    public function CallCheckerTest(methodName:String = null) {
        super(methodName);
    }

    public function testCallChecker():void {
        var checker:CallChecker = new CallChecker();
        assertEquals(checker.getCalls('babba').length, 0);
        checker.check('babba');
        checker.check('babba');
        checker.check('babba');
        checker.check('babba');
        checker.check('babba');
        checker.check('RrRrUn!');
        assertEquals(checker.getCalls('babba').length, 5);
        assertEquals(checker.getCalls('RrRrUn!').length, 1);
        assertEquals(checker.getCalls('ddd_ddd').length, 0);
        var calls:Array = checker.getCalls('babba');
        assertTrue(calls[0] <= calls[4]);
    }

}
}