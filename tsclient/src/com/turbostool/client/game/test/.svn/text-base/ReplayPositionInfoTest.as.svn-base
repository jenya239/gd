package com.turbostool.client.game.test {

import com.turbostool.client.game.ReplayPositionInfo;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;

public class ReplayPositionInfoTest extends TestCase {

    public function ReplayPositionInfoTest(methodName:String = null) {
        super(methodName);
    }

    public function testEncoding():void {
        var pos: ReplayPositionInfo = new ReplayPositionInfo(new Vector2d(1.11, 2.22), 3.4444);
        var pos2: ReplayPositionInfo = ReplayPositionInfo.decode(pos.encode());
        assertTrue(pos.equals(pos2));
    }

}
}