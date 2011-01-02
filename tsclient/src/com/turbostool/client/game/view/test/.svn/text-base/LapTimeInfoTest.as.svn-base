package com.turbostool.client.game.view.test
{

import com.turbostool.client.game.view.LapTimeInfo;
import com.turbostool.client.net.messages.LapTimeMessage;

import flexunit.framework.TestCase;

public class LapTimeInfoTest extends TestCase
{
    public function LapTimeInfoTest(methodName:String = null)
    {
        super(methodName);
    }

    public function testUpdate_firstLap(): void
    {
        var info: LapTimeInfo = new LapTimeInfo();
        info.update(50, LapTimeMessage.START);
        assertEquals(50, info.startTime);
        assertEquals(0, info.lastTime);
        assertEquals(0, info.bestTime);

        info.update(110, LapTimeMessage.FINISH);

        assertEquals(110, info.startTime);
        assertEquals(60, info.lastTime);
        assertEquals(60, info.bestTime);

        info.update(200, LapTimeMessage.RESET);

        assertEquals(0, info.startTime);
        assertEquals(60, info.lastTime);
        assertEquals(60, info.bestTime);
    }

    public function testUpdate_betterLap(): void
    {
        var info: LapTimeInfo = new LapTimeInfo();
        info.update(50, LapTimeMessage.START);
        info.update(110, LapTimeMessage.FINISH);
        info.update(110, LapTimeMessage.START);
        info.update(150, LapTimeMessage.FINISH);
        info.update(150, LapTimeMessage.START);

        assertEquals(150, info.startTime);
        assertEquals(40, info.lastTime);
        assertEquals(40, info.bestTime);
    }

    public function testUpdate_worseLap(): void
    {
        var info: LapTimeInfo = new LapTimeInfo();
        info.update(50, LapTimeMessage.START);
        info.update(110, LapTimeMessage.FINISH);
        info.update(110, LapTimeMessage.START);
        info.update(300, LapTimeMessage.FINISH);
        info.update(300, LapTimeMessage.START);

        assertEquals(300, info.startTime);
        assertEquals(190, info.lastTime);
        assertEquals(60, info.bestTime);
    }

    public function testUpdate_reset(): void
    {
        var info: LapTimeInfo = new LapTimeInfo();
        info.startTime = 100;
        info.lastTime = 200;
        info.bestTime = 300;

        info.update(400, LapTimeMessage.RESET);

        assertEquals(0, info.startTime);
        assertEquals(200, info.lastTime);
        assertEquals(300, info.bestTime);
    }

    public function testUpdate_unexpectedZeros(): void
    {
        var info: LapTimeInfo = new LapTimeInfo();

        // zero startTime - then finish
        // zero Time - then start -  ignore it
        // zero Time - then finish - ignore it

        const lastTime_: Number = 1000;
        const bestTime_: Number = 900;

        info.startTime = 0;
        info.lastTime = lastTime_;
        info.bestTime = bestTime_;

        info.update(100, LapTimeMessage.FINISH);

        assertEquals(0, info.startTime);
        assertEquals(lastTime_, info.lastTime);
        assertEquals(bestTime_, info.bestTime);

        info.startTime = 100;
        info.update(0, LapTimeMessage.FINISH);

        assertEquals(0, info.startTime);
        assertEquals(lastTime_, info.lastTime);
        assertEquals(bestTime_, info.bestTime);

        info.startTime = 100;
        info.update(0, LapTimeMessage.START);

        assertEquals(0, info.startTime);
        assertEquals(lastTime_, info.lastTime);
        assertEquals(bestTime_, info.bestTime);
    }
}
}