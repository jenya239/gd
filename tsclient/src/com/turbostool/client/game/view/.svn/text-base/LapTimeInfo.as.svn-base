package com.turbostool.client.game.view
{
import com.turbostool.client.net.messages.LapTimeMessage;

public class LapTimeInfo
{
    public var clientID: Number;
    //		public var position: Number;
    public var displayName: String;
    //		public var currentTime: Number;
    public var bestTime: Number = 0;
    public var lastTime: Number = 0;
    public var startTime: Number = 0;

    public function LapTimeInfo()
    {

    }

    public static function compare(obj1: LapTimeInfo, obj2: LapTimeInfo, fields:Array = null): int
    {
        if (obj1.bestTime == obj2.bestTime) return 0;
        if (obj1.bestTime == 0 && obj2.bestTime > 0) return 1;
        if (obj1.bestTime > 0 && obj2.bestTime == 0) return -1;
        if (obj1.bestTime < obj2.bestTime) return -1;
        return 1;
    }

    public function update(time: Number, eventType: String): Boolean
    {
        var isBestTime: Boolean = false;

        if (eventType == LapTimeMessage.RESET)
        {
            startTime = 0;
        }
        else
            if (eventType == LapTimeMessage.START)
            {
                startTime = time;
            }
            else
                if (eventType == LapTimeMessage.FINISH)
                {
                    if (time > 0 && startTime > 0)
                    {
                        lastTime = time - startTime;
                        if (bestTime == 0)
                        {
                            bestTime = lastTime;
                        }
                        else
                            if (lastTime < bestTime)
                            {
                                bestTime = lastTime;
                                isBestTime = true;
                            }
                        startTime = time;
                    }
                    else
                    {
                        startTime = 0;
                    }
                }
        return isBestTime;
    }
}
}