package com.turbostool.client.event
{
import com.turbostool.client.net.messages.LapTimeMessage;

import flash.events.Event;

public class LocalLapTimeEvent extends Event
{
    public static const LOCAL_LAP_TIME: String = "localLapTimeEvent";

    private var _lapTimeMessage: LapTimeMessage;

    public function LocalLapTimeEvent(lapTimeMessage: LapTimeMessage)
    {
        super(LOCAL_LAP_TIME);
        _lapTimeMessage = lapTimeMessage;
    }

    public function get lapTimeMessage():LapTimeMessage
    {
        return _lapTimeMessage;
    }
}
}