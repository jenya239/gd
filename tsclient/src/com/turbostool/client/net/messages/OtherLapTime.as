package com.turbostool.client.net.messages
{
public class OtherLapTime extends ChanneledMessage
{
    private var _clientID: int;
    private var _startTime: int;
    private var _lastTime: int;
    private var _bestTime: int;

    public function OtherLapTime()
    {
        super(channel);
    }

    public function get clientID():int
    {
        return _clientID;
    }

    public function set clientID(val:int):void
    {
        _clientID = val;
    }

    public function get startTime():int
    {
        return _startTime;
    }

    public function set startTime(val:int):void
    {
        _startTime = val;
    }

    public function get lastTime():int
    {
        return _lastTime;
    }

    public function set lastTime(val:int):void
    {
        _lastTime = val;
    }

    public function get bestTime():int
    {
        return _bestTime;
    }

    public function set bestTime(val:int):void
    {
        _bestTime = val;
    }
}
}