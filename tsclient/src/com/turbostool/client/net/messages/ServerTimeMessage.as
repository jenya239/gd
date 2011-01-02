package com.turbostool.client.net.messages
{
public class ServerTimeMessage
{
    public static const SERVER_TIME: String = "serverTime";

    private var _time: Number;
    private var _adv: String;

    public function get time():Number
    {
        return _time;
    }

    public function set time(val:Number):void
    {
        _time = val;
    }

    public function get adv():String
    {
        return _adv;
    }

    public function set adv(val:String):void
    {
        _adv = val;
    }
}
}