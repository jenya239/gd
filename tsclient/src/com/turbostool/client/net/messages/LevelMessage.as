package com.turbostool.client.net.messages
{
public class LevelMessage extends ChanneledMessage
{
    public static const LEVEL: String = "level";

    private var _level: int;
    private var _expLeft: int;
    private var _message: String;

    public function LevelMessage()
    {
        super(ChanneledMessage.RACE_CHANNEL);
    }

    public function get level():int
    {
        return _level;
    }

    public function set level(val:int):void
    {
        _level = val;
    }

    public function get expLeft():int
    {
        return _expLeft;
    }

    public function set expLeft(val:int):void
    {
        _expLeft = val;
    }

    public function get message():String
    {
        return _message;
    }

    public function set message(val:String):void
    {
        _message = val;
    }
}
}