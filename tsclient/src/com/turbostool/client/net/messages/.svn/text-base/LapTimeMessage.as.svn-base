package com.turbostool.client.net.messages
{
public class LapTimeMessage extends ChanneledMessage
{
    public static const LAP_TIME: String = "lapTime";

    public static const START: String = "start";
    public static const FINISH: String = "finish";
    public static const RESET: String = "reset";
    public static const LAP: String = "lap";

    private var _time: Number;
    private var _type: String;
    private var _clientID: int;

    public function LapTimeMessage(type: String = "", time: Number = 0, clientID: int = -1, channel: String = "")
    {
        super("", LAP_TIME);
        _type = type;
        _time = time;
        _clientID = clientID;
        this.channel = channel;
    }

    [Serializable(order=2)]
    public function get type(): String
    {
        return _type;
    }

    [Serializable(order=3)]
    public function get time(): Number
    {
        return _time;
    }

    public function get clientID(): int
    {
        return _clientID;
    }

    public function set time(value:Number):void
    {
        _time = value;
    }

    public function set type(value:String):void
    {
        _type = value;
    }

    public function set clientID(value:int):void
    {
        _clientID = value;
    }
}
}