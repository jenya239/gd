package com.turbostool.client.net.messages
{
public class ChanneledMessage extends ServerRequest implements IChanneledMessage
{
    public static const RACE_CHANNEL: String = "race";
    public static const GLOBAL_CHANNEL: String = "global";
    public static const LOBBY_CHANNEL: String = "lobby";
    public static const CITY_CHANNEL: String = "city";
    public static const TRADE_CHANNEL: String = "trade";
    public static const PRIVATE_CHANNEL: String = "private";

    private var _channel: String

    [Serializable(order=1)]
    public function get channel(): String
    {
        return _channel;
    }

    public function set channel(value:String):void
    {
        _channel = value;
    }

    public function ChanneledMessage(channel: String, name: String = "")
    {
        super(name);
        _channel = channel;
    }
}
}