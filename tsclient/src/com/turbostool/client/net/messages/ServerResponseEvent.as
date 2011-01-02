package com.turbostool.client.net.messages
{
import flash.events.Event;
import flash.events.Event;
import flash.events.Event;

public class ServerResponseEvent extends Event implements IChanneledMessage
{
    private var _serverResponse: Object;

    public function ServerResponseEvent(type: String, serverResponse: Object)
    {
        super(type);
        _serverResponse = serverResponse;
        // make assert
    }

    public function get response(): Object
    {
        return _serverResponse;
    }

    public function get channel(): String
    {
        if (_serverResponse is IChanneledMessage)
        {
            return IChanneledMessage(_serverResponse).channel;
        } else
        {
            return ChanneledMessage.GLOBAL_CHANNEL;
        }
    }

    override public function clone():Event
    {
        return new ServerResponseEvent(type, _serverResponse);
    }
}
}