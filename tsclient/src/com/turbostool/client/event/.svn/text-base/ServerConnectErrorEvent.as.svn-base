package com.turbostool.client.event
{
import flash.events.Event;

public class ServerConnectErrorEvent extends Event
{
    public static const SERVER_CONNECT_ERROR: String = "serverConnectError";

    private var _innerEvent: Event;

    public function ServerConnectErrorEvent(innerEvent: Event)
    {
        super(SERVER_CONNECT_ERROR);
        _innerEvent = innerEvent;
    }

    public function get innerEvent(): Event
    {
        return _innerEvent;
    }

}
}