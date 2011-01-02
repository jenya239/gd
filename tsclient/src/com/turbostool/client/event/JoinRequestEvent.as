package com.turbostool.client.event
{
import flash.events.Event;

public class JoinRequestEvent extends Event
{
    public static const JOIN_REQUEST: String = "join";
    private var _clientID: int;

    public function JoinRequestEvent(clientID: int)
    {
        super(JOIN_REQUEST);
        this._clientID = clientID;
    }

    public function get clientID():int
    {
        return _clientID;
    }

}
}