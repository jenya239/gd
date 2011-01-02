package com.turbostool.client.net.messages
{
public class RemoveClientMessage extends ChanneledMessage
{
    public static const REMOVE_CLIENT: String = "removeClient";

    private var _clientID: int;

    public function RemoveClientMessage(clientID: int = -1, channel: String = "")
    {
        super(channel);

        _clientID = clientID;
    }

    public function get clientID(): int
    {
        return _clientID;
    }

    public function set clientID(value:int):void
    {
        _clientID = value;
    }
}
}