package com.turbostool.client.net.messages
{
public class JoinRequestMessage extends ServerRequest
{
    public static const JOIN: String = "join";

    private var _clientID: int;

    public function JoinRequestMessage(clientID: int)
    {
        super(JOIN);
        _clientID = clientID;
    }

    [Serializable(order=1)]
    public function get clientID(): int
    {
        return _clientID;
    }
}
}