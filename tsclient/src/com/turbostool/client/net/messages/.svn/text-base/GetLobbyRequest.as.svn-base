package com.turbostool.client.net.messages
{
public class GetLobbyRequest extends GetPropertyRequest
{
    public static const LOBBY: String = "lobby";

    private var _lobbyID: int;

    public function GetLobbyRequest(lobbyID: int)
    {
        super(LOBBY);
        _lobbyID = lobbyID;
    }

    [Serializable(order=2)]
    public function get lobbyID():int
    {
        return _lobbyID;
    }
}
}