package com.turbostool.client.net.messages
{
public class JoinLobbyRequest extends ServerRequest
{
    public static const JOIN_LOBBY: String = "joinLobby";

    private var _lobbyID : int;

    public function JoinLobbyRequest(lobbyID: int)
    {
        super(JOIN_LOBBY);
        this._lobbyID = lobbyID;
    }

    [Serializable(order=1)]
    public function get lobbyID():int
    {
        return _lobbyID;
    }
}
}