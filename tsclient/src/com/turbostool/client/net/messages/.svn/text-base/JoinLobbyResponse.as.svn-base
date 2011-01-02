package com.turbostool.client.net.messages
{
import com.turbostool.client.model.LobbyInfo;
import com.turbostool.client.model.UpgradeInfo;

public class JoinLobbyResponse extends ReasonedResponse
{
    public static const JOIN_LOBBY: String = "joinLobby";

    private var _lobbyInfo: LobbyInfo;
    public var upgradeInfo:UpgradeInfo;    

    public function JoinLobbyResponse()
    {
        super(ChanneledMessage.LOBBY_CHANNEL);
    }

    public function get lobbyInfo():LobbyInfo
    {
        return _lobbyInfo;
    }

    public function set lobbyInfo(val:LobbyInfo):void
    {
        _lobbyInfo = val;
    }
}
}