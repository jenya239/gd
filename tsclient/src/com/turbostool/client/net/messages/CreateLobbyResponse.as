package com.turbostool.client.net.messages
{
import com.turbostool.client.model.LobbyInfo;
import com.turbostool.client.model.UpgradeInfo;

public class CreateLobbyResponse extends ReasonedResponse
{
    public static const CREATE_LOBBY: String = "createLobby";
    public static const RESULT_OK: String = "ok";
    public static const RESULT_ERROR: String = "error";

    private var _result: String;
    private var _lobbyInfo: LobbyInfo;
    public var upgradeInfo:UpgradeInfo;
    
    public function CreateLobbyResponse()
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