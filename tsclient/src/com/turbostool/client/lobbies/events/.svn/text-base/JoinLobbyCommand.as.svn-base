package com.turbostool.client.lobbies.events
{
import com.turbostool.client.model.LobbyInfo;

import flash.events.Event;

public class JoinLobbyCommand extends Event
{
    public static const JOIN_LOBBY_COMMAND:String = "joinLobbyCommand";

    public var lobbyInfo: LobbyInfo;

    public function JoinLobbyCommand(lobbyInfo: LobbyInfo)
    {
        super(JOIN_LOBBY_COMMAND);
        this.lobbyInfo = lobbyInfo;
    }
}
}