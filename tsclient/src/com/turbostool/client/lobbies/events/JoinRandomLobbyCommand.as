package com.turbostool.client.lobbies.events {
import flash.events.Event;

public class JoinRandomLobbyCommand extends Event    {
    private const JOIN_RANDOM_LOBBY_COMMAND:String = "joinRandomLobbyCommand";

    public function JoinRandomLobbyCommand()
    {
        super(JOIN_RANDOM_LOBBY_COMMAND);
    }

}
}