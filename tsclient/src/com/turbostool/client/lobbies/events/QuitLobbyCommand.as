package com.turbostool.client.lobbies.events
{
import flash.events.Event;

public class QuitLobbyCommand extends Event
{
    public static const QUIT_LOBBY_COMMAND:String = "quitLobbyCommand";

    public function QuitLobbyCommand()
    {
        super(QUIT_LOBBY_COMMAND);
    }

}
}