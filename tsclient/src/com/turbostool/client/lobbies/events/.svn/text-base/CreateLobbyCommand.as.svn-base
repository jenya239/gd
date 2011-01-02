package com.turbostool.client.lobbies.events
{
import com.turbostool.client.net.messages.CreateLobbyRequest;

import flash.events.Event;

public class CreateLobbyCommand extends Event
{
    public static const CREATE_LOBBY_COMMAND: String = "createLobbyCommand";

    public var createLobbyRequest: CreateLobbyRequest;

    public function CreateLobbyCommand(createLobbyRequest: CreateLobbyRequest)
    {
        super(CREATE_LOBBY_COMMAND);
        this.createLobbyRequest = createLobbyRequest;
    }


}
}