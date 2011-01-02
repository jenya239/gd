package com.turbostool.client.lobbies.events
{

import flash.events.Event;

public class RefreshLobbiesCommand extends Event
{
    public static const REFRESH_LOBBIES_LIST: String = "refreshLobbiesCommand";

    public function RefreshLobbiesCommand()
    {
        super(REFRESH_LOBBIES_LIST);
    }

}
}