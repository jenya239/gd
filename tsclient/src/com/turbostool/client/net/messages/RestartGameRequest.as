package com.turbostool.client.net.messages
{

public class RestartGameRequest extends ServerRequest
{
    public static const RESTART_GAME: String = "restartGame";

    public function RestartGameRequest()
    {
        super(RESTART_GAME);
    }
}
}