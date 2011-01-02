package com.turbostool.client.net.messages
{
public class PingMessage extends ServerRequest
{
    public static const PING: String = "ping";

    public function PingMessage()
    {
        super(PING);
    }
}
}