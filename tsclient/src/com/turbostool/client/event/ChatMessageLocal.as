package com.turbostool.client.event
{
import flash.events.Event;

public class ChatMessageLocal extends Event
{
    public static const CHAT_MESSAGE_LOCAL: String = "localMessage"

    public function ChatMessageLocal()
    {
        super(CHAT_MESSAGE_LOCAL);
    }

}
}