package com.turbostool.client.event
{
import flash.events.Event;

public class RegisteredEvent extends Event
{
    public static const REGISTERED: String = "registered";

    public function RegisteredEvent()
    {
        super(REGISTERED);
    }
}
}