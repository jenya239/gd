package com.turbostool.client.game.view
{
import flash.events.Event;

public class ModalManagerEvent extends Event
{
    public static const FINISHED: String = "finished";

    public function ModalManagerEvent(type: String)
    {
        super(type);
    }
}
}