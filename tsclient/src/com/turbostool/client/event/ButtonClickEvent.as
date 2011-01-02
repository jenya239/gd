package com.turbostool.client.event
{
import flash.events.Event;

public class ButtonClickEvent extends Event
{
    public static const BUTTON_CLICK: String = "buttonClick";

    private var _sender: Object;

    public function ButtonClickEvent(type: String, sender: Object)
    {
        super(type);
        _sender = sender;
    }

    public function get sender(): Object
    {
        return _sender;
    }

    public static function getSender(event: Event): Object
    {
        return event is ButtonClickEvent ? (ButtonClickEvent(event).sender) : null;
    }

    public static function isSender(event: Event, sender: Object): Boolean
    {
        return getSender(event) == sender;
    }
}
}