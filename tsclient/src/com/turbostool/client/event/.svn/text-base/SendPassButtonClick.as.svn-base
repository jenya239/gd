package com.turbostool.client.event {
import flash.events.Event;

public class SendPassButtonClick extends Event{
    public static const SEND_PASS_BUTTON_CLICK:String = "SEND_PASS_BUTTON_CLICK";
    private var _login:String;

    public function SendPassButtonClick(login:String) {
        super(SEND_PASS_BUTTON_CLICK);
        _login = login;
    }

    public function get login():String
    {
        return _login;
    }
}
}