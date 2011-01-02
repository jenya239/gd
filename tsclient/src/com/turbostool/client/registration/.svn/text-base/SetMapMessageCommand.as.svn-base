package com.turbostool.client.registration
{
import com.turbostool.client.screens.BaseScreen;

import flash.events.Event;

public class SetMapMessageCommand extends Event
{
    public static const SET_MAP_MESSAGE: String = "setMapMessage";

    private var _message: String;

    public function SetMapMessageCommand(message: String) {
        super(SET_MAP_MESSAGE);
        this._message = message;
    }

    public function get message(): String {
        return _message;
    }
}
}