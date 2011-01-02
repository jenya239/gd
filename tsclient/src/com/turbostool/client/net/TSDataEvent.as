package com.turbostool.client.net {
import flash.events.Event;

public class TSDataEvent extends Event {
    public static const TS_DATA:String = 'tsData';
    public static const CONNECTED:String = 'connected';
    public static const SESSION_CLOSE:String = 'sessionClose';

    public function TSDataEvent(type:String) {
        super(type);
    }
}
}