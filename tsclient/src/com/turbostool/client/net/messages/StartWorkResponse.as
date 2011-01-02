package com.turbostool.client.net.messages {
public class StartWorkResponse extends ReasonedResponse{
    public function StartWorkResponse() {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public static const START_WORK: String = "startWork";

    private var _time:Number;


    public function get time():Number {
        return _time;
    }

    public function set time(val:Number):void {
        _time = val;
    }
}
}