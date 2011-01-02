package com.turbostool.client.net.messages {
import com.turbostool.client.model.GasInfo;
import com.turbostool.client.model.UserInfo;
public class CleanCarResponse extends ReasonedResponse{

    public static const CLEAN_CAR: String = "cleanCar";

    private var _money: Number;

    public function CleanCarResponse() {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get money():Number {
        return _money;
    }

    public function set money(value:Number):void {
        _money = value;
    }
}
}