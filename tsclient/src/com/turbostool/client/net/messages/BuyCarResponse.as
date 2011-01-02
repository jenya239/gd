package com.turbostool.client.net.messages {
import com.turbostool.client.model.CarClass;
import com.turbostool.client.model.UserInfo;

public class BuyCarResponse extends ReasonedResponse {

    public static const BUY_CAR: String = "buyCar";

    private var _userInfo: UserInfo;
    private var _carClass: CarClass;
    private var _slot: Boolean;

    public function BuyCarResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get userInfo(): UserInfo
    {
        return _userInfo;
    }

    public function set userInfo(val: UserInfo): void
    {
        _userInfo = val;
    }

    public function get carClass():CarClass {
        return _carClass;
    }

    public function set carClass(value:CarClass):void {
        _carClass = value;
    }

    public function get slot():Boolean {
        return _slot;
    }

    public function set slot(value:Boolean):void {
        _slot = value;
    }
}
}