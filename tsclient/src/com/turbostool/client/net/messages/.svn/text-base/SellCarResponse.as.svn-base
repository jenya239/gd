package com.turbostool.client.net.messages {
import com.turbostool.client.model.CarClass;
import com.turbostool.client.model.UserInfo;
public class SellCarResponse extends ReasonedResponse{
    public static const SELL_CAR: String = "sellCar";

    private var _userInfo: UserInfo;
    private var _carClass: CarClass;

    public function SellCarResponse()
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
}
}