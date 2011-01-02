package com.turbostool.client.net.messages {
import com.turbostool.client.model.GasInfo;
import com.turbostool.client.model.UserInfo;
public class BuyFuelResponse extends ReasonedResponse{

    public static const BUY_FUEL: String = "buyFuel";

    private var _userInfo: UserInfo;
    private var _gasInfo:GasInfo;

    public function BuyFuelResponse() {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get userInfo():UserInfo {
        return _userInfo;
    }

    public function set userInfo(val:UserInfo):void {
        _userInfo = val;
    }

    public function get gasInfo():GasInfo {
        return _gasInfo;
    }

    public function set gasInfo(val:GasInfo):void {
        _gasInfo = val;
    }
}
}