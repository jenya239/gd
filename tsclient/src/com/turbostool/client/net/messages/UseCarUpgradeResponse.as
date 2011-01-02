package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class UseCarUpgradeResponse extends ReasonedResponse
{
    public static const USE_CAR_UPGRADE: String = "useCarUpgrade";

    private var _userInfo: UserInfo;

    public function UseCarUpgradeResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get userInfo():UserInfo
    {
        return _userInfo;
    }

    public function set userInfo(val:UserInfo):void
    {
        _userInfo = val;
    }
}
}