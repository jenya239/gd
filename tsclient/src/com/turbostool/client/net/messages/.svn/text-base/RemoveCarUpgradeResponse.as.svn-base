package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class RemoveCarUpgradeResponse extends ReasonedResponse
{
    public static const REMOVE_CAR_UPGRADE: String = "removeCarUpgrade";

    private var _userInfo: UserInfo;

    public function RemoveCarUpgradeResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get userInfo():UserInfo {
        return _userInfo;
    }

    public function set userInfo(val:UserInfo):void {
        _userInfo = val;
    }
}
}