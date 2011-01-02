package com.turbostool.client.net.messages
{
import com.turbostool.client.model.ItemClass;
import com.turbostool.client.model.UserInfo;

public class SwitchHomeCityResponse extends ReasonedResponse
{
    public static const SWITCH_HOME_CITY: String = "switchHomeCity";

    private var _userInfo: UserInfo;

    public function SwitchHomeCityResponse()
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
}
}