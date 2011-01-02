package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class ChangeTriggerResponse extends ReasonedResponse
{
    public static const CHANGE_TRIGGER: String = "changeTrigger";

    private var _userInfo: UserInfo;

    public function ChangeTriggerResponse()
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