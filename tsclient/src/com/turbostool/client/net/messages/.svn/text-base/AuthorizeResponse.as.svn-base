package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UpgradeInfo;
import com.turbostool.client.model.UserInfo;

public class AuthorizeResponse extends ReasonedResponse
{
    public static const AUTHORIZE: String = "authorize";

    private var _userInfo: UserInfo;
    public function AuthorizeResponse()
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