package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class SendItemResponse extends ReasonedResponse
{
    public static const SEND_ITEM: String = "sendItem";

    private var _userInfo: UserInfo;

    public function SendItemResponse()
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