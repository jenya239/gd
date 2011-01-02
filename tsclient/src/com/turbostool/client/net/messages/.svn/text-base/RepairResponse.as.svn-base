package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;
public class RepairResponse extends ReasonedResponse
{
    public static const REPAIR: String = "repair";

    private var _userInfo: UserInfo;

    public function RepairResponse()
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