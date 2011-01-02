package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class ProcessPostMessageResponse extends ReasonedResponse
{
    public static const PROCESS_POST_MESSAGE: String = "processPostMessage";

    private var _inbox: Array;
    private var _userInfo: UserInfo;

    public function ProcessPostMessageResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get inbox():Array
    {
        return _inbox;
    }

    public function set inbox(value:Array):void
    {
        _inbox = value;
    }

    public function get userInfo():UserInfo
    {
        return _userInfo;
    }

    public function set userInfo(value:UserInfo):void
    {
        _userInfo = value;
    }
}
}