package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;
public class DeleteItemResponse extends ReasonedResponse
{
    public static const DELETE_ITEM: String = "deleteItem";

    private var _userInfo: UserInfo;

    public function DeleteItemResponse()
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