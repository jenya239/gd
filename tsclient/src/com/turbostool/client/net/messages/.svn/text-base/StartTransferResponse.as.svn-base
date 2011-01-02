package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;
import com.turbostool.client.model.UserState;

public class StartTransferResponse extends ReasonedResponse
{
    public static const START_TRANSFER: String = "startTransfer";

    private var _arrivalTime: Number;
    private var _userState: UserState;
    private var _userInfo: UserInfo;

    public function StartTransferResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get arrivalTime():Number {
        return _arrivalTime;
    }

    public function set arrivalTime(value:Number):void {
        _arrivalTime = value;
    }

    public function get userState():UserState
    {
        return _userState;
    }

    public function set userState(value:UserState):void
    {
        _userState = value;
    }

    public function get userInfo():UserInfo {
        return _userInfo;
    }

    public function set userInfo(value:UserInfo):void {
        _userInfo = value;
    }
}
}