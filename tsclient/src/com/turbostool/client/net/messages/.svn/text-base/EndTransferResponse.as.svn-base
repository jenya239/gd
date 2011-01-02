package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserState;

public class EndTransferResponse extends ReasonedResponse
{
    public static const END_TRANSFER: String = "endTransfer";

    private var _cityDstID: Number;
    private var _userState: UserState;

    public function EndTransferResponse() {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get cityDstID():Number {
        return _cityDstID;
    }

    public function set cityDstID(value:Number):void {
        _cityDstID = value;
    }

    public function get userState():UserState
    {
        return _userState;
    }

    public function set userState(value:UserState):void
    {
        _userState = value;
    }
}
}