package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserState;

public class GetUserStateResponse extends ReasonedResponse
{
    public static const GET_USERSTATE_RESPONSE: String = "getUserStateResponse";

    private var _userState: UserState;
    private var _currentCity: Number;

    public function GetUserStateResponse(channel: String = ChanneledMessage.GLOBAL_CHANNEL)
    {
        super(channel);
    }

    public function get userState():UserState
    {
        return _userState;
    }

    public function set userState(value:UserState):void
    {
        _userState = value;
    }

    public function get currentCity():Number
    {
        return _currentCity;
    }

    public function set currentCity(value:Number):void
    {
        _currentCity = value;
    }
}
}