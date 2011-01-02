package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class ExchangeMoneyResponse extends ReasonedResponse
{
    public static const EXCHANGE_MONEY: String = "exchangeMoney";

    private var _userInfo: UserInfo;
    private var _realMoney: Number;

    public function ExchangeMoneyResponse()
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

    public function get realMoney():Number {
        return _realMoney;
    }

    public function set realMoney(value:Number):void {
        _realMoney = value;
    }
}
}