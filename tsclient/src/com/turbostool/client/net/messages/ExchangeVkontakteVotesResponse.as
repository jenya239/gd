package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;
import com.turbostool.client.model.VkontakteInfo;

public class ExchangeVkontakteVotesResponse extends ReasonedResponse
{
    public static const EXCHANGE_VKONTAKTE_VOTES: String = "exchangeVkontakteVotes";

    private var _userInfo: UserInfo;
    private var _votes: Number;

    public function ExchangeVkontakteVotesResponse()
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

    public function get votes():Number {
        return _votes;
    }

    public function set votes(value:Number):void {
        _votes = value;
    }
}
}