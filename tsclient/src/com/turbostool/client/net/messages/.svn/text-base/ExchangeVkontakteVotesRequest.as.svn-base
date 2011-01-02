package com.turbostool.client.net.messages
{
public class ExchangeVkontakteVotesRequest extends ServerRequest
{
    public static const EXCHANGE_VKONTAKTE_VOTES: String = "exchangeVkontakteVotes";

    private var _votes: Number;

    public function ExchangeVkontakteVotesRequest(votes: Number)
    {
        super(EXCHANGE_VKONTAKTE_VOTES);
        _votes = votes;
    }

    [Serializable(order=1)]
    public function get votes():Number {
        return _votes;
    }
}
}
