package com.turbostool.client.net.messages
{
public class ExchangeMoneyRequest extends ServerRequest
{
    public static const EXCHANGE_MONEY: String = "exchangeMoney";

    private var _realMoney: Number;

    public function ExchangeMoneyRequest(realMoneyToExchange: int)
    {
        super(EXCHANGE_MONEY);
        _realMoney = realMoneyToExchange;
    }

    [Serializable(order=1)]
    public function get realMoney(): int
    {
        return _realMoney;
    }
}
}
