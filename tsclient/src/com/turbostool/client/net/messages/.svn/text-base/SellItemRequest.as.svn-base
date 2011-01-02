package com.turbostool.client.net.messages
{
public class SellItemRequest extends ServerRequest
{
    public static const SELL_ITEM: String = "sellItem";

    private var _itemID: int;

    public function SellItemRequest(itemID: int)
    {
        super(SELL_ITEM);
        _itemID = itemID;
    }

    [Serializable(order=1)]
    public function get itemID(): int
    {
        return _itemID;
    }
}
}
