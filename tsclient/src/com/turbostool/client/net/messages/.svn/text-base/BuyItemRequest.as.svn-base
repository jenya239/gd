package com.turbostool.client.net.messages
{
public class BuyItemRequest extends ServerRequest
{
    public static const BUY_ITEM: String = "buyItem";

    private var _itemClassID: int;

    public function BuyItemRequest(itemClassID: int)
    {
        super(BUY_ITEM);
        _itemClassID = itemClassID;
    }

    [Serializable(order=1)]
    public function get itemClassID(): int
    {
        return _itemClassID;
    }
}
}
