package com.turbostool.client.net.messages
{

public class RepairRequest extends ServerRequest
{
    public static const REPAIR: String = "repair";
    private var _itemID: Number;

    public function RepairRequest(itemID: Number)
    {
        super(REPAIR);
        _itemID = itemID;
    }

    [Serializable(order=1)]
    public function get itemID(): Number
    {
        return _itemID;
    }
}
}