package com.turbostool.client.net.messages
{

public class DeleteItemRequest extends ServerRequest
{
    public static const DELETE_ITEM: String = "deleteItem";
    private var _itemID: Number;

    public function DeleteItemRequest(itemID: Number)
    {
        super(DELETE_ITEM);
        _itemID = itemID;
    }

    [Serializable(order=1)]
    public function get itemID(): Number
    {
        return _itemID;
    }
}
}