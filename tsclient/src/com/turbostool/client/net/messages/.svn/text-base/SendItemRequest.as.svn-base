package com.turbostool.client.net.messages
{
public class SendItemRequest extends ServerRequest
{
    public static const SEND_ITEM: String = "sendItem";

    private var _recepientNick: String;
    private var _itemID: Number;
    private var _money: Number;
    private var _sellPrice: Number;
    private var _comment: String;

    public function SendItemRequest(recepientNick:String, itemID:Number, money:Number, sellPrice:Number, comment:String)
    {
        super(SendItemRequest.SEND_ITEM);
        
        _recepientNick = recepientNick;
        _itemID = itemID;
        _money = money;
        _sellPrice = sellPrice;
        _comment = comment;
    }

    [Serializable(order=1)]
    public function get itemID():Number
    {
        return _itemID;
    }

    [Serializable(order=2)]
    public function get money():Number
    {
        return _money;
    }

    [Serializable(order=3)]
    public function get sellPrice():Number
    {
        return _sellPrice;
    }

    [Serializable(order=4, escape=true)]
    public function get comment():String
    {
        return _comment;
    }

    [Serializable(order=5, escape=true)]
    public function get recepientNick():String
    {
        return _recepientNick;
    }
}
}
