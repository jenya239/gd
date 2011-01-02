package com.turbostool.client.net.messages
{
import com.turbostool.client.model.ItemClass;
import com.turbostool.client.model.UserInfo;

public class BuyItemResponse extends ReasonedResponse
{
    public static const BUY_ITEM: String = "buyItem";

    private var _userInfo: UserInfo;
    private var _itemClass: ItemClass;

    public function BuyItemResponse()
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

    public function get itemClass():ItemClass {
        return _itemClass;
    }

    public function set itemClass(value:ItemClass):void {
        _itemClass = value;
    }
}
}