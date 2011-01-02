package com.turbostool.client.net.messages {
import com.turbostool.client.model.CarClass;
import com.turbostool.client.model.UserInfo;

public class LevelUpRewardsResponse extends ReasonedResponse {

    public static const LEVEL_UP_REWARDS: String = "levelUpRewards";

    private var _realMoney: Number;
    private var _money: Number;
    private var _itemName: String;
    private var _nitroCount: Number;

    public function LevelUpRewardsResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get realMoney():Number {
        return _realMoney;
    }

    public function set realMoney(value:Number):void {
        _realMoney = value;
    }

    public function get money():Number {
        return _money;
    }

    public function set money(value:Number):void {
        _money = value;
    }

    public function get itemName():String {
        return _itemName;
    }

    public function set itemName(value:String):void {
        _itemName = value;
    }

    public function get nitroCount():Number {
        return _nitroCount;
    }

    public function set nitroCount(value:Number):void {
        _nitroCount = value;
    }
}
}