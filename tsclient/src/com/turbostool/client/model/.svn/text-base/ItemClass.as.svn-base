package com.turbostool.client.model
{
import com.turbostool.client.Config;
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.utils.Utils;

[Bindable]
public class ItemClass
{
    private var _id: Number;
    private var _name: String;
    private var _category: String;
    private var _description: String;
    private var _price: Number;
    private var _usingType: String;
    private var _usingCount: String;
    private var _slot: String;
    private var _durabilityMax: Number;
    private var _power: Number;
    private var _speed: Number;
    private var _braking: Number;
    private var _controllability: Number;
    private var _minLevel: Number;
    private var _maxLevel: Number;
    private var _realPrice: Number;
    private var _repairPrice: Number;
    private var _junkName: String;

    public function ItemClass()
    {
    }

    public function get isNitro():Boolean
    {
        return usingType == "inventory"
    }

    public function get imageSource():String
    {
        return Config.instance.serverUrl + "data/items/" + _id + ".png";
    }

    public function get id():Number
    {
        return _id;
    }

    public function set id(val:Number):void
    {
        _id = val;
    }

    public function get name():String
    {
        return  _name;
    }

    public function set name(val:String):void
    {
        _name = val;
    }

    public function get category():String
    {
        return _category;
    }

    public function set category(val:String):void
    {
        _category = val;
    }

    public function get description():String
    {
        return _description;
    }

    public function set description(val:String):void
    {
        _description = val;
    }

    public function get price():Number
    {
        return _price;
    }

    public function set price(val:Number):void
    {
        _price = val;
    }

    public function get usingType():String
    {
        return _usingType;
    }

    public function set usingType(val:String):void
    {
        _usingType = val;
    }

    public function get slot():String
    {
        return _slot;
    }

    public function set slot(val:String):void
    {
        _slot = val;
    }

    public function get durabilityMax():Number
    {
        return _durabilityMax;
    }

    public function set durabilityMax(val:Number):void
    {
        _durabilityMax = val;
    }

    public function get power():Number
    {
        return _power;
    }

    public function set power(val:Number):void
    {
        _power = val;
    }

    public function get speed():Number
    {
        return _speed;
    }

    public function set speed(val:Number):void
    {
        _speed = val;
    }

    public function get braking():Number
    {
        return _braking;
    }

    public function set braking(val:Number):void
    {
        _braking = val;
    }

    public function get controllability():Number
    {
        return _controllability;
    }

    public function set controllability(val:Number):void
    {
        _controllability = val;
    }

    public function get minLevel():Number
    {
        return _minLevel;
    }

    public function set minLevel(value:Number):void
    {
        _minLevel = value;
    }

    public function get maxLevel():Number
    {
        return _maxLevel;
    }

    public function set maxLevel(value:Number):void
    {
        _maxLevel = value;
    }

    public function get realPrice():Number
    {
        return _realPrice;
    }

    public function set realPrice(value:Number):void
    {
        _realPrice = value;
    }

    public function get usingCount():String
    {
        return _usingCount;
    }

    public function set usingCount(val:String):void
    {
        _usingCount = val;
    }

    public function get repairPrice():Number
    {
        return _repairPrice;
    }

    public function set repairPrice(value:Number):void
    {
        _repairPrice = value;
    }

    public function get junkName():String
    {
        return _junkName;
    }

    public function set junkName(value:String):void
    {
        _junkName = value;
    }

    public function toItemInfo(): ItemInfo {
        var itemInfo: ItemInfo = new ItemInfo();
        
        itemInfo.id = 0;
        itemInfo.classID = this.id;
        itemInfo.name = this.name;
        itemInfo.category = this.category;
        itemInfo.description = this.description;
        itemInfo.price = this.price;
        itemInfo.usingType = this.usingType;
        itemInfo.usingCount = this.usingCount;
        itemInfo.slot = this.slot;
        itemInfo.durabilityMax = this.durabilityMax;
        itemInfo.power = this.power;
        itemInfo.speed = this.speed;
        itemInfo.braking = this.braking;
        itemInfo.controllability = this.controllability;
        itemInfo.currentDurability = this.durabilityMax;
        itemInfo.minLevel = this.minLevel;
        itemInfo.maxLevel = this.maxLevel;
        itemInfo.realPrice = this.realPrice;
        itemInfo.repairPrice = this.repairPrice;
        itemInfo.junkName = this.junkName;
        itemInfo.newDurabilityMax = this.durabilityMax;

        return itemInfo;
    }
}
}