package com.turbostool.client.model
{
import com.turbostool.client.Config;
import com.turbostool.client.utils.Utils;
import flash.events.*;
import mx.controls.Alert;

[Bindable]
public class UserInfo extends EventDispatcher
{
    private var _id: Number;
		private var _vkontakteId: Number;
    private var _login: String;
    private var _email: String;
    private var _displayName: String;
    private var _image: String;
    private var _currentCarID: Number;   //carClassID  (see line 234)
    private var _carID: Number;
    private var _carFileName: String;
    private var _level: Number;
    private var _experience: Number;
    private var _expPrevLevel: Number;
    private var _expNextLevel: Number;
    private var _nitroCount: Number;
    private var _rating: Number;
    private var _currentCityID: Number;
    private var _homeCityID: Number;
    private var _color: Number;
    private var _money: Number;
    private var _realMoney: Number;
    private var _fuel: Number;
    private var _expLeft: Number;
    private var _isWashed: Boolean;

    private var _inventory: Array;
    private var _equipment: Array;

    private var _triggers: Array;
    private var _roles: Array;

    private var _cars: Array;
    
    public var duelCount: int = 0;
    public var duelWin: int = 0;

    public var hasGift: Boolean = false;
    private var _carSlots: Number = 3;

    [Bindable(event="carInfoChanged")]
    public function get carInfo():CarInfo {
        return getCarByID(carID);
    }

    private function getCarByID(carID:Number):CarInfo
    {
        for each(var car:CarInfo in cars )
        {
           if(car.id == carID)
             return car;
        }
        throw new Error("car has not been found id =  " + carID);
    }

    public function get carID():Number {
        return _carID;
    }

    public function set carID(val:Number):void {
        _carID = val;

        dispatchEvent(new Event("carInfoChanged"));
    }

    public function get cars():Array {
        return _cars;
    }

    public function set cars(val:Array):void {
        _cars = val;

        dispatchEvent(new Event("carInfoChanged"));
    }

    public function get expLeft():Number {
        return _expLeft;
    }

    public function set expLeft(val:Number):void {
        _expLeft = val;
    }

    public function get carFileName():String {
        return _carFileName;
    }

    public function set carFileName(val:String):void {
        _carFileName = val;
    }

    public function get realMoney():Number {
        return Utils.round(_realMoney, 2);
    }

    public function set realMoney(val:Number):void {
        _realMoney = val;
    }

    public function get fuel():Number {
        return _fuel;
    }

    public function set fuel(val:Number):void {
        _fuel = Utils.round(val);
    }

    private var _upgradeInfo: UpgradeInfo;

    public function UserInfo()
    {
    }

    public function get id():int
    {
        return _id;
    }

    public function set id(value:int):void
    {
        _id = value;
    }

    public function get login():String
    {
        return _login;
    }

    public function set login(value:String):void
    {
        _login = value;
    }

    public function get email():String
    {
        return _email;
    }

    public function set email(value:String):void
    {
        _email = value;
    }

    public function get displayName():String
    {
        return _displayName;
    }

    public function set displayName(value:String):void
    {
        _displayName = value;
    }

    public function set image(value:String):void
    {
        _image = value;
    }

		//carClassID
    public function get currentCarID():int
    {
        return _currentCarID;
    }

    public function set currentCarID(value:int):void
    {
        _currentCarID = value;
    }

    public function get level():int
    {
        return _level;
    }

    public function set level(value:int):void
    {
        _level = value;
    }

    public function get experience():int
    {
        return _experience;
    }

    public function set experience(value:int):void
    {
        _experience = value;
    }

    public function get expPrevLevel(): Number
    {
        return _expPrevLevel;
    }

    public function set expPrevLevel(value: Number): void
    {
        _expPrevLevel = value;
    }

    public function get expNextLevel():Number
    {
        return _expNextLevel;
    }

    public function set expNextLevel(value:Number):void
    {
        _expNextLevel = value;
    }

    public function set nitroCount(value:int):void
    {
        _nitroCount = value;
    }

    public function get clanName(): String
    {
        return "";
    }

    public function get country(): String
    {
        return "";
    }

    public function get image(): String
    {
        return "data/selectCar/car_" + _currentCarID + "_" + _color + ".png";
    }

    public function get imageURL(): String
    {
        return Config.instance.serverUrl + image;
    }

    public function get nitroCount(): int
    {
        return _nitroCount;
    }

    public function get rating():int
    {
        return _rating;
    }

    public function set rating(value:int):void
    {
        _rating = value;
    }

    public function get currentCity(): int
    {
        return _currentCityID;
    }

    public function set currentCity(value:int): void
    {
        _currentCityID = value;
    }

    public function get homeCity():Number {
        return _homeCityID;
    }

    public function set homeCity(value:Number):void {
        _homeCityID = value;
    }

    public function get color():int
    {
        return _color;
    }

    public function set color(value:int):void
    {
        _color = value;
    }

    public function get racingCarName(): String
    {
        return "car_" + _currentCarID + "_" + color;
    }

    public function get inventory(): Array {
        return _inventory;
    }

    public function set inventory(val: Array):void {
        _inventory = val;
    }

    public function get equipment():Array {
        return _equipment;
    }

    public function set equipment(val:Array):void {
        _equipment = val;
    }

    public function get upgradeInfo():UpgradeInfo {
        return _upgradeInfo;
    }

    public function set upgradeInfo(val:UpgradeInfo):void {
        _upgradeInfo = val;
    }

    public function get money(): Number
    {
        return Utils.round(_money, 2);
    }

    public function set money(value:Number):void
    {
        _money = value;
    }

    public function get triggers():Array
    {
        return _triggers;
    }

    public function set triggers(value:Array):void
    {
        _triggers = value;
    }

    public function get roles():Array
    {
        return _roles;
    }

    public function set roles(value:Array):void
    {
        _roles = value;
    }

    public function hasRole(roleName: String): Boolean
    {
        for each(var roleInfo: RoleInfo in _roles)
        {
            if (roleInfo.name == roleName)
                return true;
        }
        return false;
    }

    public function hasTrigger(triggerName: String): Boolean
    {
        for each(var triggerInfo: TriggerInfo in _triggers)
        {
            if (triggerInfo.name == triggerName)
                return true;
        }
        return false;
    }

    public function get tutorialStage():Number {
        for each(var triggerInfo: TriggerInfo in _triggers)
        {
            if (triggerInfo.name == "tutorialStage")
                return triggerInfo.value;
        }

        return Number.NaN;
    }

    public function get tutorialStageStr():String {
        var stage:Number = tutorialStage;
        if(!isNaN(stage))
            return "tutorialStage" + stage;
        else
            return "";
    }

    public function getTrigger(triggerName: String): TriggerInfo
    {
        for each(var triggerInfo: TriggerInfo in _triggers)
        {
            if (triggerInfo.name == triggerName)
                return triggerInfo;
        }

        return null;
    }

    public function get carSlots():Number {
        return _carSlots;
    }

    public function set carSlots(value:Number):void {
        _carSlots = value;
    }

    public function get isWashed():Boolean
    {
        return _isWashed;
    }

    public function set isWashed(value:Boolean):void
    {
        _isWashed = value;
    }

	public function get vkontakteId():Number {
		return _vkontakteId;
	}

	public function set vkontakteId(value:Number):void {
		_vkontakteId = value;
	}
}
}
