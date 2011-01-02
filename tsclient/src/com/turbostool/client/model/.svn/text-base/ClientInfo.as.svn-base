package com.turbostool.client.model
{
import com.turbostool.client.Config;

public class ClientInfo
{
    public var id: int;
    public var carID: int;
    public var userID: int;
    public var carColor: int;
    public var homeCity: int;
    public var currentCity: int;
    public var level: int = 1;
    public var userInfo:UserInfo;
    private var _carFileName:String;

    private var _upgradeInfo: UpgradeInfo;

    [Bindable]
    public var displayName: String;

    [Bindable]
    public var loadingProgress: int;

    public function ClientInfo(id: int = -1, displayName: String = "", carID: int = -1, loadingProgress: int = 0, upgInfo:UpgradeInfo = null, cityId: int = 1, level: int = 1)
    {
        this.id = id;
        this.displayName = displayName;
        this.carID = carID;
        this.loadingProgress = loadingProgress;
        this.upgradeInfo = upgInfo;
        this.currentCity = cityId;
        this.level = level;
    }

    public function get carFileName():String {
        return _carFileName;
    }

    public function set carName(val:String):void {
        _carFileName = val;
    }

    public function get racingCarName(): String
    {
        return "car_" + carID + "_" + carColor;
    }

    public function get garageCarUrl(): String
    {
        return Config.instance.serverUrl + "data/selectCar/car_" + carID + "_" + carColor + ".png";
    }

    public function get upgradeInfo(): UpgradeInfo
    {
        return _upgradeInfo;
    }

    public function set upgradeInfo(value:UpgradeInfo):void
    {
        _upgradeInfo = value;
    }

    public function toString():String {
        return "ClientInfo id = " + id + " display name = " + displayName + " carID = " + carID
                + "carFileName = " + carFileName + " racingCarName = " + racingCarName;
    }
}
}
