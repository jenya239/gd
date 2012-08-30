package com.turbostool.client.model
{
import com.turbostool.client.Config;
import com.turbostool.client.utils.Utils;
import mx.controls.Alert;

[Bindable]
public class CarInfo
{
    public static const POSTFIX_ARRAY: Array = new Array("_locked_up.png", "_unlocked_up.png", "_preview_locked.png", "_preview_unlocked.png", "_scores.png", "_new.png");

    private var _id: int;
    private var _classID:Number;
    private var _displayName: String;
    private var _fileName: String;
    private var _fileNameBase: String;
    private var _description: String;
    private var _minLevel: int;

    private var _controllability: Number = 0;
    private var _power: Number = 0;
    private var _speed: Number = 0;
    private var _breaking: Number = 0;

    private var _fuelCapacity:Number;
    private var _fuelConsumption:Number;
    private var _fuel:Number;
    private var _color:Number;
    private var _nitroCount:Number;
    private var _currentDurability:Number;
    private var _durabilityMax:Number;
    private var _price:Number;
    private var _realPrice:Number;
    private var _repairPrice:Number;
    private var _capitalRepairPrice:Number=0;
    private var _junkName:Number;

    private var _upgrades:Array;
    private var _recolorInfo:Array;
    private var _sellPrice:Number=-1;

    //old
    private var _maxSpeed: Number = 0;
    private var _acceleration: Number = 0;
    private var _steering: Number = 0;
    private var _accessible: Boolean = true;

    private var _count: int;

    public function get capitalRepairPrice():Number {
        return _capitalRepairPrice;
    }

    public function set capitalRepairPrice(val:Number):void {
        _capitalRepairPrice = val;
    }

    public function get sellPrice():Number {
        return Utils.round(_sellPrice);
    }

    public function set sellPrice(val:Number):void {
        _sellPrice = val;
    }

    public function get recolorInfo():Array {
        return _recolorInfo;
    }

    public function set recolorInfo(val:Array):void {
        _recolorInfo = val;
    }

    public function recolorPrice(colorInd:Number):String
    {        
        for each(var rec:RecolorInfo in _recolorInfo){
            if(rec.color == colorInd)
              return (rec.realPrice == 0)
                ? "" + Utils.formatPrice(rec.price, false)
                : "" + Utils.formatPrice(rec.realPrice, true);
        }
        return "error=)";
    }

    public function set displayPrice(a:String):void {
    }

    public function get displayPrice():String
    {
        return (_realPrice == 0)
                ? "" + Utils.formatPrice(price, false)
                : "" + Utils.formatPrice(realPrice, true)
    }

    public function get durabilityMax():Number {
        return _durabilityMax;
    }

    public function set durabilityMax(val:Number):void {
        _durabilityMax = val;
    }

    public function get upgrades():Array {
        return _upgrades;
    }

    public function set upgrades(val:Array):void {
        _upgrades = val;
    }

    public function get classID():Number {
        return _classID;
    }

    public function get fileName():String {
        return _fileName;
    }

    public function get description():String {
        return _description;
    }

    public function get power():Number {
        return _power;
    }

    public function get speed():Number {
        return _speed;
    }

    public function get breaking():Number {
        return _breaking;
    }

    public function get fuel():Number {
        return _fuel;
    }

    public function get color():Number {
        return _color;
    }

    public function get nitroCount():Number {
        return _nitroCount;
    }

    public function get currentDurability():Number {
        return _currentDurability;
    }

    public function get price():Number {
        return _price;
    }

    public function get realPrice():Number {
        return _realPrice;
    }

    public function get repairPrice():Number {
        return _repairPrice;
    }

    public function get junkName():Number {
        return _junkName;
    }

    public function get accessible():Boolean {
        return _accessible;
    }

    public function set classID(val:Number):void {
        _classID = val;
    }

    public function set fileName(val:String):void {
        _fileName = val;
    }

    public function set description(val:String):void {
        _description = val;
    }

    public function set power(val:Number):void {
        _power = val;
    }

    public function set speed(val:Number):void {
        _speed = val;
    }

    public function set breaking(val:Number):void {
        _breaking = val;
    }

    public function set fuel(val:Number):void {
        _fuel = val;
    }

    public function set color(val:Number):void {
        _color = val;
    }

    public function set nitroCount(val:Number):void {
        _nitroCount = val;
    }

    public function set currentDurability(val:Number):void {
        _currentDurability = val;
    }

    public function set price(val:Number):void {
        _price = val;
    }

    public function set realPrice(val:Number):void {
        _realPrice = val;
    }

    public function set repairPrice(val:Number):void {
        _repairPrice = val;
    }

    public function set junkName(val:Number):void {
        _junkName = val;
    }


    public function get fuelCapacity():Number {
        return _fuelCapacity;
    }

    public function set fuelCapacity(val:Number):void {
        _fuelCapacity = val;
    }

    public static function create(id: int, displayName: String): CarInfo
    {
        var carInfo: CarInfo = new CarInfo();
        carInfo._id = id;
        carInfo._displayName = displayName;
        return carInfo;
    }

    public function get suffix(): String
    {
        return "_unlocked";
    }

    public function getImageList(): Array
    {
        var list: Array = new Array();
        for (var i: int = 0; i < POSTFIX_ARRAY.length; i++)
        {
            list.push(basePath + fileNameBase + POSTFIX_ARRAY[i]);
        }
        return list;
    }

    public function get id(): int
    {
        return _id;
    }

    public function get minLevel(): int
    {
        return _minLevel;
    }

    public function get fileNameBase(): String
    {
        return _fileNameBase;
    }


    public function get displayName(): String
    {
        return _displayName;
    }

    public function get maxSpeed(): Number
    {
        return _maxSpeed;
    }

    public function get acceleration(): Number
    {
        return _acceleration;
    }

    public function get steering():Number
    {
        return _steering;
    }

    public function get basePath(): String
    {
        return "data/cars/" + _fileNameBase + "/";
    }

    public function get previewFileName(): String
    {
        return "data/cars/" + _fileNameBase + "/" + _fileNameBase + "_preview" + suffix + ".png";
    }

    public function get newCarPreviewFileName(): String
    {
        return "data/cars/" + _fileNameBase + "/" + _fileNameBase + "_new.png";
    }

    public function get iconFileName():String
    {
        return "data/cars/" + _fileNameBase + "/" + _fileNameBase + suffix;
    }

    public function set id(value:int):void
    {
        _id = value;
    }

    public function set displayName(value:String):void
    {
        _displayName = value;
    }

    public function set fileNameBase(value:String):void
    {
        _fileNameBase = value;
    }

    public function set maxSpeed(value:Number):void
    {
        _maxSpeed = value;
    }

    public function set acceleration(value:Number):void
    {
        _acceleration = value;
    }

    public function set steering(value:Number):void
    {
        _steering = value;
    }

    public function set minLevel(value:int):void
    {
        _minLevel = value;
    }

    public function get controllability():int
    {
        return _controllability;
    }

    public function set controllability(value:int):void
    {
        _controllability = value;
    }

    public function get fuelConsumption():Number {
        return _fuelConsumption;
    }

    public function set fuelConsumption(val:Number):void {
        _fuelConsumption = val;
    }

    public function get imageSource():String
    {
        var res:String = Config.instance.serverUrl + "data/selectCar/car_" + classID + "_" + color + ".png";
        return res;
    }

    public function get count():int {
        return _count;
    }

    public function set count(value:int):void {
        _count = value;
    }
}

}
