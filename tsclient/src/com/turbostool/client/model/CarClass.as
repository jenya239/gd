package com.turbostool.client.model
{
import com.turbostool.client.Config;
import com.turbostool.client.utils.Utils;
import mx.controls.Alert;

[Bindable]
public class CarClass
{
    private var _id: int;
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
    private var _durabilityMax:Number;
    private var _price:Number;
    private var _realPrice:Number;
    private var _junkName:Number;
    private var _count: int;


    public function set displayPrice(a:String):void {
    }

    public function get displayPrice():String
    {
        return (_realPrice == 0)
                ? "" + Utils.formatPrice(price, false)
                : "" + Utils.formatPrice(realPrice, true);
    }

    public function get durabilityMax():Number {
        return _durabilityMax;
    }

    public function set durabilityMax(val:Number):void {
        _durabilityMax = val;
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

    public function get price():Number {
        return _price;
    }

    public function get realPrice():Number {
        return _realPrice;
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

    public function set price(val:Number):void {
        _price = val;
    }

    public function set realPrice(val:Number):void {
        _realPrice = val;
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

    public function get basePath(): String
    {
        return "data/cars/" + _fileNameBase + "/";
    }

    public function get newCarPreviewFileName(): String
    {
        return "data/cars/" + _fileNameBase + "/" + _fileNameBase + "_new.png";
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

    public function get count():int {
        return _count;
    }

    public function set count(value:int):void {
        _count = value;
    }
}

}
