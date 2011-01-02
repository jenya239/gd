package com.turbostool.client.model
{
[Bindable]
public class UserDailyScoreInfo
{
    private var _userID: Number;
    private var _displayName: String;
    private var _score: Number;
    private var _homeCity: Number;
    private var _position: String;

    public function get userID():Number
    {
        return _userID;
    }

    public function set userID(value:Number):void
    {
        _userID = value;
    }

    public function get displayName():String
    {
        return _displayName;
    }

    public function set displayName(value:String):void
    {
        _displayName = value;
    }

    public function get score():Number
    {
        return _score;
    }

    public function set score(value:Number):void
    {
        _score = value;
    }

    public function get homeCity():Number
    {
        return _homeCity;
    }

    public function set homeCity(value:Number):void
    {
        _homeCity = value;
    }

    public function get position():String
    {
        return _position;
    }

    public function set position(value:String):void
    {
        _position = value;
    }
}
}