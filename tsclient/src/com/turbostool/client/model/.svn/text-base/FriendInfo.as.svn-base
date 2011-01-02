package com.turbostool.client.model
{
public class FriendInfo
{
    private var _userID: int = -1;
    private var _displayName: String = "";
    private var _rating: Number = 0;
    private var _level: int;
    private var _city: int;
    private var _isWashed: Boolean;

    private var _vkontakteID: Number;
    private var _vkRealName: String;
    private var _vkImageURL: String;

    public static function create(vkonakteID: Number, vkRealName: String, vkImageURL: String): FriendInfo {
        var friendInfo: FriendInfo = new FriendInfo();
        friendInfo.vkontakteID = vkonakteID;
        friendInfo.vkImageURL = vkImageURL;
        friendInfo.vkRealName = vkRealName;
        return friendInfo;
    }

    public function get userID():int
    {
        return _userID;
    }

    public function set userID(value:int):void
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

    public function get rating():int
    {
        return _rating;
    }

    public function set rating(value:int):void
    {
        _rating = value;
    }

    public function get level():int
    {
        return _level;
    }

    public function set level(value:int):void
    {
        _level = value;
    }

    public function get city():int
    {
        return _city;
    }

    public function set city(value:int):void
    {
        _city = value;
    }

    public function get vkRealName():String
    {
        return _vkRealName;
    }

    public function set vkRealName(value:String):void
    {
        _vkRealName = value;
    }

    public function get vkImageURL():String
    {
        return _vkImageURL;
    }

    public function set vkImageURL(value:String):void
    {
        _vkImageURL = value;
    }

    public function get vkontakteID():Number
    {
        return _vkontakteID;
    }

    public function set vkontakteID(value:Number):void
    {
        _vkontakteID = value;
    }

    public function get isWashed():Boolean
    {
        return _isWashed;
    }

    public function set isWashed(value:Boolean):void
    {
        _isWashed = value;
    }
}
}