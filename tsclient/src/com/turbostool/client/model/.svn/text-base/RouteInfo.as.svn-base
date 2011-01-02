package com.turbostool.client.model
{
import com.turbostool.client.Config;

import flash.events.EventDispatcher;

[Bindable]
public class RouteInfo extends EventDispatcher
{
    private var _id: int;
    private var _displayName: String;
    private var _fileNameBase: String;
    private var _length: int;
    private var _minLevel: int;
    private var _isHomeCity: Boolean;
    private var _isBattleCity: Boolean;

    public function get id(): int
    {
        return _id;
    }

    public function get fileNameBase(): String
    {
        return _fileNameBase;
    }

    public function get displayName(): String
    {
        return _displayName;
    }

    public function get displayName2(): String
    {
        return _displayName + " (" + _minLevel + " " + Client.instance.str("level") + ")";
    }

    public function get length(): int
    {
        return _length;
    }

    public function set length(value:int):void
    {
        _length = value;
    }

    public function get bestTime(): String
    {
        return "0:00";
    }

    public function get iconFileName(): String
    {
        return "data/routes/" + _fileNameBase + "/map_icon.png";
    }

    public function get previewFileName(): String
    {
        return "data/routes/" + _fileNameBase + "/map_preview.png";
    }

    public function get imageURL(): String
    {
        return Config.instance.serverUrl + "data/routes/" + _fileNameBase + "/map_preview.png";
    }

    public function set imageURL(c: String): void
    {
        
    }

    public function get newRoutePreviewFileName(): String
    {
        return "data/routes/" + _fileNameBase + "/new.png";
    }

    public function get lockedPreviewPath(): String
    {
        return "data/routes/locked/map_preview.png";
    }

    public function get lockedIconPath(): String
    {
        return "data/routes/locked/map_icon.png";
    }

    public function get basePath(): String
    {
        return "data/cars/" + _fileNameBase + "/";
    }

    public function get accessible():Boolean
    {
        return _minLevel <= Client.instance.modelsStorage.userInfo.level;
    }

    public function get minLevel():int
    {
        return _minLevel;
    }

    public function getImageList(): Array
    {
        var list: Array = new Array();
        list.push(previewFileName);
        list.push(iconFileName);
        list.push(lockedPreviewPath);
        list.push(lockedIconPath);
        list.push(newRoutePreviewFileName);
        return list;
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

    public function get isHomeCity():Boolean
    {
        return _isHomeCity;
    }

    public function set isHomeCity(value:Boolean):void
    {
        _isHomeCity = value;
    }

    public function get isBattleCity():Boolean
    {
        return _isBattleCity;
    }

    public function set isBattleCity(value:Boolean):void
    {
        _isBattleCity = value;
    }
}
}