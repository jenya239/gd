package com.turbostool.client.model
{
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.EventDispatcher;

[Bindable]
public class PostMessageInfo extends EventDispatcher
{
    private var _id: Number;
    private var _itemInfo: Array;
    private var _senderID: Number;
    private var _fromNick: String;
    private var _timeStamp: Number;
    private var _timeStampDate: Date;
    private var _comment: String;
    private var _money: Number;
    private var _sellPrice: Number;

    public function PostMessageInfo()
    {
    }

    public function get id():Number
    {
        return _id;
    }

    public function set id(value:Number):void
    {
        _id = value;
    }

    [Bindable(event="itemInfoChanged")]
    public function get firstItemInfo():ItemInfo
    {
        return itemInfo.length > 0 ? itemInfo[0] : null;
    }

    public function get itemInfo():Array
    {
        return _itemInfo;
    }

    public function set itemInfo(value:Array):void
    {
        _itemInfo = value;
        dispatchEvent(new Event("itemInfoChanged"));
    }

    public function get fromNick():String
    {
        return _fromNick;
    }

    public function set fromNick(value:String):void
    {
        _fromNick = value;
    }

    public function get timeStamp():Number
    {
        return _timeStamp;
    }

    public function set timeStamp(value:Number):void
    {
        _timeStamp = value;
        timeStampDate = new Date(value);
    }

    public function get comment():String
    {
        return _comment;
    }

    public function set comment(value:String):void
    {
        _comment = value;
    }

    public function get money():Number
    {
        return _money;
    }

    public function set money(value:Number):void
    {
        _money = value;
    }

    public function get sellPrice():Number
    {
        return _sellPrice;
    }

    public function set sellPrice(value:Number):void
    {
        _sellPrice = value;
    }

    public function get timeStampDate():Date
    {
        return _timeStampDate;
    }

    public function set timeStampDate(value: Date): void
    {
        _timeStampDate = value;
        dispatchEvent(new Event("timeStampDateChanged"));
    }

    [Bindable(event="timeStampDateChanged")]
    public function get timeStampDateString(): String
    {
        return Utils.stringFormat("{0}/{1}/{2}", timeStampDate.day, timeStampDate.month+1, timeStampDate.fullYear); 
    }

    public function get senderID():Number
    {
        return _senderID;
    }

    public function set senderID(value:Number):void
    {
        _senderID = value;
    }
}
}