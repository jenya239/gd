package com.turbostool.client.model
{

[Bindable]
public class ChatMessageInfo
{
    private var _timestamp: Date;
    private var _nick: String;
    private var _text: String;
    private var _userId:Number=0;
    private var _homeCity: Number;
    public var admin:String="false";
    public var rj:String="false";

    public function ChatMessageInfo(timestamp: Date, nick: String, text: String, userId: Number, homeCity: Number, admin1:String, rj1:String)
    {
        _timestamp = timestamp;
        _nick = nick;
        _text = text;
        _userId = userId;
        _homeCity = homeCity;
				admin=admin1;
				rj=rj1;
    }

    public function get timestamp(): Date
    {
        return _timestamp;
    }

    public function get nick(): String
    {
        return _nick;
    }

    public function get text(): String
    {
        return _text;
    }

    public function get userId():Number {
        return _userId;
    }

    public function get homeCity():Number {
        return _homeCity;
    }

    public function set homeCity(value:Number):void {
        _homeCity = value;
    }

    public function set timestamp(value:Date):void
    {
        _timestamp = value;
    }

    public function set nick(value:String):void
    {
        _nick = value;
    }

    public function set text(value:String):void
    {
        _text = value;
    }

    public function set userId(value:Number):void
    {
        _userId = value;
    }
}
}