package com.turbostool.client.net.messages
{
import com.turbostool.client.model.UserInfo;

public class ChatMessageMessage extends ChanneledMessage
{
    public static const CHAT_MESSAGE: String = "chatMessage";

    private var _text: String;
    private var _clientID: int;
    private var _timeStamp: Number;
    private var _nick: String;
    private var _userId: Number;
    private var _homeCity: Number
    public var admin:String;
    
    public function ChatMessageMessage(text_: String = "", clientID: int = -1, channel: String = ChanneledMessage.GLOBAL_CHANNEL, nick: String = "", homeCity: Number =0)
    {
        super(channel, CHAT_MESSAGE);
        this._text = text_;
        this._clientID = clientID;
        this._nick = nick;
        this._homeCity = homeCity;
    }

    [Serializable(order=2, escape="true")]
    public function get text(): String
    {
        return _text;
    }

    public function get clientID(): int
    {
        return _clientID;
    }

    public function set text(value:String):void
    {
        _text = value;
    }

    public function set clientID(value:int):void
    {
        _clientID = value;
    }

    [Serializable(order=3)]
    public function get nick():String
    {
        return _nick;
    }

    public function set nick(value:String):void
    {
        _nick = value;
    }

    public function set timeStamp(value:Number):void
    {
        _timeStamp = value;
    }

    public function get timeStampDate(): Date
    {
        return new Date(_timeStamp);
    }

    public function get userId():Number {
        return _userId;
    }

    public function set userId(value:Number):void {
        _userId = value;
    }

    public function get homeCity():Number {
        return _homeCity;
    }

    public function set homeCity(value:Number):void {
        _homeCity = value;
    }
}
}