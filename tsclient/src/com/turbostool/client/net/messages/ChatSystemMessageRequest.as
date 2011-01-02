package com.turbostool.client.net.messages
{

public class ChatSystemMessageRequest extends ServerRequest
{
    public static const CHAT_MESSAGE: String = "chatSystemMessage";

    private var _text: String;
    private var _timeStamp: Number;

    public function ChatSystemMessageRequest(text: String)
    {
        super(CHAT_MESSAGE);
        this._text = text;
    }

    [Serializable(order=1, escape="true")]
    public function get text(): String
    {
        return _text;
    }

    public function set text(value:String):void
    {
        _text = value;
    }
}
}