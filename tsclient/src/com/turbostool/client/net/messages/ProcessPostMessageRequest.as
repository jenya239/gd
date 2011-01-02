package com.turbostool.client.net.messages
{
public class ProcessPostMessageRequest extends ServerRequest
{
    public static const PROCESS_POST_MESSAGE: String = "processPostMessage";

    private var _id: Number;
    private var _action: String;

    public function ProcessPostMessageRequest(id: Number, action: String)
    {
        super(PROCESS_POST_MESSAGE);

        _id = id;
        _action = action;
    }

    [Serializable(order=1)]
    public function get id():Number
    {
        return _id;
    }

    [Serializable(order=2)]
    public function get action():String
    {
        return _action;
    }
}
}
