package com.turbostool.client.net.messages
{
public class LoadingProgressMessage extends ChanneledMessage
{
    public static const LOADING_PROGRESS: String = "loadingProgress";
    private var _progress: int;
    private var _clientID: int;

    public function LoadingProgressMessage(progress: int = 0, channel: String = "")
    {
        super(channel, LOADING_PROGRESS);

        _progress = progress;
    }

    [Serializable(order=2)]
    public function get progress():int
    {
        return _progress;
    }

    public function set progress(val:int):void
    {
        _progress = val;
    }

    public function get clientID():int
    {
        return _clientID;
    }

    public function set clientID(val:int):void
    {
        _clientID = val;
    }
}
}