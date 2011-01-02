package com.turbostool.client.net.messages
{
public class QuitLobbyRequest extends ServerRequest
{
    public static const QUIT_LOBBY: String = "quitLobby";

    private var _reason: String = "";

    public function QuitLobbyRequest()
    {
        super(QUIT_LOBBY);
    }

    [Serializable(order=1)]
    public function get reason():String {
        return _reason;
    }

    public function set reason(val:String):void {
        _reason = val;
    }
}
}