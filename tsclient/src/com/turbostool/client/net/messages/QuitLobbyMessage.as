package com.turbostool.client.net.messages
{
public class QuitLobbyMessage extends ChanneledMessage
{
    public static const QUIT_LOBBY: String = "quitLobby";

    private var _reason: String;

    public function QuitLobbyMessage()
    {
        super(ChanneledMessage.LOBBY_CHANNEL);
    }

    public function get reason():String {
        return _reason;
    }

    public function set reason(val:String):void {
        _reason = val;
    }
}
}