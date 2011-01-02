package com.turbostool.client.net.messages
{
public class LobbyRaceEventMessage extends ChanneledMessage
{
    public static const LOBBY_RACE_EVENT: String = "lobbyRaceEvent";

    public static const FINISH_COUNTDOWN_STARTED: String = "finishCountDownStarted";
    public static const OTHER_CAR_FINISHED: String = "otherCarFinished";
    public static const GO: String = "go";
    public static const INITIALIZE: String = "initialize";
    public static const COUNTDOWN: String = "countdown";

    private var _type: String;
    private var _param: String;
    private var _clientID: int;

    public function LobbyRaceEventMessage()
    {
        super(ChanneledMessage.LOBBY_CHANNEL);
    }

    public function get type():String
    {
        return _type;
    }

    public function set type(val:String):void
    {
        _type = val;
    }

    public function get param():String
    {
        return _param;
    }

    public function set param(val:String):void
    {
        _param = val;
    }

    public function get clientID():int
    {
        return _clientID;
    }

    public function set clientID(val:int):void
    {
        _clientID = val;
    }

    public function get floatParam(): Number
    {
        return parseFloat(_param);
    }

}
}