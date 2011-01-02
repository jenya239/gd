package com.turbostool.client.net.messages
{
import com.turbostool.client.model.LobbyRaceResult;

public class LobbyRaceResultsMessage extends ChanneledMessage
{
    public static const LOBBY_RACE_RESULTS: String = "lobbyRaceResults";

    private var _results: Array;
    private var _upgrades:Array;
    private var _wearOutUpgrades:Array;
    private var _blueScore: Number;
    private var _redScore: Number;

    public function LobbyRaceResultsMessage()
    {
        super(ChanneledMessage.LOBBY_CHANNEL);
    }

    public function get fuel():Number
    {
        var id:int = Client.instance.modelsStorage.userInfo.id;
        for each(var result:LobbyRaceResult in _results) {
            if (id == result.userID) return result.fuel;
        }
        return 0;
    }

    public function get results(): Array
    {
        return _results;
    }

    public function set results(val:Array):void
    {
        _results = val;
    }

    public function get upgrades():Array {
        return _upgrades;
    }

    public function set upgrades(val:Array):void {
        _upgrades = val;
    }

    public function get wearOutUpgrades():Array {
        return _wearOutUpgrades;
    }

    public function set wearOutUpgrades(value:Array):void {
        _wearOutUpgrades = value;
    }

    [Bindable]
    public function get blueScore():Number {
        return _blueScore;
    }

    public function set blueScore(value:Number):void {
        _blueScore = value;
    }

    [Bindable]
    public function get redScore():Number {
        return _redScore;
    }

    public function set redScore(value:Number):void {
        _redScore = value;
    }
}
}
