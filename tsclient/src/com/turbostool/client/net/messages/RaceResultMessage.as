package com.turbostool.client.net.messages
{
public class RaceResultMessage extends ChanneledMessage
{
    public static const RACE_RESULT: String = "raceResult";

    private var _rewardType: String;
    private var _experience: int;
    private var _time: Number;

    public function RaceResultMessage()
    {
        super(ChanneledMessage.RACE_CHANNEL);
    }

    public function get rewardTypeText(): String
    {
        switch (_rewardType)
                {
            case "goldMedal":
                return "золотую";
            case "silverMedal":
                return "серебряную";
            case "bronzeMedal":
                return "бронзовую";
            default:
                return "";
        }
    }

    public function get rewardType():String
    {
        return _rewardType;
    }

    public function set rewardType(val:String):void
    {
        _rewardType = val;
    }

    public function get experience():int
    {
        return _experience;
    }

    public function set experience(val:int):void
    {
        _experience = val;
    }

    public function get time():Number
    {
        return _time;
    }

    public function set time(val:Number):void
    {
        _time = val;
    }
}
}