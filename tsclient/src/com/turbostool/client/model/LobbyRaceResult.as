package com.turbostool.client.model
{
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.EventDispatcher;

[Bindable]
public class LobbyRaceResult extends EventDispatcher
{
	private var _clientID: int;
    private var _userID: int;
	private var _displayName: String;
	private var _time: Number;
    private var _oldRating: Number;
	private var _newRating: Number;
	private var _fuel:Number;
	private var _money: Number;
	private var _experience: Number;
    private var _position: Number;
    private var _score: Number;

    public function get position():Number {
        return _position;
    }

    public function set position(val:Number):void {
        _position = val;
    }

    public function get fuel():Number {
		return _fuel;
	}

	public function set fuel(val:Number):void {
		_fuel = val;
	}
      public function get userID():int {
        return _userID;
    }

    public function set userID(val:int):void {
        _userID = val;
    }
	public function get clientID():int
	{
		return _clientID;
	}

	public function set clientID(value:int):void
	{
		_clientID = value;
	}

	public function get displayName():String
	{
		return _displayName;
	}

	public function set displayName(value:String):void
	{
		_displayName = value;
	}

	public function get time():Number
	{
		return _time;
	}

	public function set time(value:Number):void
	{
		_time = value;
	}

	public function get oldRating():Number
	{
		return _oldRating;
	}

	public function set oldRating(value:Number):void
	{
		_oldRating = value;
	}

	public function get newRating():Number
	{
		return _newRating;
	}

	public function set newRating(value:Number):void
	{
		_newRating = value;
	}

	override public function toString():String
	{
		var timeStr: String = "";
		if (_time < 0)
			timeStr = "Не финишировал";
		else
			timeStr = Utils.formatTime(_time);

		var difference: Number = newRating - oldRating;
		var resultStr: String = _displayName + ": " + timeStr + ". Рейтинг: " + newRating;
		if (difference >= 0)
		{
			resultStr += " (+" + difference + ")";
		}
		else
		{
			resultStr += " (" + difference + ")";
		}
		resultStr += " денег " + money;
		resultStr += " бензина осталось" + fuel;
		return resultStr;
	}

	public function get money():Number {
		return _money;
	}

	public function set money(val:Number):void {
		_money = val;
	}

	public function LobbyRaceResult() {
	}

	public static function create(id:int, displayName:String, time:Number, oldRating:Number, newRating:Number, fuel:Number, money:Number, experience:Number, userId:int): LobbyRaceResult {
		var r: LobbyRaceResult = new LobbyRaceResult();
		r._clientID = id;
		r._displayName = displayName;
		r._time = time;
		r._oldRating = oldRating;
		r._newRating = newRating;
		r._fuel = fuel;
		r._money = money;
		r._experience = experience;
		r._userID = userId;
		return r;
	}

	public function get experience():Number {
		return _experience;
	}

	public function set experience(value:Number):void {
		_experience = value;
	}

    public function get score():Number
    {
        return _score;
    }

    public function set score(value:Number):void
    {
        _score = value;
    }
}
}
