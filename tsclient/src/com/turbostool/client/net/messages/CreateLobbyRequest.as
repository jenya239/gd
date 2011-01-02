package com.turbostool.client.net.messages
{
public class CreateLobbyRequest extends ServerRequest
{
    public static const CREATE_LOBBY: String = "createLobby";

    private var _routeID : int;
    private var _lapNumber : int;
    private var _direction : String;
    private var _playerMax : int;
    private var _allowedCarID : int;
    private var _timerLength : int;
    private var _league:int;
    private var _stake:int;

    public function CreateLobbyRequest(routeID: int, lapNumber: int, direction: String, allowedCarID: int, playerMax: int, timerLength: int, league:int, stake:int)
    {
        super(CREATE_LOBBY);

        _routeID = routeID;
        _lapNumber = lapNumber;
        _direction = direction;
        _playerMax = playerMax;
        _allowedCarID = allowedCarID;
        _timerLength = timerLength;
        _league = league;
	_stake = stake;	
    }

    [Serializable(order=1)]
    public function get routeID():int
    {
        return _routeID;
    }

    [Serializable(order=2)]
    public function get lapNumber():int
    {
        return _lapNumber;
    }

    [Serializable(order=3)]
    public function get direction():String
    {
        return _direction;
    }

    [Serializable(order=4)]
    public function get playerMax():int
    {
        return _playerMax;
    }

    [Serializable(order=5)]
    public function get allowedCarID():int
    {
        return _allowedCarID;
    }

    [Serializable(order=6)]
    public function get timerLength():int
    {
        return _timerLength;
    }

    [Serializable(order=7)]
    public function get league():int {
        return _league;
    }
    
    [Serializable(order=8)]
    public function get stake():int {
        return _stake;
    }
}
}