package com.turbostool.client.net.messages
{
public class ConnectRaceRequest extends ServerRequest
{
    public static const CONNECT_RACE: String = "connectRace";
    public static const DIRECTION_FORWARD: String = "forward";
    public static const DIRECTION_BACK: String = "back";

    private var _routeID: int;
    private var _direction: String;
    private var _lapNumber: int;

    public function ConnectRaceRequest(routeID: int, direction: String, lapNumber: int)
    {
        super(CONNECT_RACE);
        _routeID = routeID;
        _direction = direction;
        _lapNumber = lapNumber;
    }

    [Serializable(order=1)]
    public function get routeID(): int
    {
        return _routeID;
    }

    [Serializable(order=2)]
    public function get direction(): String
    {
        return _direction;
    }

    [Serializable(order=3)]
    public function get lapNumber(): int
    {
        return _lapNumber;
    }
}
}