package com.turbostool.client.event
{
public class ConnectButtonClickEvent extends ButtonClickEvent
{
    public static const CONNECT_BUTTON_CLICK: String = "CONNECT_BUTTON_CLICK";

    private var _routeID: int;
    private var _direction: String;
    private var _lapNumber: int;

    public function ConnectButtonClickEvent(sender:Object, routeID: int, direction: String, lapNumber: int)
    {
        super(CONNECT_BUTTON_CLICK, sender);
        _routeID = routeID;
        _direction = direction;
        _lapNumber = lapNumber;
    }

    public function get routeID(): int
    {
        return _routeID;
    }

    public function get direction(): String
    {
        return _direction;
    }

    public function get lapNumber(): int
    {
        return _lapNumber;
    }

}
}