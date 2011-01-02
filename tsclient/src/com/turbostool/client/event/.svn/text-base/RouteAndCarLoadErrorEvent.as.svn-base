package com.turbostool.client.event
{
import flash.events.Event;

public class RouteAndCarLoadErrorEvent extends Event
{
    public static const ROUTE_AND_CAR_LOAD_ERROR: String = 'routeAndCarLoadError';

    private var _routeName: String;
    private var _carName: String;

    public function RouteAndCarLoadErrorEvent(routeName: String, carName: String)
    {
        super(ROUTE_AND_CAR_LOAD_ERROR);
        _routeName = routeName;
        _carName = carName;
    }

    public function get routeName(): String
    {
        return _routeName;
    }

    public function get carName():String
    {
        return _carName;
    }
}
}