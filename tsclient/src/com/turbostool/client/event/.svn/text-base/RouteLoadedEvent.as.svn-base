package com.turbostool.client.event
{
import flash.events.Event;

public class RouteLoadedEvent extends Event
{
    public static const ROUTE_LOADED: String = 'routeLoaded';

    private var _routeName: String;

    public function RouteLoadedEvent(routeName: String)
    {
        super(ROUTE_LOADED);
        _routeName = routeName;
    }

    public function get routeName(): String
    {
        return _routeName;
    }
}
}