package com.turbostool.client.event
{
import flash.events.Event;

public class RouteLoadErrorEvent extends Event
{
    public static const ROUTE_LOAD_ERROR: String = 'routeLoadError';

    private var _routeName: String;
    private var _error: Error;

    public function RouteLoadErrorEvent(routeName: String, error: Error)
    {
        super(ROUTE_LOAD_ERROR);
        _routeName = routeName;
        _error = error;
    }

    public function get routeName(): String
    {
        return _routeName;
    }

    public function get error(): Error
    {
        return _error;
    }
}
}