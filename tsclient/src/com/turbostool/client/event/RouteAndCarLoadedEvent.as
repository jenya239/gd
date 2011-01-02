package com.turbostool.client.event
{
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.game.components.car.Car;

import flash.events.Event;

public class RouteAndCarLoadedEvent extends Event
{
    public static const ROUTE_AND_CAR_LOADED: String = 'routeAndCarLoaded';

    private var _routeName: String;
    private var _carName: String;
    private var _raceWorld: RaceWorld;
    private var _car: Car;

    public function RouteAndCarLoadedEvent(routeName:String, carName:String, route:RaceWorld, car:Car)
    {
        super(ROUTE_AND_CAR_LOADED);
        _routeName = routeName;
        _carName = carName;
        _raceWorld = route;
        _car = car;
    }

    public function get car():Car
    {
        return _car;
    }

    public function get raceWorld():RaceWorld
    {
        return _raceWorld;
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