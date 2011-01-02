package com.turbostool.carEditor
{
import com.turbostool.client.game.RaceWorld;

import flash.events.Event;

public class RouteSelectedEvent extends Event
{
    public static const ROUTE_SELECTED: String = "routeSelected";

    private var _raceWorld: RaceWorld;

    public function RouteSelectedEvent(rw: RaceWorld)
    {
        super(ROUTE_SELECTED);
        _raceWorld = rw;
    }

    public function get raceWorld(): RaceWorld
    {
        return _raceWorld;
    }

}
}