package com.turbostool.client.controllers
{
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.game.components.car.Car;

public class BaseRacingStateMachine extends BaseStateMachine
{
    protected var _routeIDToLoad: int;
    protected var _isReverse: Boolean;
    protected var _lapNumber: int;
    public var raceWorld: RaceWorld;

    public var car: Car;

    public function get routeIDToLoad():int
    {
        return _routeIDToLoad;
    }

    public function set routeIDToLoad(val:int):void
    {
        _routeIDToLoad = val;
    }

    public function get isReverse():Boolean
    {
        return _isReverse;
    }

    public function set isReverse(val:Boolean):void
    {
        _isReverse = val;
    }

    public function get lapNumber():int
    {
        return _lapNumber;
    }

    public function set lapNumber(val:int):void
    {
        _lapNumber = val;
    }

    public function BaseRacingStateMachine()
    {
    }
}
}