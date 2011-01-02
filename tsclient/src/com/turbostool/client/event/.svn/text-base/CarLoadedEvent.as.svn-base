package com.turbostool.client.event
{
import com.turbostool.client.game.components.car.Car;

import flash.events.Event;

public class CarLoadedEvent extends Event
{
    public static const CAR_LOADED: String = "carLoaded";

    private var _car: Car;
    private var _carName: String;
    private var _clientID: int;

    public function CarLoadedEvent(carName: String, clientID: int, car: Car)
    {
        super(CAR_LOADED);
        _carName = carName;
        _car = car;
        _clientID = clientID;
    }

    public function get car(): Car
    {
        return _car;
    }

    public function get carName(): String
    {
        return _carName;
    }

    public function get clientID():int
    {
        return _clientID;
    }

    public override function toString(): String
    {
        return _carName + " " + _car.clientID;
    }
}
}