package com.turbostool.carEditor
{
import com.turbostool.client.game.components.car.Car;

import flash.events.Event;

public class CarSelectedEvent extends Event
{
    public static const CAR_SELECTED: String = "carSelected";

    private var _car: Car;

    public function CarSelectedEvent(car: Car)
    {
        super(CAR_SELECTED);
        _car = car;
    }

    public function get car(): Car
    {
        return _car;
    }

}
}