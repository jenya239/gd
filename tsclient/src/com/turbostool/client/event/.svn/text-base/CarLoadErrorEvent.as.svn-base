package com.turbostool.client.event
{
import flash.events.Event;

public class CarLoadErrorEvent extends Event
{
    public static const CAR_LOAD_ERROR: String = "carLoadError";

    private var _carName: String;
    private var _error: Error;

    public function CarLoadErrorEvent(carName: String, error: Error)
    {
        super(CAR_LOAD_ERROR);
        _carName = carName;
        _error = error;
    }

    public function get carName(): String
    {
        return _carName;
    }

    public function get error(): Error
    {
        return _error;
    }
}
}