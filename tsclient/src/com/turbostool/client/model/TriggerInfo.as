package com.turbostool.client.model
{
public class TriggerInfo
{
    private var _name: String;
    private var _value: Number;

    public function get name(): String
    {
        return _name;
    }

    public function set name(value:String): void
    {
        _name = value;
    }

    public function get value():Number {
        return _value;
    }

    public function set value(value:Number):void {
        _value = value;
    }
}
}