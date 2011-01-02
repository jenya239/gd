package com.turbostool.client.net.messages
{
public class SetPropertyRequest extends ServerRequest
{
    public static const SET_PROPERTY: String = "set";
    public static const CURRENT_CAR_ID: String = "currentCarID";

    private var _property: String = "carName";
    private var _value: Object;

    public function SetPropertyRequest(property: String, value: *)
    {
        super(SET_PROPERTY);
        _property = property;
        _value = value;
    }

    [Serializable(order=1)]
    public function get property():String
    {
        return _property;
    }

    [Serializable(order=2)]
    public function get value(): String
    {
        return _value.toString();
    }

}
}