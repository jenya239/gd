package com.turbostool.client.net.messages
{
public class ReadinessRequest extends ServerRequest
{
    public static const READINESS: String = "readiness";

    public static const NONE: String = "none";
    public static const LOADED: String = "loaded";
    public static const INITIALIZED: String = "initialized";

    private var _value: String;

    public function ReadinessRequest(readiness: String)
    {
        super(READINESS);
        _value = readiness;
    }

    [Serializable(order=1)]
    public function get value():String
    {
        return _value;
    }
}
}