package com.turbostool.client.net.messages
{
import com.turbostool.client.utils.TSError;

public class ReasonedResponse extends ChanneledMessage
{
    public static const RESULT_OK: String = "ok";
    public static const RESULT_ERROR: String = "error";

    private var _result: String;
    private var _reason: String;
    private var _message: String;
    private var _error: TSError;

    public function ReasonedResponse(channel:String)
    {
        super(channel);
    }

    public function get result():String
    {
        return _result;
    }

    public function set result(val:String):void
    {
        _result = val;
    }

    public function get reason():String
    {
        return _reason;
    }

    public function set reason(val:String):void
    {
        _reason = val;
    }

    public function get message():String
    {
        return _message;
    }

    public function set message(val:String):void
    {
        _message = val;
    }

    public function get error():TSError
    {
        return new TSError(message);
    }

    public function get isOK(): Boolean
    {
        return result == ReasonedResponse.RESULT_OK;
    }

}
}