package com.turbostool.client.net.messages
{
public class StartTransferRequest extends ServerRequest
{
    public static const START_TRANSFER: String = "startTransfer";

    private var _transitionType: String;
    private var _citySrcID: Number;
    private var _cityDstID: Number;

    public function StartTransferRequest(transitionType: String, citySrcID: Number, cityDstID: Number)
    {
        super(START_TRANSFER);

        _transitionType = transitionType;
        _citySrcID = citySrcID;
        _cityDstID = cityDstID;
    }

    [Serializable(order=1)]
    public function get transitionType():String {
        return _transitionType;
    }

    public function set transitionType(value:String):void {
        _transitionType = value;
    }

    [Serializable(order=2)]
    public function get citySrcID():Number {
        return _citySrcID;
    }

    public function set citySrcID(value:Number):void {
        _citySrcID = value;
    }

    [Serializable(order=3)]
    public function get cityDstID():Number {
        return _cityDstID;
    }

    public function set cityDstID(value:Number):void {
        _cityDstID = value;
    }
}
}