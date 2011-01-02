package com.turbostool.client.station {
import flash.events.Event;

public class StartTransferCommand extends Event {
    public static const START_TRANSFER: String = "startTransferCommand";

    public static const TRAIN: String = "train";
    public static const PLANE: String = "plane";

    private var _transferType: String;
    private var _citySrcID: Number;
    private var _cityDstID: Number;

    public function StartTransferCommand(type: String, citySrc: Number, cityDst: Number) {
        super(START_TRANSFER);

        _transferType = type;
        _citySrcID = citySrc;
        _cityDstID = cityDst;
    }

    public function get transferType():String {
        return _transferType;
    }

    public function set transferType(value:String):void {
        _transferType = value;
    }

    public function get citySrcID():Number {
        return _citySrcID;
    }

    public function set citySrcID(value:Number):void {
        _citySrcID = value;
    }

    public function get cityDstID():Number {
        return _cityDstID;
    }

    public function set cityDstID(value:Number):void {
        _cityDstID = value;
    }
}
}