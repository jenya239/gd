package com.turbostool.client.model {
import flash.events.EventDispatcher;
[Bindable]
public class GasInfo extends EventDispatcher {
    private var _fuelPrice:Number;
    private var _maxFuelCount:Number;
    private var _jobOffers:Array;

    public var message0:String;
    public var message1:String;
    public var message2:String;
    public var message3:String;

    public function GasInfo() {
        _jobOffers = new Array();
        for(var i:Number=0; i<4;i++){
            _jobOffers[i] = new WorkOfferInfo();
        }
    }

    public function get fuelPrice():Number {
        return _fuelPrice;
    }

    public function set fuelPrice(val:Number):void {
        _fuelPrice = val;
    }

    public function get jobOffers():Array {
        return _jobOffers;
    }

    public function set jobOffers(val:Array):void {
        _jobOffers = val;
        for(var i:int=0; i<4; i++){
            this["message"+i] = _jobOffers[i];
        }
    }

    public function get maxFuelCount():Number {
        return _maxFuelCount;
    }

    public function set maxFuelCount(val:Number):void {
        _maxFuelCount = val;
    }
}
}