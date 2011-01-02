package com.turbostool.client.model {
public class WorkOfferInfo {
    public function WorkOfferInfo() {

    }
    public var id:Number;
    public var message:String="";
    public var time:Number;
    public var fuel:Number;

    public function toString():String{
        return message;
    }


}
}