package com.turbostool.client.utils
{
public class MethodRecord
{
    private var myMethodName:String;
    private var myCalls:Array;

    public function MethodRecord(methodName:String, callTime:uint = 0) {
        myCalls = new Array();
        myMethodName = methodName;
        if (callTime != 0) myCalls.push(callTime);
    }

    public function getName():String {
        return myMethodName;
    }

    public function getCalls():Array {
        return myCalls;
    }
}
}