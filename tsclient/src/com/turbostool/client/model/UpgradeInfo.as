package com.turbostool.client.model {
[Bindable]
public class UpgradeInfo {

    public var userID:int;
    public var power:Number;
    public var speed:Number;
    public var controllability:Number;
    public var braking:Number;

    public function UpgradeInfo() {
    }

    public function toString():String {
        return "UpgradeInfo ClientID = " + userID + " power = " + power;
    }
}
}