package com.turbostool.client.net.messages
{
public class ChangeTriggerMessage extends ServerRequest
{
    public static const CHANGE_TRIGGER: String = "changeTrigger";

    public static const NITRO_TIP: String = "showNitroTip";
    public static const CLICK_ON_CAR_TIP: String = "showClickOnCarTip";
    public static const HOW_TO_DRIVE_TIP: String = "showHowToDriveTip";
    public static const TUTORIAL_STAGE: String = "tutorialStage";
    public static const INVITES_REWARD: String = "showInvitesReward";
    public static const LEVEL_UP: String = "levelUp";

    private var _trigger: String;
    private var _delta: Number;

    public function ChangeTriggerMessage(name: String, delta: Number)
    {
        super(CHANGE_TRIGGER);

        _trigger = name;
        _delta = delta;
    }

    [Serializable(order=1)]
    public function get trigger():String {
        return _trigger;
    }

    public function set trigger(value:String):void {
        _trigger = value;
    }

    [Serializable(order=2)]
    public function get delta():Number
    {
        return _delta;
    }

    public function set delta(value:Number):void
    {
        _delta = value;
    }
}
}