package com.turbostool.client.net.messages {
public class LevelUpRewardsRequest extends ServerRequest{

    public static const LEVEL_UP_REWARDS: String = "levelUpRewards";

    private var _level: Number;

    public function LevelUpRewardsRequest(level: Number) {
        super(LEVEL_UP_REWARDS);

        _level = level;
    }

    [Serializable(order=1)]
    public function get level():Number {
        return _level;
    }
}
}