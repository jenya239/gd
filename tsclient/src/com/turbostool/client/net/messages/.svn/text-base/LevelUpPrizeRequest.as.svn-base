package com.turbostool.client.net.messages {
public class LevelUpPrizeRequest extends ServerRequest{

    public static const LEVEL_UP_PRIZE: String = "levelUpPrize";

    public static const MONEY: String = "money";
    public static const ITEM: String = "item";
    public static const NITRO: String = "nitro";

    private var _level: Number;
    private var _type: String;

    public function LevelUpPrizeRequest(level: Number, type: String) {
        super(LEVEL_UP_PRIZE);

        _level = level;
        _type = type;
    }

    [Serializable(order=1)]
    public function get level():Number {
        return _level;
    }

    [Serializable(order=2)]
    public function get type():String {
        return _type;
    }
}
}