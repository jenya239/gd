package com.turbostool.client.net.messages {
public class LevelUpInfoRequest extends ServerRequest{

    public static const LEVEL_UP_INFO: String = "levelUpInfo";

    private var _level: Number;

    public function LevelUpInfoRequest(level: Number) {
        super(LEVEL_UP_INFO);

        _level = level;
    }

    [Serializable(order=1)]
    public function get level():Number {
        return _level;
    }
}
}