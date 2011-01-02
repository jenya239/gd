package com.turbostool.client.net.messages {
import com.turbostool.client.model.CarClass;
import com.turbostool.client.model.ItemClass;
import com.turbostool.client.model.ItemInfo;
import com.turbostool.client.model.RouteInfo;
import com.turbostool.client.model.UserInfo;

import mx.collections.ArrayCollection;

public class LevelUpPrizeResponse extends ReasonedResponse {

    public static const LEVEL_UP_PRIZE: String = "levelUpPrize";

    private var _userInfo: UserInfo;

    public function LevelUpPrizeResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get userInfo(): UserInfo
    {
        return _userInfo;
    }

    public function set userInfo(val: UserInfo): void
    {
        _userInfo = val;
    }
}
}