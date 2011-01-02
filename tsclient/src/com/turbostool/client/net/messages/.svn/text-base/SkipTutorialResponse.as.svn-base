package com.turbostool.client.net.messages {
import com.turbostool.client.model.UserInfo;

public class SkipTutorialResponse extends ReasonedResponse{

    public static const SKIP_TUTORIAL: String = "skipTutorial";

    private var _userInfo: UserInfo;

    public function SkipTutorialResponse()
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
