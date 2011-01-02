package com.turbostool.client.net.messages {
import com.turbostool.client.model.GasInfo;
import com.turbostool.client.model.UserInfo;
public class FinishWorkMessage extends ChanneledMessage{

    public static const FINISH_WORK: String = "finishWork";

    public function FinishWorkMessage() {
        super(ChanneledMessage.GLOBAL_CHANNEL, FINISH_WORK);
    }

    private var _gasInfo:GasInfo;
    private var _userInfo:UserInfo;

    public function get gasInfo():GasInfo {
        return _gasInfo;
    }

    public function set gasInfo(val:GasInfo):void {
        _gasInfo = val;
    }

    public function get userInfo():UserInfo {
        return _userInfo;
    }

    public function set userInfo(val:UserInfo):void {
        _userInfo = val;
    }
}
}