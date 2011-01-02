package com.turbostool.client.net.messages {
import com.turbostool.client.model.RecolorInfo;
import com.turbostool.client.model.UserInfo;
public class RepaintCarResponse extends ReasonedResponse{
    public static const REPAINT_CAR: String = "repaintCar";

    private var _userInfo: UserInfo;
    private var _recolorInfo: RecolorInfo;

    public function RepaintCarResponse() {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }

    public function get userInfo():UserInfo {
        return _userInfo;
    }

    public function set userInfo(val:UserInfo):void {
        _userInfo = val;
    }

    public function get recolorInfo():RecolorInfo {
        return _recolorInfo;
    }

    public function set recolorInfo(value:RecolorInfo):void {
        _recolorInfo = value;
    }
}
}