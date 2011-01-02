package com.turbostool.client.net.messages {
import com.turbostool.client.net.*;
import com.turbostool.client.model.GasInfo;
import com.turbostool.client.model.UserInfo;
import com.turbostool.client.net.messages.ChanneledMessage;
import com.turbostool.client.net.messages.ReasonedResponse;
import mx.controls.Alert;
public class CancelWorkResponse extends ReasonedResponse{
    public static const CANCEL_WORK: String = "cancelWork";
    public function CancelWorkResponse() {
        super(ChanneledMessage.GLOBAL_CHANNEL);
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