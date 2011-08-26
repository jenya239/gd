package com.turbostool.client.net.messages {
import com.turbostool.client.model.CarClass;
import com.turbostool.client.model.UserInfo;

public class UnbanResponse extends ReasonedResponse {

    public static const UNBAN: String = "unban";

    public function UnbanResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }
}
}