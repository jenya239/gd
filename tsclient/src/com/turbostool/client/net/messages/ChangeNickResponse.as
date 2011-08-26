package com.turbostool.client.net.messages
{
import com.turbostool.client.model.ItemClass;
import com.turbostool.client.model.UserInfo;

public class ChangeNickResponse extends ReasonedResponse
{
		public static const CHANGE_NICK: String = "changeNick";

    public function ChangeNickResponse()
    {
        super(ChanneledMessage.GLOBAL_CHANNEL);
    }
}
}