package com.turbostool.client.net.messages {
public class UnbanRequest extends ServerRequest{

    public static const UNBAN: String = "unban";

    private var _id:Number;

    public function UnbanRequest(id:Number) {
				super(UNBAN);
        _id=id;
    }

    [Serializable(order=1)]
    public function get id():Number {
        return _id;
    }

}
}