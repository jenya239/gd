package com.turbostool.client.net.messages {
public class BanRequest extends ServerRequest{

    public static const BAN: String = "ban";

    private var _id:Number;
    private var _time:Number;

    public function BanRequest(id:Number, time:Number) {
        super(BAN);
        _id=id;
        _time=time;
    }

    [Serializable(order=1)]
    public function get id():Number {
        return _id;
    }

    [Serializable(order=2)]
    public function get time():Number {
        return _time;
    }

}
}