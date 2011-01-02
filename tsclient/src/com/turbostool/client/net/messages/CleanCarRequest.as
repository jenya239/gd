package com.turbostool.client.net.messages {
public class CleanCarRequest extends ServerRequest{

    public static const CLEAN_CAR: String = "cleanCar";

    private var _friendID: Number;

    public function CleanCarRequest(friendID: Number) {
        super(CLEAN_CAR);

        _friendID = friendID;
    }

    [Serializable(order=1)]
    public function get friendID():Number {
        return _friendID;
    }
}
}