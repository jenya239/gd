package com.turbostool.client.net.messages {
public class SetCarRequest extends ServerRequest{
    public static const SET_CAR: String = "setCar";

    private var _carID: int;

    public function SetCarRequest(carID: int)
    {
        super(SET_CAR);
        _carID = carID;
    }

    [Serializable(order=1)]
    public function get carID(): int
    {
        return _carID;
    }
}
}