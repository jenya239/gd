package com.turbostool.client.net.messages {
public class SellCarRequest extends ServerRequest{
    public static const SELL_CAR: String = "sellCar";

    private var _carID: int;

    public function SellCarRequest(carID: int)
    {
        super(SELL_CAR);
        _carID = carID;
    }

    [Serializable(order=1)]
    public function get carID(): int
    {
        return _carID;
    }
}
}