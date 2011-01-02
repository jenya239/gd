package com.turbostool.client.net.messages {
public class BuyFuelRequest extends ServerRequest{

    public static const BUY_FUEL: String = "buyFuel";

    private var _fuelCount: Number;

    public function BuyFuelRequest(fCount:Number) {
        super(BUY_FUEL);
        _fuelCount = fCount;
    }

    [Serializable(order=1)]
    public function get fuelCount(): int
    {
        return _fuelCount;
    }

}
}