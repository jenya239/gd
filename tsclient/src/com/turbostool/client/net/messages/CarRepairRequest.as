package com.turbostool.client.net.messages {
public class CarRepairRequest extends ServerRequest{

    public static const REPAIR: String = "repairCar";
    private var _carID: Number;
    public function CarRepairRequest(carID:Number) {
        super(REPAIR);
        _carID = carID;
    }

    [Serializable(order=1)]
    public function get carID(): Number
    {
        return _carID;
    }

}
}