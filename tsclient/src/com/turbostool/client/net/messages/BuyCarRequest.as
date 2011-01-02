package com.turbostool.client.net.messages {
public class BuyCarRequest extends ServerRequest{
    public static const BUY_CAR: String = "buyCar";

    private var _carClassID: int;
    private var _color: int;
    private var _needSlot: Boolean;

    public function BuyCarRequest(carClassID: int, color: int, needSlot: Boolean)
    {
        super(BUY_CAR);
        
        _carClassID = carClassID;
        _color = color;
        _needSlot = needSlot;
    }

    [Serializable(order=1)]
    public function get carClassID(): int
    {
        return _carClassID;
    }

    [Serializable(order=2)]
    public function get color(): int
    {
        return _color;
    }

    [Serializable(order=3)]
    public function get needSlot():Boolean {
        return _needSlot;
    }
}
}