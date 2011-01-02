package com.turbostool.client.net.messages {
public class RepaintCarRequest extends ServerRequest{

    private var _carID:Number;
    private var _color:Number;

    public static const REPAINT_CAR: String = "repaintCar";

    public function RepaintCarRequest(carID:Number, color:Number) {
        super(REPAINT_CAR);
        _carID = carID;
        _color = color;
    }

    [Serializable(order=1)]
    public function get carID():Number {
        return _carID;
    }

    [Serializable(order=2)]
    public function get color():Number {
        return _color;
    }
}
}