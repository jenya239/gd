package com.turbostool.client.net.messages {
public class UseCarUpgradeRequest extends ServerRequest
{
    public static const USE_CAR_UPGRADE: String = "useCarUpgrade";

    private var _id: int;

    public function UseCarUpgradeRequest(id: int)
    {
        super(USE_CAR_UPGRADE);
        _id = id;
    }
    
    [Serializable(order=1)]
    public function get id(): int
    {
        return _id;
    }
}
}
