package com.turbostool.client.net.messages {
public class RemoveCarUpgradeRequest extends ServerRequest
{
    public static const REMOVE_CAR_UPGRADE: String = "removeCarUpgrade";

    private var _id: int;

    public function RemoveCarUpgradeRequest(id: int)
    {
        super(REMOVE_CAR_UPGRADE);
        
        _id = id;
    }
    
    [Serializable(order=1)]
    public function get id(): int
    {
        return _id;
    }
}
}
