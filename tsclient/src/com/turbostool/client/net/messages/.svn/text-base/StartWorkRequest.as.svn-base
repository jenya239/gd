package com.turbostool.client.net.messages {
public class StartWorkRequest extends ServerRequest{

    public static const START_WORK: String = "startWork";

    private var _workID: int;

    public function StartWorkRequest(workID: int)
    {
        super(START_WORK);
        _workID = workID;
    }

    [Serializable(order=1)]
    public function get workID(): int
    {
        return _workID;
    }


}
}