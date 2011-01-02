package com.turbostool.client.net.messages
{
public class JoinRaceRequest extends ServerRequest
{
    public static const JOIN_RACE: String = "joinRace";

    private var _raceID: int;

    public function JoinRaceRequest(raceID: int)
    {
        super(JOIN_RACE);
        _raceID = raceID;
    }

    [Serializable(order=1)]
    public function get raceID(): int
    {
        return _raceID;
    }
}
}