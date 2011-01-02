package com.turbostool.client.net.messages
{
public class GetFriendInfosRequest extends GetPropertyRequest
{
    public static const FRIEND_INFOS: String = "friendInfos";

    private var _friendIDs: String;

    public function GetFriendInfosRequest(friendIDs: String)
    {
        super(FRIEND_INFOS);
        _friendIDs = friendIDs;
    }

    [Serializable(order=2)]
    public function get friendIDs():String
    {
        return _friendIDs;
    }
}
}