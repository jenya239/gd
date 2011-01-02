package com.turbostool.client.net.messages
{
public class GetSecureUserInfoRequest extends GetPropertyRequest
{
    public static const SECURE_USER_INFO: String = "secureUserInfo";

    private var _userID: int;

    public function GetSecureUserInfoRequest(userID: int)
    {
        super(SECURE_USER_INFO);

        _userID = userID;
    }

    [Serializable(order=2)]
    public function get userID():int
    {
        return _userID;
    }
}
}