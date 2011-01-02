package com.turbostool.client.net.messages
{

public class AuthorizeVkontakteRequest extends ServerRequest
{
    public static const AUTHORIZE_VKONTAKTE: String = "authorizeVkontakte";

    private var _vkontakteID: String;
    private var _authKey: String;
    private var _firstLastName: String;
    private var _vkontakteOwnerID: String;

    public function AuthorizeVkontakteRequest(vkontakteID: String, authKey: String, firstLastName: String, vkontakteOwnerID: String)
    {
        super(AUTHORIZE_VKONTAKTE);
        _vkontakteID = vkontakteID;
        _authKey = authKey;
        _firstLastName = firstLastName;
        _vkontakteOwnerID = vkontakteOwnerID;
    }

    [Serializable(order=1)]
    public function get vkontakteID(): String
    {
        return _vkontakteID;
    }

    [Serializable(order=2)]
    public function get authKey():String
    {
        return _authKey;
    }

    [Serializable(order=3)]
    public function get firstLastName():String
    {
        return _firstLastName;
    }

    [Serializable(order=4)]
    public function get vkontakteOwnerID():String
    {
        return _vkontakteOwnerID;
    }
}
}