package com.turbostool.client.event
{
public class LoginButtonClickEvent extends ButtonClickEvent
{
    public static const LOGIN_BUTTON_CLICK: String = "LOGIN_BUTTON_CLICK";

    private var _login: String;
    private var _password: String;

    public function LoginButtonClickEvent(sender:Object, login: String, password: String)
    {
        super(LOGIN_BUTTON_CLICK, sender);
        _login = login;
        _password = password;
    }

    public function get login(): String
    {
        return _login;
    }

    public function get password(): String
    {
        return _password;
    }

}
}