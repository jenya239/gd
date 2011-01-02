package com.turbostool.client.event
{

public class SubmitRegDataClick extends ButtonClickEvent
{
    public static const REGISTER_BUTTON_CLICK: String = "REGISTER_BUTTON_CLICK";

    private var _nickname: String;
    private var _email: String;
    private var _password: String;

    public function SubmitRegDataClick(sender: Object, nickname: String, email: String, password: String)
    {
        super(REGISTER_BUTTON_CLICK, sender);
        _nickname = nickname;
        _email = email;
        _password = password;
    }

    public function get email():String
    {
        return _email;
    }

    public function get nickname():String
    {
        return _nickname;
    }

    public function get password():String
    {
        return _password;
    }
}
}