package com.turbostool.client.net.messages
{
public class RegisterRequest extends ServerRequest
{
    public static const REGISTER: String = "register";

    private var _nickname: String;
    private var _car: Number;
    private var _color: Number;
    private var _city: Number;

    public function RegisterRequest(nickname: String, car: Number, color: Number, city: Number)
    {
        super(REGISTER);

        _nickname = nickname;
        _car = car;
        _color = color;
        _city = city;
    }

    [Serializable(order=1, escape=true)]
    public function get nickname():String
    {
        return _nickname;
    }

    [Serializable(order=2)]
    public function get car():Number {
        return _car;
    }

    [Serializable(order=3)]
    public function get color():Number {
        return _color;
    }

    [Serializable(order=4)]
    public function get city():Number {
        return _city;
    }
}
}