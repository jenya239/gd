package com.turbostool.client.registration
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.RegisteredEvent;
import com.turbostool.client.model.CarInfo;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.RegisterRequest;
import com.turbostool.client.net.messages.RegisterResponse;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.utils.collections.FilteredCollection;

import flash.events.EventDispatcher;

import mx.collections.ArrayCollection;

[Bindable]
public class RegistrationController extends EventDispatcher
{
    private var _nickname: String = "";
    private var _defaultNickname: String = "";
    private var _city: Number = -1;
    private var _carClassId: Number = -1;
    private var _colorIndex: Number = -1;
    private var _socket: SessionSocket;
    private var _modelsStorage: ModelsStorage;
    private var _state: String = "";

    public function RegistrationController(socket: SessionSocket, storage: ModelsStorage)
    {
        _socket = socket;
        _modelsStorage = storage;

        EventManager.globalChannel.addEventListener(RegisterResponse.REGISTER, onRegister);
    }

    public function get defaultNickname():String
    {
        return _defaultNickname;
    }

    public function set defaultNickname(value:String):void
    {
        _defaultNickname = value;
    }

    public function get nickname(): String
    {
        return _nickname;
    }

    public function set nickname(value: String):void
    {
        _nickname = value;
    }

    public function get city(): Number
    {
        return _city;
    }

    public function set city(value: Number): void
    {
        _city = value;
    }

    public function get carClassId():int
    {
        return _carClassId;
    }

    public function set carClassId(value:int):void
    {
        _carClassId = value;
    }

    public function get colorIndex(): int
    {
        return _colorIndex;
    }

    public function set colorIndex(value: int): void
    {
        _colorIndex = value;
    }

    public function get state():String
    {
        return _state;
    }

    public function set state(value:String):void
    {
        _state = value;
    }

    public function get currentCarInfo(): CarInfo
    {
        return _modelsStorage.cars.filter(function(e:*, i:int, arr:Array):Boolean{ return e.classID == _carClassId; })[0];     //[_carClassId - 1];
    }

    public function register(): void
    {
        trace("NICK : " + (_nickname != _defaultNickname));
        _socket.sendMessage(new RegisterRequest(_nickname != _defaultNickname ? _nickname : "", _carClassId, _colorIndex, _city));
    }

    private function onRegister(event: ServerResponseEvent): void
    {
        var response: RegisterResponse = RegisterResponse(event.response);
        if (response.isOK)
        {
            _modelsStorage.userInfo = response.userInfo;

            EventManager.globalChannel.dispatchEvent(new RegisteredEvent());
        }

        this.dispatchEvent(event);
    }

    public function get modelsStorage(): ModelsStorage
    {
        return _modelsStorage;
    }

    private function filterCars(carInfo: CarInfo): Boolean
    {
        return carInfo.minLevel == 1;
    }

    public function get cars(): ArrayCollection
    {
        return new FilteredCollection(_modelsStorage.cars, filterCars);
    }
}
}