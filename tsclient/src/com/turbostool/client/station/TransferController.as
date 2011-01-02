package com.turbostool.client.station {
import com.turbostool.client.Tracker;
import com.turbostool.client.city.ScreenSelectedCommand;
import com.turbostool.client.event.EventManager;

import com.turbostool.client.model.UserState;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.EndTransferRequest;
import com.turbostool.client.net.messages.EndTransferResponse;

import com.turbostool.client.net.messages.GetPropertyRequest;
import com.turbostool.client.net.messages.GetPropertyResponse;
import com.turbostool.client.net.messages.GetUserStateResponse;
import com.turbostool.client.net.messages.ReasonedResponse;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.net.messages.StartTransferRequest;

import com.turbostool.client.net.messages.StartTransferResponse;

import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.TimerEvent;
import flash.utils.Timer;

import mx.controls.Alert;

public class TransferController extends EventDispatcher{
    public static const TRANSITION: String = "transition";
    public static const STATION_HALL: String = "stationHall";

    private var _timer: Timer;
    private var _userState: UserState;

    public function TransferController() {
        EventManager.instance.addEventListener(StartTransferCommand.START_TRANSFER, onStartTransfer);
        EventManager.instance.addEventListener(StartTransferResponse.START_TRANSFER, onStartTransferResponse);
        EventManager.instance.addEventListener(GetUserStateResponse.GET_USERSTATE_RESPONSE, onUserStateResponse);

        _timer = new Timer(1000, 0);
        _timer.addEventListener("timer", transferTimerHandler);
    }

    private function transferTimerHandler(e: TimerEvent): void {
        if(state == TRANSITION) {
            dispatchEvent(new Event("transitionTimerChanged"));

            var left: Number = arrivalTime - Utils.now();
            if(left <= 0) {
                SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.USER_STATE));
            }
        }
    }

    private function onUserStateResponse(event: ServerResponseEvent):void {
        var message: GetUserStateResponse = GetUserStateResponse(event.response);
        userState = message.userState;
        
        if(message.isOK) {
            if(message.currentCity != Client.instance.modelsStorage.userInfo.currentCity) {
                Client.instance.modelsStorage.userInfo.currentCity = message.currentCity;
               _timer.stop();
                EventManager.instance.dispatchEvent(new ScreenSelectedCommand("map"));
            } else {
                if(!_timer.running)
                _timer.start();
            }
        } else {
            if(!_timer.running)
                _timer.start();
        }
    }

    private function onStartTransfer(event:StartTransferCommand):void {
        SessionSocket.instance.sendMessage(new StartTransferRequest(event.transferType, event.citySrcID, event.cityDstID));
    }

    private function onStartTransferResponse(event:ServerResponseEvent):void {
        var response: StartTransferResponse = StartTransferResponse(event.response);

        if(response.isOK) {
            userState = response.userState;
            Client.instance.modelsStorage.userInfo = response.userInfo;
            _timer.start();
            dispatchEvent(new Event("transitionTimerChanged"));

            Tracker.instance.trackPageview('/traveling/' + userState.userTransferState.type);
            if(userState.userTransferState.type == 'plain') {
                Tracker.instance.trackTrans('plane', 'traveling', 0, Client.instance.modelsStorage.globalInfo.transferTimePlane);
            }

        } else {
            Alert.show(response.message);
        }
    }

    [Bindable(event="transitionTimerChanged")]
    public function timeLeftString(): String {
        var left: Number = arrivalTime - Utils.now();
        left = left > 0 ? left : 0;
        var time: int = left / 1000;
        var minutes: int = time / 60;
        var seconds: int = time % 60;

        var secondsString: String = "" + seconds;
        if(secondsString.length == 1) {
            secondsString = "0" + seconds;
        }

        return minutes + ":" + secondsString;
    }

    [Bindable(event="stateChanged")]
    public function get arrivalTime(): Number {
        if(_userState != null && _userState.userTransferState != null) {
            return _userState.userTransferState.arrivalTime;
        }

        return 0;
    }

    [Bindable(event="stateChanged")]
    public function get state(): String {
        if(_userState != null && _userState.userTransferState != null) {
            return TRANSITION;
        }

        return STATION_HALL;
    }

    public function set userState (value:UserState):void
    {
        _userState=value;
        trace("_userState = "  + _userState);
        if (_userState != null)
            trace("_userState = "  + _userState.userTransferState);
        
        dispatchEvent(new Event("stateChanged"));
    }

    [Bindable(event="stateChanged")]
    public function get userState():UserState
    {
        return _userState;
    }
}
}