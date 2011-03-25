package com.turbostool.client.controllers
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.event.ConnectButtonClickEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.InvitationAcceptedMessage;
import com.turbostool.client.event.JoinRequestEvent;
import com.turbostool.client.event.StateChangedEvent;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.ConnectRaceRequest;
import com.turbostool.client.net.messages.ConnectRaceResponse;
import com.turbostool.client.net.messages.DisconnectRaceRequest;
import com.turbostool.client.net.messages.JoinRaceRequest;
import com.turbostool.client.net.messages.JoinRequestMessage;
import com.turbostool.client.net.messages.QuitLobbyRequest;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.utils.Utils;

import flash.events.Event;

import mx.controls.Alert;
import mx.logging.Log;

public class ConnectController extends BaseStateMachine
{
    // состояния
    public static const DISCONNECTED: String = "disconnected";
    public static const WAITING_RESPONSE: String = "waitingResponse";
    public static const CONNECTED: String = "connected";

    // ссылки
    private var _socket: SessionSocket;
    private var _racingSM: RacingStateMachine;
    private var _modelsStorage: ModelsStorage;

    //todo переделать ?
    // может не надо торчать переменными наружу? ведь их можно забить обнулиить или еще что.. И кто то,
    // кто хочет их прочитать должен иметь ссылку на даннйы контроллер.
    // по другому можно их как-то заворачивать в StateChangedEvent, и получатель их из евента достанет

    public function ConnectController(racingSM: RacingStateMachine, modelsStorage: ModelsStorage)
    {
        super();
        _racingSM = racingSM;
        _logger = Log.getLogger(Utils.getClassName(this));
        _socket = SessionSocket.instance;
        _modelsStorage = modelsStorage;

        EventManager.singleRaceChannel.addEventListener(ConnectRaceResponse.CONNECT_RACE, processEvent);
        EventManager.singleRaceChannel.addEventListener(ConnectButtonClickEvent.CONNECT_BUTTON_CLICK, processEvent);
        EventManager.singleRaceChannel.addEventListener(InvitationAcceptedMessage.INVITATION_ACCEPTED, processEvent);
        EventManager.singleRaceChannel.addEventListener(JoinRequestEvent.JOIN_REQUEST, processEvent);

        EventManager.instance.addEventListener(StateChangedEvent.STATE_CHANGED_EVENT, processEvent);

        _state = DISCONNECTED;
        onEnterNewState(_state);
    }

    protected override function processEvent(event: Event): void
    {
        super.processEvent(event);

        //trace("ConnectController.processEvent: " + event.type + ", " + event);

        switch (_state)
                {
            case DISCONNECTED:
                switch (event.type)
                        {
                    case ConnectButtonClickEvent.CONNECT_BUTTON_CLICK:
                        _socket.sendMessage(new QuitLobbyRequest());
                        sendConnectRaceRequest(ConnectButtonClickEvent(event));
                        changeState(WAITING_RESPONSE);
                        break;
                    case InvitationAcceptedMessage.INVITATION_ACCEPTED:
                        _socket.sendMessage(new JoinRaceRequest(InvitationAcceptedMessage(ServerResponseEvent(event).response).raceID));
                        changeState(WAITING_RESPONSE);
                        break;
                    case JoinRequestEvent.JOIN_REQUEST:
                        _socket.sendMessage(new JoinRequestMessage(JoinRequestEvent(ServerResponseEvent(event)).clientID));
                        changeState(WAITING_RESPONSE);
                        break;
                }
                break;

            case WAITING_RESPONSE:
                if (event.type == ConnectRaceResponse.CONNECT_RACE)
                {
                    if (ConnectRaceResponse(ServerResponseEvent(event).response).isOK)
                    {
                        changeState(CONNECTED);
                    }
                    else
                    {
                        _error = ConnectRaceResponse(ServerResponseEvent(event).response).error;
                        showAlert();
                        changeState(DISCONNECTED);
                    }
                }
                break;

            case CONNECTED:
                switch (event.type)
                        {
                    case StateChangedEvent.STATE_CHANGED_EVENT:
                        if (StateChangedEvent.check(event, Client.instance.controller.racingSM, RacingStateMachine.DISCONNECTED))
                        {
                            changeState(DISCONNECTED);
                        }
                        break;
                    case ConnectButtonClickEvent.CONNECT_BUTTON_CLICK:
                        sendDisconnect();
                        sendConnectRaceRequest(ConnectButtonClickEvent(event));
                        changeState(WAITING_RESPONSE);
                        break;
                    case InvitationAcceptedMessage.INVITATION_ACCEPTED:
                        sendDisconnect();
                        _socket.sendMessage(new JoinRaceRequest(InvitationAcceptedMessage(ServerResponseEvent(event)).raceID));
                        changeState(WAITING_RESPONSE);
                        break;
                    case JoinRequestEvent.JOIN_REQUEST:
                        sendDisconnect();
                        _socket.sendMessage(new JoinRequestMessage(JoinRequestEvent(ServerResponseEvent(event)).clientID));
                        changeState(WAITING_RESPONSE);
                        break;
                }
                break;
        }
    }

    private function sendConnectRaceRequest(event: ConnectButtonClickEvent): void
    {
        _racingSM.routeIDToLoad = ConnectButtonClickEvent(event).routeID;
        _racingSM.isReverse = ConnectButtonClickEvent(event).direction == "back";
        _racingSM.lapNumber = ConnectButtonClickEvent(event).lapNumber;
        _socket.sendMessage(new ConnectRaceRequest(ConnectButtonClickEvent(event).routeID, ConnectButtonClickEvent(event).direction, ConnectButtonClickEvent(event).lapNumber));
    }

    // действия

    private function showAlert(): void
    {
        Alert.show(_error.toString());
    }

    private function sendDisconnect(): void
    {
        _socket.sendMessage(new DisconnectRaceRequest());
    }
}
}