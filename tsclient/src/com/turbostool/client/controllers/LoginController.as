package com.turbostool.client.controllers
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.event.AuthorizationChangedEvent;
import com.turbostool.client.event.BackToLoginButtonClick;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.LoginButtonClickEvent;
import com.turbostool.client.event.LogoutClickedEvent;
import com.turbostool.client.event.StateChangedEvent;
import com.turbostool.client.model.UserInfo;
import com.turbostool.client.model.UserState;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.AuthorizeResponse;
import com.turbostool.client.net.messages.AuthorizeVkontakteRequest;
import com.turbostool.client.net.messages.EndTransferRequest;
import com.turbostool.client.net.messages.EndTransferResponse;
import com.turbostool.client.net.messages.GetPropertyRequest;
import com.turbostool.client.net.messages.GetPropertyResponse;
import com.turbostool.client.net.messages.GetUserStateResponse;
import com.turbostool.client.net.messages.RegisterResponse;
import com.turbostool.client.net.messages.ServerResponseEvent;
import com.turbostool.client.registration.RegistrationController;
import com.turbostool.client.utils.Utils;

import flash.events.Event;

import mx.controls.Alert;
import mx.logging.Log;

public class LoginController extends BaseStateMachine
{
    // состояния
    public static const LOGGED_OUT: String = "loggedOut";
    public static const WAITING_RESPONSE: String = "waitingResponse";
    public static const LOGGED_IN: String = "loggedIn";
    public static const REGISTRATION: String = "registration";
    public static const LOADING_DATA: String = "loadingData";
    public static const LOADING_REG_DATA: String = "loadingRegData";
    public static const WAITING_REG_RESPONSE: String = "waitingRegResponse";
    public static const WAITING_VK_ADD_APPLICATION: String = "waitingVKAddApplication";

    // ссылки
    private var _socket: SessionSocket;
    private var _modelsStorage: ModelsStorage;
    public var registrationController: RegistrationController;

    public function LoginController(modelsStorage: ModelsStorage)
    {
        super();
        _logger = Log.getLogger(Utils.getClassName(this));
        _socket = SessionSocket.instance;
        _modelsStorage = modelsStorage;

        EventManager.instance.addEventListener(AuthorizeResponse.AUTHORIZE, processEvent);
        EventManager.instance.addEventListener(RegisterResponse.REGISTER, processEvent);
        EventManager.instance.addEventListener(LoginButtonClickEvent.LOGIN_BUTTON_CLICK, processEvent);
        EventManager.instance.addEventListener(BackToLoginButtonClick.BACK_TO_LOGIN_SCREEN, processEvent);
        EventManager.instance.addEventListener(LogoutClickedEvent.LOGOUT_CLICKED, processEvent);
        EventManager.instance.addEventListener(StateChangedEvent.STATE_CHANGED_EVENT, processEvent);
        EventManager.instance.addEventListener("dataLoaded", processEvent);
        EventManager.instance.addEventListener("regDataLoaded", processEvent);
        EventManager.instance.addEventListener("onApplicationAdded", processEvent);

        _state = LOGGED_OUT;
        onEnterNewState(_state);
    }

    protected override function processEvent(event: Event): void
    {
        super.processEvent(event);

        switch (_state)
                {
            case LOGGED_OUT:
                if (StateChangedEvent.check(event, Client.instance.controller.initController, InitController.INITIALIZED))
                {
                    // Launset user for tests
                    //var authKey: String = "d8ca6cc823fa608281805dcaf7c2d9f9";
                    //var viewerID: String = "45895";

                    var authKey: String = Client.instance.flashParameters["auth_key"];
                    var viewerID: String = Client.instance.flashParameters["viewer_id"];

                    SessionSocket.instance.sendMessage(new AuthorizeVkontakteRequest(viewerID, authKey, Client.instance.controller.initController.vkontakteUserName, InitController.vkontakteOwnerID));
                    changeState(WAITING_RESPONSE);
                    //var ar: AuthorizeResponse = new AuthorizeResponse();
                    //ar.result = "ok";
                    //EventManager.instance.dispatchEvent(new ServerResponseEvent(AuthorizeResponse.AUTHORIZE, ar));
                }
                break;

            case WAITING_RESPONSE:
                if (event.type == AuthorizeResponse.AUTHORIZE)
                {
                    if (AuthorizeResponse(ServerResponseEvent(event).response).isOK)
                    {
                        var userInfo: UserInfo = AuthorizeResponse(ServerResponseEvent(event).response).userInfo;
                        _modelsStorage.userInfo = userInfo;
                        if (_modelsStorage.userInfo.hasTrigger("register"))
                        {
                            changeState(LOADING_REG_DATA);
                        }
                        else
                        {
                            if (Client.instance.isAppUser == "1")
                            {
                                changeState(LOADING_DATA);
                            }
                            else
                            {
                                changeState(WAITING_VK_ADD_APPLICATION);
                            }
                        }
                    }
                    else
                    {
                        _error = AuthorizeResponse(ServerResponseEvent(event).response).error;
                        showAlert();
                        changeState(LOGGED_OUT);
                    }
                }
                break;

            case WAITING_VK_ADD_APPLICATION:
                if (event.type == "onApplicationAdded")
                {
                    changeState(LOADING_DATA);
                }
                break;

            case LOADING_REG_DATA:
                if (event.type == "regDataLoaded")
                {
                    registrationController.nickname = _modelsStorage.userInfo.displayName;
                    registrationController.defaultNickname = _modelsStorage.userInfo.displayName;
                    registrationController.carClassId = _modelsStorage.userInfo.currentCarID;
                    registrationController.colorIndex = _modelsStorage.userInfo.color;
                    registrationController.city = _modelsStorage.userInfo.homeCity;
                    changeState(REGISTRATION);
                }
                break;

            case REGISTRATION:
                if (event.type == RegisterResponse.REGISTER)
                {
                    if (RegisterResponse(ServerResponseEvent(event).response).isOK)
                    {
                        changeState(LOADING_DATA);
                    }
                }
                break;

            case LOADING_DATA:
                if (event.type == "dataLoaded")
                {
                    changeState(LOGGED_IN);
                }
                break;
        }
    }

    protected override function onEnterNewState(state: String): void
    {
        if (state == LOGGED_OUT)
        {
            EventManager.globalChannel.dispatchEvent(new AuthorizationChangedEvent(false));
        }
        else
            if (state == LOADING_REG_DATA)
            {
                EventManager.instance.addEventListener(GetPropertyResponse.GET_CARS_RESPONSE, onRegCars);
                SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.CARS));
            }
            else
                if (state == WAITING_VK_ADD_APPLICATION)
                {
                    Client.instance.wrapper.external.showInstallBox();
                }
                else
                    if (state == LOADING_DATA)
                    {
                        EventManager.instance.addEventListener(GetPropertyResponse.GET_RATINGS_RESPONSE, onRatings);
                        SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.RATINGS));
                        var vkontakteRequest: VkontakteRequest = new VkontakteRequest(Client.instance.flashParameters["viewer_id"]);
                        vkontakteRequest.setNameInMenu("Город Дорог");
                        if( Client.instance.isAPIWrapper ){
			                //Client.instance.wrapper.external.showInviteBox();
                        }
                    }
                    else
                        if (state == LOGGED_IN)
                        {
                            EventManager.globalChannel.dispatchEvent(new AuthorizationChangedEvent(true));
                        }
    }

    private function onRatings(event: ServerResponseEvent): void
    {
        EventManager.instance.removeEventListener(GetPropertyResponse.GET_RATINGS_RESPONSE, onRatings);
        EventManager.instance.addEventListener(GetPropertyResponse.GET_ROUTES_RESPONSE, onRoutes);
        SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.ROUTES));
    }

    private function onRoutes(event: ServerResponseEvent): void
    {
        EventManager.instance.removeEventListener(GetPropertyResponse.GET_ROUTES_RESPONSE, onRoutes);
        EventManager.instance.addEventListener(GetPropertyResponse.GET_GLOBALINFO_RESPONSE, onGlobalInfo);
        SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.GLOBAL_INFO));
    }

    private function onGlobalInfo(event: ServerResponseEvent): void
    {
        EventManager.instance.removeEventListener(GetPropertyResponse.GET_GLOBALINFO_RESPONSE, onGlobalInfo);
        EventManager.instance.addEventListener(GetPropertyResponse.GET_CARS_RESPONSE, onCars);
        SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.CARS));
    }

    private function onCars(event: ServerResponseEvent): void
    {
        EventManager.instance.removeEventListener(GetPropertyResponse.GET_CARS_RESPONSE, onCars);
        EventManager.instance.addEventListener(GetUserStateResponse.GET_USERSTATE_RESPONSE, onUserStateResponse);
        SessionSocket.instance.sendMessage(new GetPropertyRequest(GetPropertyRequest.USER_STATE));
    }

    private function onUserStateResponse(event: ServerResponseEvent): void {
        EventManager.instance.removeEventListener(GetUserStateResponse.GET_USERSTATE_RESPONSE, onUserStateResponse);
        EventManager.instance.dispatchEvent(new Event("dataLoaded"));    
    }

    private function onRegCars(event: ServerResponseEvent): void
    {
        EventManager.instance.removeEventListener(GetPropertyResponse.GET_CARS_RESPONSE, onRegCars);
        EventManager.instance.dispatchEvent(new Event("regDataLoaded"));
    }

    private function showAlert(): void
    {
        Alert.show(_error.toString());
    }
}
}
