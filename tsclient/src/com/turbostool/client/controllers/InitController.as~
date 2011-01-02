package com.turbostool.client.controllers
{
import com.turbostool.client.Config;
import com.turbostool.client.event.ConfigLoadErrorEvent;
import com.turbostool.client.event.ConfigLoadedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.event.ServerConnectErrorEvent;
import com.turbostool.client.event.VkontakteEvent;
import com.turbostool.client.net.ServerConnectedEvent;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.AuthorizeResponse;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.net.SharedObject;

import mx.controls.Alert;
import mx.logging.Log;

public class InitController extends BaseStateMachine
{
    // состояния
    public static const PROCESSING_VKONTAKTE: String = "processingVkontakte";
    public static const LOADING_CONFIG: String = "loadingConfig";
    public static const CONNECTING_SERVER: String = "connectingServer";
    public static const INITIALIZED: String = "initialized";
    public static const FAILER_TO_INITIALIZE: String = "failedToInitialize";

    public static const REASON_VKONTAKTE_ERROR: String = "reasonVkontakteError";
    public static const REASON_VKONTAKTE_ONLY: String = "reasonVkontakteOnly";
    public static const REASON_ADD_APPLICATION: String = "reasonAddApplication";
    public static const REASON_VKONTAKTE_AUTH: String = "reasonVkontakteAuth";

    // константы
    private static const CONFIG_PATH: String = "config.xml";

    // ссылки
    private var _reason: String;
    private var _socket: SessionSocket;
    private var _config: Config;
    private var _eventManager: EventManager;
    private var _vkontakteRequest: VkontakteRequest = new VkontakteRequest(Client.instance.flashParameters["viewer_id"]);
    public var randomAuthKey: String;
    public var vkontakteUserName: String;


    public function InitController()
    {
        super();

        _logger = Log.getLogger(Utils.getClassName(this));
        _config = Config.instance;
        _socket = SessionSocket.instance;
        _eventManager = EventManager.instance;

        _eventManager.addEventListener(VkontakteEvent.PROCESSED, processEvent);
        _eventManager.addEventListener(VkontakteEvent.ERROR, processEvent);
        _eventManager.addEventListener(VkontakteEvent.AUTHORIZED, processEvent);
        _eventManager.addEventListener(AuthorizeResponse.AUTHORIZE, processEvent);
        _eventManager.addEventListener(ConfigLoadedEvent.CONFIG_LOADED, processEvent);
        _eventManager.addEventListener(ConfigLoadErrorEvent.CONFIG_LOAD_ERROR, processEvent);
        _eventManager.addEventListener(ServerConnectedEvent.SERVER_CONNECTED, processEvent);
        _eventManager.addEventListener(ServerConnectErrorEvent.SERVER_CONNECT_ERROR, processEvent);

        _state = LOADING_CONFIG;
        onEnterNewState(_state);
    }

    public static function get vkontakteOwnerID(): String {
        return SharedObject.getLocal("vkontakteOwnerID").data.value as String;
    }

    public static function saveVkontakteOwnerID(): void {
        if (SharedObject.getLocal("vkontakteOwnerID").data.value == null) {
            var userID: String = Client.instance.flashParameters["user_id"];
            if (userID == null || userID == "") {
                userID = "0";
            }
            SharedObject.getLocal("vkontakteOwnerID").data.value = userID;
        }

    }

    // обработка события для данного автомата
    protected override function processEvent(event: Event): void
    {
        super.processEvent(event);

        switch (_state)
                {

            case LOADING_CONFIG:
                switch (event.type)
                        {
                    case ConfigLoadedEvent.CONFIG_LOADED:
                        setLogLevel();
                        _logger.debug("CONFIG_LOADED");
                        changeState(PROCESSING_VKONTAKTE);
                        break;

                    case ConfigLoadErrorEvent.CONFIG_LOAD_ERROR:
                        _logger.debug("Не удалось загрузить конфиг: ");
                        _error = new TSError(str("cantLoadConfig") + ": " + ConfigLoadErrorEvent(event).error.message);
                        changeState(FAILER_TO_INITIALIZE);
                        break;
                }
                break;

            case PROCESSING_VKONTAKTE:
                switch (event.type)
                        {
                    case VkontakteEvent.PROCESSED:
                        _logger.debug("VkontakteEvent.PROCESSED");
                        changeState(CONNECTING_SERVER);
                        break;
                    case VkontakteEvent.ERROR:
                        _logger.debug("VkontakteEvent.ERROR");
                        _error = new Error((event as VkontakteEvent).message);
                        changeState(FAILER_TO_INITIALIZE);
                        break;
                }

            case CONNECTING_SERVER:
                switch (event.type)
                        {
                    case ServerConnectedEvent.SERVER_CONNECTED:
                        changeState(INITIALIZED);
                        break;

                    case ServerConnectErrorEvent.SERVER_CONNECT_ERROR:
                    {
                        var errorType: String = ServerConnectErrorEvent(event).innerEvent.type;
                        
                        _error = new TSError(str("serverMaintenanceWorks"));

                        changeState(FAILER_TO_INITIALIZE);
                        break;
                    }
                }
                break;
        }
    }

    // действия, вызываемые при входе в состояние
    protected override function onEnterNewState(newState: String): void
    {
        _logger.debug("init controller onEnterNewState state = " + newState);
        switch (newState)
                {

            case LOADING_CONFIG:
                startLoadConfig();
                break;

            case PROCESSING_VKONTAKTE:
                startProcessingVkontakte();
                break;

            case CONNECTING_SERVER:
                startConnectToServer();
                break;

            case INITIALIZED:
                dispatchEvent(new Event(Event.COMPLETE));
                break;

            case FAILER_TO_INITIALIZE:
                showAlert();
                break;
        }
    }

    public static function getRandomSuffix(): String
    {
        return "?" + Math.round(Math.random() * 10000000000);
    }

    // действия
    private function startLoadConfig(): void
    {
        _config.loadConfig(CONFIG_PATH + InitController.getRandomSuffix());
    }

    private function startConnectToServer(): void
    {
        _logger.debug("init controller startConnectToServer");
        _socket.connect2(_config.host, _config.port, _config.policyPort);
    }

    private function showAlert(): void
    {
        if (_error != null && _error.message != "")
        {
            Alert.show(_error.toString());
        }
    }

    private function setLogLevel(): void
    {
        Client.instance.logLevel = _config.logLevel;
    }

    public function get reason():String
    {
        return _reason;
    }

    private function startProcessingVkontakte(): void
    {
        saveVkontakteOwnerID();
        if (Client.instance.flashParameters["viewer_id"] == null || Client.instance.flashParameters["viewer_id"] == "")
        {
            _reason = REASON_VKONTAKTE_ONLY;
            _error = new Error("Application must be running at vkontakte.ru");
            changeState(FAILER_TO_INITIALIZE);
            return;
        }

        var isAppUser: Object = Client.instance.flashParameters["is_app_user"];
        if (!Client.instance.isAPIWrapper && isAppUser != "1") {
            _reason = REASON_ADD_APPLICATION;
            _eventManager.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, ""));
            return;
        }

        //_vkontakteRequest.addEventListener(VkontakteEvent.RESPONSE, onIsAppUser);
        //_vkontakteRequest.addEventListener(VkontakteEvent.ERROR, onIsAppUser);
        //_vkontakteRequest.sendIsAppUserRequest();
        _vkontakteRequest.addEventListener(VkontakteEvent.RESPONSE, onGetVkontakteUserName);
        _vkontakteRequest.addEventListener(VkontakteEvent.ERROR, onGetVkontakteUserName);
        _vkontakteRequest.sendGetVariable("1281");

    }

    //    private function onIsAppUser(event: VkontakteEvent): void
    //    {
    //        event.request.removeEventListener(VkontakteEvent.RESPONSE, onIsAppUser);
    //        event.request.removeEventListener(VkontakteEvent.ERROR, onIsAppUser);
    //        if (event.type == VkontakteEvent.RESPONSE)
    //        {
    //            if (event.xml == "1")
    //            {
    //                _vkontakteRequest.addEventListener(VkontakteEvent.RESPONSE, onGetVkontakteUserName);
    //                _vkontakteRequest.addEventListener(VkontakteEvent.ERROR, onGetVkontakteUserName);
    //                _vkontakteRequest.sendGetVariable("1281");
    //            }
    //            else
    //            {
    //                _reason = REASON_ADD_APPLICATION;
    //                _eventManager.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, ""));
    //            }
    //        }
    //        else
    //        {
    //            _reason = REASON_VKONTAKTE_ERROR;
    //            _eventManager.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, event.message));
    //        }
    //    }

    private function onGetVkontakteUserName(event: VkontakteEvent): void
    {
        event.request.removeEventListener(VkontakteEvent.RESPONSE, onGetVkontakteUserName);
        event.request.removeEventListener(VkontakteEvent.ERROR, onGetVkontakteUserName);
        if (event.type == VkontakteEvent.RESPONSE)
        {
            vkontakteUserName = event.xml.toString();
            _eventManager.dispatchEvent(new VkontakteEvent(VkontakteEvent.PROCESSED));
        }
        else
        {
            _reason = REASON_VKONTAKTE_ERROR;
            _eventManager.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, event.message));
        }
    }

}
}