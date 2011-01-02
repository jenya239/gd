package com.turbostool.client
{
import com.turbostool.client.event.ConfigLoadErrorEvent;
import com.turbostool.client.event.ConfigLoadedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.ReplayData;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.IOErrorEvent;
import flash.events.SecurityErrorEvent;
import flash.net.URLLoader;
import flash.net.URLRequest;
import flash.system.Security;

import mx.collections.ArrayCollection;

public class Config extends EventDispatcher
{
    private static var _instance: Config = null;

    public static function get instance(): Config
    {
        if (_instance == null)
        {
            _instance = new Config();
        }
        return _instance;
    }

    private static var _initialized: Boolean = false;

    private var _host: String;
    private var _port: int;
    private var _policyPort: int;
    private var _serverUrlMain: String;
    private var _serverUrlCollection: ArrayCollection = new ArrayCollection();
    private var _serverUrl: String;
    private var _radio: String;
    private var _defaultRoutePath: String;
    private var _defaultCarPath: String;
    private var _defaultReplayString: String = "";
    private var _replayMode: String = ReplayData.MODE_OFF;
    private var _replayAutoPlayToIndex: int = -1;
    private var _drawDebugData: Boolean;
    private var _loadFromPHPServer:Boolean;
    private var _logLevel: int;
    private var _dynamicEngineQuantLocal: Number;
    private var _dynamicEngineQuantRemote: Number;
    private var _vkontakteAPIUrl: String;
    private var _carStatePosError: Number;
    private var _carStateAngleError: Number;
    private var _carStateIdleInterval: Number;
    private var _carStateMinInterval: Number;
    private var _filterCoeff: Number;

    private function assertInitialized(): void
    {
        if (!_initialized)
        {
            throw TSError("Config not loaded yet");
        }
    }

    public function get defaultCarPath(): String
    {
        assertInitialized();
        return _defaultCarPath;
    }

    public function get defaultRoutePath(): String
    {
        assertInitialized();
        return _defaultRoutePath;
    }

    public function get host(): String
    {
        assertInitialized();
        return _host;
    }
    
    public function get radio(): String
    {
        assertInitialized();
        return _radio;
    }

    public function get port(): int
    {
        assertInitialized();
        return _port;
    }

    public function get policyPort(): int
    {
        assertInitialized();
        return _policyPort;
    }

    public function get serverUrl(): String
    {
        return _serverUrl;
    }

    private function getRandomServerUrl(): String
    {
        var url: String;
        if (_serverUrlCollection.length > 0)
        {
            url = String(_serverUrlCollection.getItemAt(int(Math.random()*_serverUrlCollection.length)));
        } else
        {
            url = "";
        }
        return url;
    }

    public function get defaultReplayString(): String
    {
        assertInitialized();
        return _defaultReplayString;
    }

    public function get replayMode(): String
    {
        assertInitialized();
        return _replayMode;
    }

    public function get replayEnabled(): Boolean
    {
        assertInitialized();
        return _replayMode == ReplayData.MODE_ON || _replayMode == ReplayData.MODE_DEBUG;
    }

    public function get replayDebugEnabled(): Boolean
    {
        assertInitialized();
        return _replayMode == ReplayData.MODE_DEBUG;
    }

    public function get replayAutoPlayToIndex(): int
    {
        assertInitialized();
        return _replayAutoPlayToIndex;
    }

    public function get loadFromPHPServer(): Boolean
    {
        assertInitialized();
        return _loadFromPHPServer;
    }

    public function get drawDebugData(): Boolean
    {
        assertInitialized();
        return _drawDebugData;
    }

    public function get logLevel(): int
    {
        assertInitialized();
        return _logLevel;
    }

    public function get dynamicEngineQuantLocal(): Number
    {
        return _dynamicEngineQuantLocal;
    }

    public function get dynamicEngineQuantRemote(): Number
    {
        return _dynamicEngineQuantRemote;
    }

    public function get vkontakteAPIUrl(): String
    {
        return _vkontakteAPIUrl != "" ? _vkontakteAPIUrl : Client.instance.flashParameters["api_url"];
    }

    public function get carStatePosError():Number {
        return _carStatePosError;
    }

    public function get carStateAngleError():Number {
        return _carStateAngleError;
    }

    public function get carStateIdleInterval():Number
    {
        return _carStateIdleInterval;
    }

    public function get carStateMinInterval():Number
    {
        return _carStateMinInterval;
    }

    public function get filterCoeff():Number
    {
        return _filterCoeff;
    }

    private function parseXML(xml: XML): Config
    {
        _host = xml.host;
        _port = xml.port;
        _policyPort = xml.policyPort;
        _serverUrlCollection.removeAll();
        _serverUrlMain = xml.serverUrl;
        for each(var serverUrlXML: XML in xml.serverUrlList.url)
        {
            _serverUrlCollection.addItem(serverUrlXML.toString());
        }
        _serverUrl = getRandomServerUrl();
        _defaultRoutePath = xml.defaultRoutePath;
        _defaultCarPath = xml.defaultCarPath;
        _defaultReplayString = xml.defaultReplay;
        _replayMode = xml.replayMode;
        _replayAutoPlayToIndex = xml.replayAutoPlayToIndex;
        _drawDebugData = Utils.str2bool(xml.drawDebugData)
        _logLevel = xml.logLevel;
        _loadFromPHPServer = Utils.str2bool(xml.loadFromPHPServer);
        _dynamicEngineQuantLocal = xml.dynamicEngineQuantLocal;
        _dynamicEngineQuantRemote = xml.dynamicEngineQuantRemote;
        _vkontakteAPIUrl = xml.vkontakteAPIUrl;
        _carStateIdleInterval = xml.carStateIdleInterval;
        _carStateMinInterval = xml.carStateMinInterval;
        _carStatePosError = xml.carStatePosError;
        _carStateAngleError = xml.carStateAngleError;
        _filterCoeff = xml.filterCoeff;
	_radio = xml.radio;
        return this;
    }

    public function loadConfig(path: String): void
    {
        var request: URLRequest = new URLRequest(path);
        var loader: URLLoader = new URLLoader();
        loader.addEventListener(Event.COMPLETE, onLoad);
        loader.addEventListener(IOErrorEvent.IO_ERROR, onFirstError);
        loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onFirstError);
        loader.load(request);

        function onLoad(e: Event): void
        {
            try
            {
                Config.instance.parseXML(new XML(loader.data));
                Security.loadPolicyFile(serverUrlMain + "crossdomain.xml");
                for each(var url: String in _serverUrlCollection)
                {
                    Security.loadPolicyFile(url + "crossdomain.xml");
                }
                Security.loadPolicyFile("http://" + _host + "/crossdomain.xml");
                _initialized = true;

                EventManager.instance.dispatchEvent(new ConfigLoadedEvent(Config.instance));
                dispatchEvent(new ConfigLoadedEvent(Config.instance));
            }
            catch (e: Error)
            {
                EventManager.instance.dispatchEvent(new ConfigLoadErrorEvent(e));
                dispatchEvent(new ConfigLoadErrorEvent(e));
            }
        }

        function onError(e: Event): void
        {
            EventManager.instance.dispatchEvent(new ConfigLoadErrorEvent(new TSError(e.toString())));
            dispatchEvent(new ConfigLoadErrorEvent(new TSError(e.toString())));
        }

        function onFirstError(e: Event): void
        {
            loader.removeEventListener(IOErrorEvent.IO_ERROR, onFirstError);
            loader.removeEventListener(SecurityErrorEvent.SECURITY_ERROR, onFirstError);
            loader.addEventListener(IOErrorEvent.IO_ERROR, onError);
            loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onError);
            Security.loadPolicyFile("http://web.goroddorog.ru/crossdomain.xml");
            loader.load(new URLRequest("http://web.goroddorog.ru/" + path));
        }
    }

    public static function get initialized(): Boolean {
        return _initialized;
    }


    public function get serverUrlMain():String
    {
        return _serverUrlMain;
    }

    public function set serverUrlMain(value:String):void
    {
        _serverUrlMain = value;
    }
}
}