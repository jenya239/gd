package com.turbostool.client.controllers
{
import com.turbostool.client.Config;
import com.turbostool.client.event.VkontakteEvent;
import com.turbostool.client.utils.MD5;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.IOErrorEvent;
import flash.events.SecurityErrorEvent;
import flash.net.URLLoader;
import flash.net.URLRequest;

import mx.utils.URLUtil;

public class VkontakteRequest extends EventDispatcher
{
    public static const RESULT_OK: String = "resultOk";
    public static const RESULT_ERROR: String = "resultError";
    private var _viewerID: String;
    private const APP_KEY: String = "XLOC22kp7Y";
    private const APP_ID: String = "468346";
    // GD Beta 2 test app info
    //    private const APP_KEY: String = "nTPg1BRTKT";
    //    private const APP_ID: String = "1509618";


    public function VkontakteRequest(viewerID: String = null)
    {
        if (viewerID == null)
        {
            _viewerID = Client.instance.flashParameters ["viewer_id"];
        }
        else
        {
            _viewerID = viewerID;
        }
    }

    public function sendIsAppUserRequest(): void
    {
        var params: Object = new Object();
        createCommonParams(params);
        params.method = "isAppUser";
        sendRequest(_viewerID, params);
    }

    public function sendPutVariable(key: String, value: String): void
    {
        var params: Object = new Object();
        createCommonParams(params);
        params.method = "putVariable";
        params.key = key;
        params.value = value;
        sendRequest(_viewerID, params);
    }

    public function sendGetVariable(key: String, userID: String = ""): void
    {
        var params: Object = new Object();
        createCommonParams(params);
        if (userID != "")
        {
            params.user_id = userID;
        }
        params.method = "getVariable";
        params.key = key;
        sendRequest(_viewerID, params);
    }

    public function sendGetAppFriends(): void
    {
        var params: Object = new Object();
        createCommonParams(params);
        params.method = "getAppFriends";
        sendRequest(_viewerID, params);
    }

    public function sendGetProfiles(idList: String): void
    {
        var params: Object = new Object();
        createCommonParams(params);
        params.method = "getProfiles";
        params.uids = idList;
        params.fields = "uid,first_name,last_name,photo_medium";
        sendRequest(_viewerID, params);
    }

    public function setNameInMenu(nameInMenu: String):void
    {
        var params: Object = new Object();
        createCommonParams(params);
        params.method = "setNameInMenu";
        params.name = nameInMenu;
        sendRequest(_viewerID, params);
    }

    public function sendRequest(viewerID: String, params: Object): void
    {
        var THIS: VkontakteRequest = this;
        var url: String = createSignedUrlStringFromParams(viewerID, params);
        var request: URLRequest = new URLRequest(url);        
        var loader: URLLoader = new URLLoader(request);

        try
        {
            loader.addEventListener(Event.COMPLETE, onResponse);
            loader.addEventListener(IOErrorEvent.IO_ERROR, onResponse);
            loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onResponse);
            loader.load(request);
        }
        catch (error: Error)
        {
            loader.removeEventListener(Event.COMPLETE, onResponse);
            loader.removeEventListener(IOErrorEvent.IO_ERROR, onResponse);
            loader.removeEventListener(SecurityErrorEvent.SECURITY_ERROR, onResponse);
            THIS.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, error.message, this));
        }

        function onResponse(event: Event):void
        {
            loader.removeEventListener(Event.COMPLETE, onResponse);
            loader.removeEventListener(IOErrorEvent.IO_ERROR, onResponse);
            loader.removeEventListener(SecurityErrorEvent.SECURITY_ERROR, onResponse);
            //trace(loader.data);
            if (event.type == Event.COMPLETE)
            {
                try
                {
                    var responseXML: XML = new XML(loader.data);
                    if (responseXML.localName() == "response")
                    {
                        THIS.dispatchEvent(new VkontakteEvent(VkontakteEvent.RESPONSE, "", THIS, responseXML));
                    }
                    else
                    {
                        THIS.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, responseXML.error_msg, THIS, responseXML));
                    }
                }
                catch (e: Error)
                {
                    THIS.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, "Ошибка ответа vkontakte.ru", THIS));
                }
            }
            else
            {
                THIS.dispatchEvent(new VkontakteEvent(VkontakteEvent.ERROR, "Ошибка запроса vkontakte.ru", THIS));
            }
        }
    }

    private function createSignedUrlStringFromParams(viewerID: String, params: Object): String
    {
        var md5String: String = viewerID;
        var sortedParamNames: Array = new Array();
        for (var paramName: String in params)
        {
            sortedParamNames.push(paramName);
        }
        sortedParamNames.sort();

        var sortedParamName: String;
        for each(sortedParamName in sortedParamNames)
        {
            md5String += sortedParamName + "=" + params[sortedParamName];
        }
        md5String += APP_KEY;
        var md5: String = MD5.encrypt(md5String);

        var url: String = URLUtil.objectToString(params, "&", true);

        url += "&sig=" + md5;
        url = Config.instance.vkontakteAPIUrl + "?" + url;
        return url;
    }

    private function createCommonParams(params: Object): void
    {
        params.api_id = APP_ID;
        params.viewer_id = _viewerID;
        params.v = "2.0";
        params.format = "XML";
        params.test_mode = "0";
    }
}
}