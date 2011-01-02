package com.turbostool.client.game.components.car {
import com.turbostool.client.event.CarLoadErrorEvent;
import com.turbostool.client.event.CarLoadedEvent;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.collections.HashMap;

import mx.controls.Alert;

import flash.net.*;
import flash.events.*;
import flash.utils.*;
import	flash.display.*;

public class CarLoader
{
    private static var _instance: CarLoader = null;
    //public static const DEFAULT_CAR_ID: String = 1;

    //    private var _loadedCars: HashMap = new HashMap();
    private var _requests: Array = new Array();
    //  private var _carNamesToLoad: Array = new Array();

    private var _xmlLoader:URLLoader = new URLLoader();
    private var _textureLoader: Loader = new Loader();

    // private var _currentName: String = "";

    private var _currentRequest:CarLoadRequest;
    private var _loadedXML: XML = null;
    private var _loadedTexture: BitmapData = null;

    private var _cacherXML: HashMap = new HashMap();
    private var _cacherTexture: HashMap = new HashMap();

    public function loadCar(carName: String, carFileName:String, clientID: int): void
    {
        _requests.push(new CarLoadRequest(carName, carFileName, clientID));
        tryStartLoadNext();
    }

    public static function get instance(): CarLoader
    {
        if (_instance == null)
        {
            _instance = new CarLoader;
        }
        return _instance;
    }

    public function CarLoader()
    {
        if (_instance != null)
        {
            throw new TSError("Вызов конструктора синглетона CarLoader");
        }
        _textureLoader.contentLoaderInfo.addEventListener(Event.COMPLETE, onTextureLoaded);
        _textureLoader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onTextureLoadError);
        _xmlLoader.addEventListener(Event.COMPLETE, onXMLLoaded);
        _xmlLoader.addEventListener(IOErrorEvent.IO_ERROR, onXMLLoaded);
    }

    private function onXMLLoaded(event: Event): void
    {
        _loadedXML = new XML(_xmlLoader.data);
        _cacherXML.setValue(_currentRequest.carXMLPath, _loadedXML);
        checkAllLoaded();
    }

    private function onTextureLoaded(event: Event): void
    {
        _loadedTexture = Bitmap(_textureLoader.content).bitmapData;
        _cacherTexture.setValue(_currentRequest.texturePath, _loadedTexture);
        checkAllLoaded();
    }

    private function onCarLoadError(e: IOErrorEvent): void
    {
        onCarOrTextureLoadError(e, "машины");

    }

    private function onTextureLoadError(e: IOErrorEvent): void
    {
        onCarOrTextureLoadError(e, "текстуры машины");
    }

    private function onCarOrTextureLoadError(e: IOErrorEvent, suffix:String): void {
        Alert.show(e.text);
        EventManager.instance.dispatchEvent(new CarLoadErrorEvent(_currentRequest.carName,
                new TSError("Ошибка загрузки " + suffix + ": " + e.text)));
        _currentRequest = null;
        tryStartLoadNext();
    }

    private function tryStartLoadNext(): void
    {
        if ((_currentRequest == null) && (_requests.length > 0))
        {
            _loadedXML = null;
            _loadedTexture = null;
            _currentRequest = _requests.shift();
            startLoadXML(_currentRequest.carXMLPath);
            startLoadTexture(_currentRequest.texturePath);
            checkAllLoaded();
        }
    }

    private function startLoadXML(carFileName:String):void
    {
        if (_cacherXML.containsKey(carFileName))
        {
            _loadedXML = _cacherXML.getValue(carFileName) as XML;
        } else {
            _xmlLoader.load(new URLRequest(carFileName));
        }
    }

    private function startLoadTexture(textureName:String):void
    {
        if (_cacherTexture.containsKey(textureName))
        {
            _loadedTexture = _cacherTexture.getValue(textureName) as BitmapData;
        } else {
            _textureLoader.load(new URLRequest(textureName));
        }
    }

    private function checkAllLoaded(): void
    {
        if ((_loadedTexture != null) && (_loadedXML != null))
        {
            var car: Car = new Car(_currentRequest.clientID, _loadedTexture.clone());
            car.setName(_currentRequest.carName);
            car.getEditableCarParameters().load(_loadedXML.toString(), true, true);
            try {
                _textureLoader.close();
            } catch(e:*) {
            }
            try {
                _xmlLoader.close();
            } catch(e:*) {
            }
            EventManager.instance.dispatchEvent(new CarLoadedEvent(_currentRequest.carName, _currentRequest.clientID, car));
            _currentRequest = null;
            _loadedTexture = null;
            _loadedXML = null;
            tryStartLoadNext();
        }
    }
}
}