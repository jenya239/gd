package com.turbostool.client.game.route
{
import flash.display.DisplayObject;
import flash.display.Loader;
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.IOErrorEvent;
import flash.events.SecurityErrorEvent;
import flash.net.URLRequest;

public class SingleLoader extends EventDispatcher
{
    private var _loader: Loader = new Loader();
    private var _key: String;
    private var _isLoading: Boolean = false;

    public function SingleLoader()
    {
        _loader.contentLoaderInfo.addEventListener(Event.COMPLETE, onLoaded);
        _loader.contentLoaderInfo.addEventListener(IOErrorEvent.IO_ERROR, onError);
        _loader.contentLoaderInfo.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onError);
    }

    private function onLoaded(e: Event): void
    {
        _isLoading = false;
        dispatchEvent(new TextureLoadResult(true, this, _loader.contentLoaderInfo.bytesTotal));
    }

    private function onError(e: Event = null): void
    {
        _isLoading = false;
        dispatchEvent(new TextureLoadResult(false, this));
    }

    public function get key(): String
    {
        return _key;
    }

    public function get content(): DisplayObject
    {
        return _loader.content;
    }

    public function get isLoading(): Boolean
    {
        return _isLoading;
    }

    public function loadTexture(url: String, key: String = null): void
    {
        _key = key;
        try {
            _loader.load(new URLRequest(url));
            _isLoading = true;
            //trace("url = " + url);
            //trace("key = " + key);
        }
        catch (e: Error)
        {
            onError();
        }
    }
}
}