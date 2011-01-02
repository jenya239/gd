package com.turbostool.client.event
{
import com.turbostool.client.controllers.VkontakteRequest;

import flash.events.Event;

public class VkontakteEvent extends Event
{
    public static const ERROR: String = "error";
    public static const RESPONSE: String = "response";
    public static const PROCESSED: String = "processed";
    public static const AUTHORIZED: String = "authorized";

    private var _message: String;
    private var _xml: XML;
    private var _request: VkontakteRequest;

    public function VkontakteEvent(type: String, message: String = "", request: VkontakteRequest = null, xml: XML = null)
    {
        super(type);
        _message = message;
        _request = request;
        _xml = xml;
    }

    public function get message():String
    {
        return _message;
    }

    public function get request():VkontakteRequest
    {
        return _request;
    }

    public function get xml():XML
    {
        return _xml;
    }
}
}