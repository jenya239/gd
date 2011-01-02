package com.turbostool.client.event
{
import com.turbostool.client.game.CarControlEventGenerator;
import com.turbostool.client.net.messages.ChanneledMessage;
import com.turbostool.client.net.messages.IChanneledMessage;
import com.turbostool.client.utils.Assert;
import com.turbostool.client.utils.collections.HashMap;

import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.IEventDispatcher;
import flash.events.KeyboardEvent;

import mx.core.Application;

public class EventManager implements IEventDispatcher
{
    private static var _instance: EventManager;

    private var _carControlsEnabled: Boolean = false;

    public static var globalChannel: EventDispatcher = new EventDispatcher();
    public static var singleRaceChannel: ChanneledEventDispatcher = new ChanneledEventDispatcher(ChanneledMessage.RACE_CHANNEL);
    public static var lobbyRaceChannel: ChanneledEventDispatcher = new ChanneledEventDispatcher(ChanneledMessage.LOBBY_CHANNEL);
    public static var cityChannel: ChanneledEventDispatcher = new ChanneledEventDispatcher(ChanneledMessage.CITY_CHANNEL);
    public static var tradeChannel: ChanneledEventDispatcher = new ChanneledEventDispatcher(ChanneledMessage.TRADE_CHANNEL);
    public static var privateChannel: ChanneledEventDispatcher = new ChanneledEventDispatcher(ChanneledMessage.PRIVATE_CHANNEL);
    public static var channels: HashMap;

    public function EventManager()
    {
        Assert.assertNotNull(_instance, "EventManager singleton was already constructed");
        channels = new HashMap();
        channels.setValue(singleRaceChannel.channel, singleRaceChannel);
        channels.setValue(lobbyRaceChannel.channel, lobbyRaceChannel);
        channels.setValue(cityChannel.channel, cityChannel);
        channels.setValue(tradeChannel.channel, tradeChannel);
        channels.setValue(privateChannel.channel, privateChannel);
    }

    public function subscribe(): void
    {
        Application.application.stage.addEventListener(KeyboardEvent.KEY_DOWN, onKey);
        Application.application.stage.addEventListener(KeyboardEvent.KEY_UP, onKey);
    }

    public function onKey(e: KeyboardEvent): void
    {
        if (_carControlsEnabled)
        {
            dispatchEvent(e);
        }
    }

    public static function get instance(): EventManager
    {
        if (_instance == null)
        {
            _instance = new EventManager();
        }

        return _instance;
    }

    public function get carControlsEnabled(): Boolean
    {
        return _carControlsEnabled;
    }

    public function set carControlsEnabled(value: Boolean): void
    {
        //trace("carControlsEnabled = " + value);
        _carControlsEnabled = value;
        if (!value)
        {
            CarControlEventGenerator.instance.clearKeyCodes();
        }
    }

    public function dispatchEvent(event: Event): Boolean
    {
        var dispatcher: EventDispatcher;
        if (event is IChanneledMessage)
        {
            dispatcher = EventDispatcher(channels.getValue(IChanneledMessage(event).channel));
            if (dispatcher == null)
            {
                dispatcher = globalChannel;
            }
        } else
        {
            dispatcher = globalChannel;
        }

        return dispatcher.dispatchEvent(event);
    }

    public function hasEventListener(type:String):Boolean
    {
        return globalChannel.hasEventListener(type);
    }

    public function willTrigger(type:String):Boolean
    {
        return globalChannel.willTrigger(type);
    }

    public function removeEventListener(type:String, listener:Function, useCapture:Boolean = false):void
    {
        return globalChannel.removeEventListener(type, listener, useCapture);
    }

    public function addEventListener(type:String, listener:Function, useCapture:Boolean = false, priority:int = 0, useWeakReference:Boolean = false):void
    {
        return globalChannel.addEventListener(type, listener, useCapture, priority, useWeakReference);
    }
}
}