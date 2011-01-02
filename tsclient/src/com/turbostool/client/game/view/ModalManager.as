package com.turbostool.client.game.view
{
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.MouseEvent;

import mx.collections.ArrayCollection;
import mx.core.Application;
import mx.core.UIComponent;
import mx.events.EffectEvent;
import mx.managers.PopUpManager;

public class ModalManager extends EventDispatcher
{
    private var _modalWindows: ArrayCollection = new ArrayCollection();
    private var _needCenter: ArrayCollection = new ArrayCollection();
    private var _activeWindow: UIComponent;
    private static var _instance: ModalManager;
    private var _closeEventType: String;

    public function get activeWindow():UIComponent
    {
        return _activeWindow;
    }

    public function ModalManager()
    {

    }

    public static function get instance(): ModalManager
    {
        if (_instance == null)
        {
            _instance = new ModalManager();
        }
        return _instance;
    }

    public function addModalWindow(window: UIComponent, closeEventType: String = null, center: Boolean = true): void
    {
        _closeEventType = closeEventType;
        _modalWindows.addItem(window);
        _needCenter.addItem(center);

        if (closeEventType != null)
            window.addEventListener(closeEventType, onCloseEvent);
        else
            window.addEventListener(MouseEvent.CLICK, onCloseEvent);

        if (_activeWindow == null)
        {
            showNextWindow();
        }
    }

    private function showNextWindow():void
    {
        if (_modalWindows.length > 0)
        {
            var nextWindow: UIComponent = UIComponent(_modalWindows.removeItemAt(0));
            var needCenter: Boolean = Boolean(_needCenter.removeItemAt(0));
            
            PopUpManager.addPopUp(nextWindow, Client.instance, true);
            PopUpManager.bringToFront(nextWindow);

            if(needCenter)
                PopUpManager.centerPopUp(nextWindow);
            
            _activeWindow = nextWindow;
            _activeWindow.setFocus();
        } else
        {
            _activeWindow = null;
            dispatchEvent(new ModalManagerEvent(ModalManagerEvent.FINISHED));
        }
    }

    private function onCloseEvent(event: Event):void
    {
        closeWindow();
    }

    private function closeWindow():void
    {
        _activeWindow.removeEventListener(MouseEvent.CLICK, onCloseEvent);
        if (_closeEventType != null)
        {
            _activeWindow.removeEventListener(_closeEventType, onCloseEvent);
        }
        Application.application.addEventListener(EffectEvent.EFFECT_END, onAfterUnblur);
        PopUpManager.removePopUp(_activeWindow);
    }

    private function onAfterUnblur(event: EffectEvent): void
    {
        Application.application.removeEventListener(EffectEvent.EFFECT_END, onAfterUnblur);
        showNextWindow();
    }


    public function closeAllWindows():void
    {
        _modalWindows.removeAll();

        if (_activeWindow != null) {
            closeWindow();
        }
    }
}
}