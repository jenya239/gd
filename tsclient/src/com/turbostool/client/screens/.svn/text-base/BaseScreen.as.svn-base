package com.turbostool.client.screens
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.city.ScreenSelectedCommand;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.ServerRequest;
import com.turbostool.controls.LoadingIndicator;

import mx.core.*;
import mx.events.FlexEvent;
import mx.logging.ILogger;
import mx.managers.PopUpManager;
import mx.states.State;

public class BaseScreen extends GlowContainer
{
    // логгер
    protected var _logger: ILogger;

    protected var _modelsStorage: ModelsStorage;

    protected var loadingIndicator:IFlexDisplayObject = new LoadingIndicator();
    // LoadingIndicator

    [Bindable]
    public function get modelsStorage(): ModelsStorage
    {
        return _modelsStorage;
    }

    public function set modelsStorage(value:ModelsStorage):void
    {
        _modelsStorage = value;
    }

    public function BaseScreen()
    {
        //      _logger = Log.getLogger(className);
        addEventListener(FlexEvent.INITIALIZE, onInitialize);
        super();
    }

    protected function typicalSendMessage(responseEventType:String, handler:Function, message:ServerRequest):void
    {
        EventManager.instance.addEventListener(responseEventType, handler);
        SessionSocket.instance.sendMessage(message);
        PopUpManager.addPopUp(loadingIndicator, this, true);
        PopUpManager.centerPopUp(loadingIndicator);
        PopUpManager.bringToFront(loadingIndicator);
    }

    protected function showLoadingIndicator(): void {
        PopUpManager.addPopUp(loadingIndicator, this, true);
        PopUpManager.centerPopUp(loadingIndicator);
        PopUpManager.bringToFront(loadingIndicator);
    }

    protected function hideLoadingIndicator(): void {
        PopUpManager.removePopUp(loadingIndicator);
    }

    // контролы созданы
    public function onInitialize(event: FlexEvent): void
    {

    }

    public override function setFocus(): void
    {
        super.setFocus();
    }

    protected function onBackClick(): void
    {
        dispatchEvent(new ScreenSelectedCommand("map"));
        EventManager.instance.dispatchEvent(new ScreenSelectedCommand("map"));
    }

    protected function getStateIfExists(name:String):String
    {
        for each(var state:State in this.states)
        {
            if (state.name == name)
                return name;
        }

        return "";
    }

    public function str(name: String, params: * = null, n: int = -1): String {
        return Client.instance.str(name, params, n);
    }
}
}