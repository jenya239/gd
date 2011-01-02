package com.turbostool.carEditor
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.components.car.EditableCarParameters;
import com.turbostool.client.utils.Utils;

import flash.events.Event;
import flash.events.MouseEvent;
import flash.net.URLLoader;
import flash.net.URLRequest;
import flash.net.URLRequestMethod;

import mx.containers.Canvas;
import mx.controls.DataGrid;
import mx.controls.TextInput;
import mx.utils.StringUtil;

public class CarEditorScreenBase extends Canvas
{

    private var _manager: EventManager = EventManager.instance;
    public var dgGear: DataGrid;
    public var tiName: TextInput;

    [Bindable]
    public var ecp: EditableCarParameters = null;

    public function CarEditorScreenBase()
    {
        super();
        _manager.addEventListener(CarSelectedEvent.CAR_SELECTED, onCarSelect);
    }

    private function onCarSelect(e: CarSelectedEvent): void
    {
        ecp = e.car.getEditableCarParameters();
    }

    protected function get gearCoefs(): XML
    {
        var xmlGear:XML = new XML('<gearCoefs/>');
        for each (var xmlNode1:XML in dgGear.dataProvider) {
            xmlGear.appendChild(xmlNode1);
        }
        return xmlGear;
    }

    protected function saveClickHandler(event: MouseEvent):void {
        var url: String = ((StringUtil.trim(tiName.text) == "") || (StringUtil.trim(tiName.text) == ecp.myName))
                ? Utils.stringFormat('carServices.php?action=save&name={0}', ecp.myName)
                : Utils.stringFormat('carServices.php?action=save&name={0}&oldName={1}', tiName.text, ecp.myName);
        var request:URLRequest = new URLRequest(url);
        request.contentType = "text/xml";
        request.data = ecp.save();
        request.method = URLRequestMethod.POST;
        var loader:URLLoader = new URLLoader();
        loader.addEventListener(Event.COMPLETE, saveCompletedHandler);
        try {
            loader.load(request);
        } catch (error:ArgumentError) {
            trace("An ArgumentError has occurred.");
        } catch (error:SecurityError) {
            trace("A SecurityError has occurred.");
        }
        enabled = false;
        function saveCompletedHandler(event:Event):void {
            CarEditor.instance.loaderPanel.loadCarList();
            CarEditor.instance.loaderPanel.savedCarName = ecp.myName;
            enabled = true;
        }
    }

}
}