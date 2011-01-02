package com.turbostool.carEditor {
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.EditableCarParameters;
import com.turbostool.client.utils.Utils;

import flash.events.MouseEvent;
import flash.events.TimerEvent;
import flash.utils.Timer;

import mx.containers.VBox;
import mx.controls.Alert;
import mx.controls.Button;
import mx.controls.Label;
import mx.controls.TextInput;
import mx.events.FlexEvent;

public class CarEditorPanelBase extends VBox{
    private var _manager: EventManager = EventManager.instance;
    private var _car: Car = null;
    private var _upgradedCar: Car = null;

    public var tiPower: TextInput;
    public var tiSpeed: TextInput;
    public var tiControllability: TextInput;
    public var tiBraking: TextInput;
    public var btnApply: Button;

    public function CarEditorPanelBase() {
        _manager.addEventListener(CarSelectedEvent.CAR_SELECTED, onCarSelect);
        addEventListener(FlexEvent.CREATION_COMPLETE, onCreated);
    }

    private function onCreated(e: FlexEvent): void {
        btnApply.addEventListener(MouseEvent.CLICK, onApplyClick);
    }

    private function resetInputs(): void {
        tiPower.text = "0";
        tiSpeed.text = "0";
        tiControllability.text = "0";
        tiBraking.text = "0";
    }

    private function onCarSelect(e: CarSelectedEvent): void {
        if( _car == null ){
            resetInputs();
            _car = e.car;
        }else if( _car == e.car ){
            resetInputs();
            _upgradedCar = null;
        }else if( _upgradedCar == e.car ){
            //ничего не делать
        }else{
            resetInputs();
            _car = e.car;    
        }
    }

    private function onApplyClick(e: MouseEvent): void {
        _upgradedCar = _car.clone();
        _upgradedCar.carModel.applyUpgrades(parseFloat(tiPower.text), parseFloat(tiSpeed.text),
            parseFloat(tiControllability.text), parseFloat(tiBraking.text));
        _manager.dispatchEvent(new CarSelectedEvent(_upgradedCar));
    }
}
}