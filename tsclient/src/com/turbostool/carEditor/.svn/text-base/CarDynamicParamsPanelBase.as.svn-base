package com.turbostool.carEditor {
import com.turbostool.client.event.EventManager;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.utils.Utils;

import flash.events.TimerEvent;
import flash.utils.Timer;

import mx.containers.VBox;
import mx.controls.Label;

public class CarDynamicParamsPanelBase extends VBox{
    private var _manager: EventManager = EventManager.instance;
    private var _car: Car = null;
    private var _timer: Timer = new Timer(40);

    public var lblVelocity: Label;
    public var lblClutch: Label;

    public function CarDynamicParamsPanelBase() {
        _manager.addEventListener(CarSelectedEvent.CAR_SELECTED, onCarSelect);
        _timer.addEventListener(TimerEvent.TIMER, updateView);
    }

    private function onCarSelect(e: CarSelectedEvent): void {
        _car = e.car;
        _timer.start();
    }

    private function updateView(e: TimerEvent): void {
        lblVelocity.text = Utils.numberToString(_car.carModel.myVelocity.length() * 3.6, 4, 1) + " km/h";
        lblClutch.text = Utils.numberToString(_car.carModel.engine.myEngineCoef, 4, 2);
    }
}
}