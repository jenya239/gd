package com.turbostool.client.game.components.car {

public class EngineControl{
    private var myAccelerateProperty:Boolean = false;
    private var myAccelerationVelocity:Number = 0;
    private var myBaseAccelerationVelocityProperty:Number = 1;
    private var myEngine:CarEngine;

    public function EngineControl(engine:CarEngine) {
        super();
        myEngine = engine;
    }

    public function calcVelocity(dt:Number):void {
        //изврат какой-то. этому ифу место в calcCoordinates
        if (myAccelerateProperty) {
            myAccelerationVelocity = myBaseAccelerationVelocityProperty * 0.1;
        } else {
            myAccelerationVelocity = - myBaseAccelerationVelocityProperty * 0.01;
        }
    }

    public function calcCoordinates(dt:Number):void {
        myEngine.myEngineCoef += myAccelerationVelocity * dt;
        if (myEngine.myEngineCoef > 1) {
            myEngine.myEngineCoef = 1;
        }
        if (myEngine.myEngineCoef < 0) {
            myEngine.myEngineCoef = 0;
        }
    }

    public function get myAccelerate():Boolean {
        return myAccelerateProperty;
    }

    public function set myAccelerate(setValue:Boolean):void {
        myAccelerateProperty = setValue;
    }

    public function get myBaseAccelerationVelocity():Number {
        return myBaseAccelerationVelocityProperty;
    }

    public function set myBaseAccelerationVelocity(setValue:Number):void {
        myBaseAccelerationVelocityProperty = setValue;
    }
}
}