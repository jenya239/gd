package com.turbostool.client.game.components.car {
import com.turbostool.client.dynamicEngine.*;
import com.turbostool.client.game.IGameComponentModel;
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.geom.Rectangle;
import com.turbostool.client.model.UpgradeInfo;
import com.turbostool.client.net.ErrorDataEvent;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;

import flash.events.Event;
import flash.events.EventDispatcher;
import mx.controls.Alert;

public class CarModel extends EventDispatcher implements IGameComponentModel
{
    private var mySystem:CarDynamicEngine;
    private var isUpgraded:Boolean = false;

    public function CarModel(width:Number, length:Number)
    {
        mySystem = new CarDynamicEngine(width, length);
    }

    public function get myCenterOfMass():Vector2d
    {
        return mySystem.myChassis.getHull().myFrame.myR;
    }

    public function get myHull():Hull
    {
        return mySystem.myChassis.getHull();
    }

    public function get myVelocity():Vector2d
    {
        return myHull.myVelocity;
    }

    public function set myVelocity(setValue:Vector2d):void
    {
        myHull.myVelocity = setValue;
    }

    public function get myMass():Number
    {
        return myHull.myMass;
    }

    public function get myMomentOfInertia():Number
    {
        return myHull.myMomentOfInertia;
    }

    public function get myAngularVelocity():Number
    {
        return myHull.myAngularVelocity;
    }

    public function set myAngularVelocity(setValue:Number):void
    {
        myHull.myAngularVelocity = setValue;
    }

    public function rollback():void
    {
        mySystem.rollback();
    }

    public function get myR():Vector2d
    {
        return myHull.myFrame.myR;
    }

    public function getDynamics():Collection
    {
        var dynamics:ArrayList = new ArrayList();
        dynamics.addItem(mySystem);

        return dynamics;
    }

    public function getCollidables():Collection
    {
        var collidables:ArrayList = new ArrayList();
        collidables.addItem(this);
        return collidables;
    }

    public function getHullRectangle():Rectangle
    {
        return mySystem.myHull.getRectangle();
    }

    public function getVisibleWheels():Collection
    {
        return mySystem.myChassis.getWheels();
    }

    public function get myFrame():LocalFrame2d
    {
        return myHull.myFrame;
    }

    public function setHullMass(mass:Number):void
    {
        mySystem.myHull.myMass = mass;
    }

    public function setHullMomentOfInertia(inertia:Number):void
    {
        mySystem.myHull.myMomentOfInertia = inertia;
    }

    public function get myAccelerate():Boolean
    {
        return mySystem.myEngineControl.myAccelerate;
    }

    public function set myAccelerate(setValue:Boolean):void
    {
        mySystem.myEngineControl.myAccelerate = setValue;
    }

    public function get myBrake():Boolean
    {
        return mySystem.myBrakes.myActive;
    }

    public function set myBrake(setValue:Boolean):void
    {
        mySystem.myBrakes.myActive = setValue;
    }

    public function get myRudderControl():RudderControl
    {
        return mySystem.myRudder;
    }

    //		public function set myRudderControl(setValue:int):void {
    //			mySystem.myRudder.myAction = setValue;
    //		}

    [Bindable]
    public function get engine(): CarEngine
    {
        return mySystem.myCarEngine;
    }

    public function set engine(value: CarEngine): void
    {
        mySystem.myCarEngine = value;
    }

    public function getChassis():Chassis
    {
        return mySystem.myChassis;
    }

    public function getTransmission():Transmission
    {
        return mySystem.myTransmission;
    }

    public function getRudder():RudderControl
    {
        return mySystem.myRudder;
    }

    public function getHull():Hull
    {
        return mySystem.myHull;
    }

    public function getSuspension():Suspension
    {
        return mySystem.mySuspension;
    }

    public function getBrakes():Brakes
    {
        return mySystem.myBrakes;
    }

    public function getRPM():Number
    {
        return engine.myAngularVelocity * 60 / (2 * Math.PI);
    }

    public function getEngineAngularVelocity():Number
    {
        return engine.myAngularVelocity;
    }

    public function getMaxRPM():Number
    {
        return engine.getMaxAngularVelocity() * 60 / (2 * Math.PI);
    }

    public function getFrontWheelsAngle():Number
    {
        return mySystem.myRudder.myAngle;
    }

    public function get myHandBrake():Boolean
    {
        return mySystem.myBrakes.myHandBrake;
    }

    public function set myHandBrake(setValue:Boolean):void
    {
        mySystem.myBrakes.myHandBrake = setValue;
    }

    public function getGearNumber():int
    {
        return getTransmission().myNumber;
    }

    public function getGearCoef():Number
    {
        return getTransmission().getCoef();
    }

    public function getLongAngle():Number
    {
        return mySystem.mySuspension.getLongAngle();
    }

    public function getMaxLongAngle():Number
    {
        return mySystem.mySuspension.getMaxLongAngle();
    }

    public function getLateralAngle():Number
    {
        return mySystem.mySuspension.getLateralAngle();
    }

    public function getMaxLateralAngle():Number
    {
        return mySystem.mySuspension.getMaxLateralAngle();
    }

    public function getLongEquilibrium():Number
    {
        return mySystem.mySuspension.getLongEquilibrium();
    }

    public function getLateralEquilibrium():Number
    {
        return mySystem.mySuspension.getLateralEquilibrium();
    }

    public function getWeight():Number
    {
        return myHull.myWeight;
    }

    public function getTotalPinForce():Vector2d
    {
        return mySystem.myChassis.getTotalPinForceGlobal();
    }

    public function isDrift():Boolean
    {
        return !mySystem.myChassis.isRails();
    }

    public function isRudderRotated():Boolean
    {
        return mySystem.myChassis.isRudderRotated();
    }

    public function getTurnVector():Vector2d
    {
        return mySystem.myChassis.getTurnVector();
    }

    public function getSuspensionDelta(v:Vector2d):Number
    {
        return getSuspension().getDeltaZ(v);
    }

    public function getMaxSuspensionDelta(v:Vector2d):Number
    {
        return getSuspension().getMaxDeltaZ(v);
    }

    public function getGear():Number
    {
        return getTransmission().myNumber;
    }

    //function set myGear(setValue:Number):void;
    //function getMaxGear():int;

    public function setGear(setValue:Number):void
    {
        getTransmission().myNumber = setValue;
    }

    public function getCarDirectionVelocity():Number
    {
        return - myVelocity.scalarProjection(myFrame.getYOrt());
    }

    public function getLateralAngularVelocity():Number
    {
        return mySystem.mySuspension.getLateralAngularVelocity();
    }

    public function getLongAngularVelocity():Number
    {
        return mySystem.mySuspension.getLongAngularVelocity();
    }

    public function get myDifferential() :Differential
    {
        return mySystem.myDiff;
    }

    public function get myCarDynamicEngine():CarDynamicEngine
    {
        return mySystem;
    }

    public function getAngularVelocity():Number
    {
        return mySystem.myHull.myAngularVelocity;
    }

    public function get myGlobalPolygon():Polygon
    {
        return myHull.myGlobalPolygon;
    }

    public function get myPreviousGlobalPolygon():Polygon
    {
        return myHull.myPreviousGlobalPolygon;
    }

    public function updateBorder():void
    {
        myHull.updateBorder();
    }

    public function getBorder():BodyBorder
    {
        return myHull.getBorder();
    }

    public function get myGeomFrame():LocalFrame2d
    {
        return myHull.myGeomFrame;
    }

    public function getChassisLength(): Number
    {
        return getChassis().myLength;
    }

    public function getLongShift(): Number
    {
        return getChassis().myMassCenterLongShift;
    }

    public function getRudderAngle(): Number
    {
        return getChassis().myRudderAngle;
    }

    public function get myRearLeft():Wheel
    {
        return getChassis().myRearLeft;
    }

    public function get myRearRight():Wheel
    {
        return getChassis().myRearRight;
    }

    public function get myFrontRight():Wheel
    {
        return getChassis().myFrontRight;
    }

    public function get myFrontLeft():Wheel
    {
        return getChassis().myFrontLeft;
    }

    public function fullStop():void
    {
        getChassis().fullStop();
        getSuspension().fullStop();
    }

    public function getEngineParameters(): EngineParameters
    {
        return engine.myParameters;
    }

    public function get maxSpeed():Number
    {
        var max: Number = Math.abs(engine.getMaxAngularVelocity() * getChassis().myFrontRight.myRadius / getTransmission().maxCoef);
        return max;
    }

    public function updateCar():void
    {
        dispatchEvent(new Event(Event.CHANGE));
    }

    public function startEmergencyStop(): void
    {
        myBrake = true;
        myHandBrake = true;
        myAccelerate = false;
    }

    public function get myOrientation():Vector2d {
        return myFrame.getYOrt().invertion2d();
    }

    public function setOrientationBy(vector: Vector2d): void {
        //а ведь здесь надо поворачивать относительно центра симметрии, а не центра масс!
        myFrame.myAngle = vector.getAngle() + Math.PI / 2;
    }

    public function get myMinTurnRadius(): Number {
        var carLength: Number = getChassisLength();
        var angle: Number = getRudder().myMaxAngle;
        var beta: Number = Math.atan(0.5 * Math.tan(angle));
        var radius: Number = carLength / (2 * Math.sin(beta));
        return radius;
    }

    public function set myMinTurnRadius(setValue: Number): void {
        var carLength: Number = getChassisLength();
        var beta: Number = Math.asin(carLength / (2 * setValue));
        var angle: Number = Math.atan(2 * Math.tan(beta));
        getRudder().myMaxAngle = angle;
    }

    public function applyUpgradeInfo(upgrades:UpgradeInfo):void{
        applyUpgrades(upgrades.power,upgrades.speed,upgrades.controllability,upgrades.braking);
    }

    public function applyUpgrades(power: Number, speed: Number, controllability: Number, braking: Number): void {
        if (isUpgraded) {
            Alert.show("Car is already upgraded");
            throw new Error("is already Upgraded");
        }
        isUpgraded = true;
        var power1:Number = normalizeParameter(power);
        var speed1:Number = normalizeParameter(speed);
        var controllability1 :Number = normalizeParameter(controllability);
        var braking1:Number = normalizeParameter(braking);
        getEngineParameters().myMaxTorque *= power1;
        getEngineParameters().myStartTorque *= power1;
        getEngineParameters().myEndTorque *= power1;

        getEngineParameters().myMaxVelocity *= speed1;
        getEngineParameters().myEndMaxMomentVelocity *= speed1;

        myCarDynamicEngine.myChassis.myStaticFriction *= controllability1;
        mySystem.myBrakes.myBrakeMoment *= braking;
        //Alert.show("apply car upgrade"+power+" "+speed+" " +controllability+" " )
    }
    private function normalizeParameter(param:Number):Number{
        return param;
      //  var a:Number = param;
      //  if(a < 0) {a = 0;}
      //  if(a > 1000) {a = 1000;}
      //  return 1 + (3 * a / 1000);
    }
}
}
