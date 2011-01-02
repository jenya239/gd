package com.turbostool.client.game.components.car
{
import com.turbostool.client.game.IGameComponent;
import com.turbostool.client.game.IGameComponentModel;
import com.turbostool.client.newGraphic.LayerConstants;
import com.turbostool.client.newGraphic.NGCarView;
import com.turbostool.client.newGraphic.NGDrawable;
import com.turbostool.client.newGraphic.NGShadow;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.*;

import flash.display.BitmapData;
import flash.utils.describeType;

public class Car implements IGameComponent
{
    public static const LOCAL_CAR_ID: int = -1;

    private var myModel: CarModel;
    private var myNGView: NGCarView;
    private var myName: String;
    private var myController: CarController;
    private var _clientID: int;
    private var myRouteNameProperty: String;
    private var _currentLap: int = -1;
    private var _lastPosition: Vector2d;
    private var _firstCarState: Boolean = true;

    public function Car(id: int = 0, texture: BitmapData = null)
    {
        _clientID = id;
        myName = getClassName() + id;
        myModel = new CarModel(2, 4);
        myNGView = new NGCarView(myModel, texture);
		if( id == LOCAL_CAR_ID ) myNGView.myLayer = LayerConstants.LOCAL_CAR;
        myController = new CarController(myModel);
        myRouteNameProperty = '';
    }

    public function get firstCarState():Boolean
    {
        return _firstCarState;
    }

    public function set firstCarState(value:Boolean):void
    {
        _firstCarState = value;
    }

    public function getClassName(): String
    {
        var fullName:String = describeType(this).attribute("name").toString();
        return fullName.split('::')[1];
    }

    public function getName():String
    {
        return myName;
    }

    public function getModel():IGameComponentModel
    {
        return myModel;
    }

    public function getController():CarController
    {
        return myController;
    }

    [Bindable]
    public function get carModel():CarModel
    {
        return myModel;
    }

    public function set carModel(val:CarModel):void
    {
        myModel = val;
    }

    public function setName(setValue:String):void
    {
        myName = setValue;
    }

    public function get clientID():int
    {
        return _clientID;
    }

    public function get myR():Vector2d
    {
        return Vector2d.createFrom3d(carModel.myR);
    }

    public function get myColor():uint
    {
        return myNGView.myColor;
    }

    public function set myColor(setValue:uint):void
    {
        myNGView.myColor = setValue;
    }

    public function getDrawables():Collection
    {
        return new ArrayList();
    }

    public function getModels():Collection
    {
        return new ArrayList(getModel());
    }

    public function get myNGCarView(): NGCarView
    {
        return myNGView;
    }

    public function getEditableCarParameters(): EditableCarParameters
    {
        return new EditableCarParameters(this);
    }

    public function getNGDrawable(): NGDrawable
    {
        return myNGView;
    }

    public function getShadow():NGShadow
    {
        return myNGCarView.myShadow;
    }

    public function get isLocal(): Boolean
    {
        return clientID == LOCAL_CAR_ID;
    }

    public function set clientID(id: int): void
    {
        _clientID = id;
    }

    public function clone(): Car
    {
        var car: Car = new Car(_clientID, myNGCarView.myTexure);
        car.getEditableCarParameters().load(new XML(getEditableCarParameters().save()), false, false);
        return car;
    }

    [Bindable]
    public function get currentLap(): int
    {
        return _currentLap;
    }

    public function set currentLap(value: int): void
    {
        this._currentLap = value;
    }

    public function get lastPosition(): Vector2d
    {
        return _lastPosition;
    }

    public function set lastPosition(value: Vector2d): void
    {
        this._lastPosition = value;
    }

}
}