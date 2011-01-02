package com.turbostool.client.game.view
{
import com.turbostool.client.game.components.car.CarModel;

import flash.display.Graphics;

import mx.containers.Canvas;
import mx.controls.Image;
import mx.controls.Label;
import mx.controls.Text;

public class TachometerMain extends Canvas
{
    public var lSpeed:Label;
    public var lGear:Label;
    public var iRPMArrow: Image;
    public var iSpeedArrow: Image;
    public var cnvTachometerScale: Canvas;

    private var _carModel: CarModel;

    private var mySpeedometer:Label;
    private var myGearLabel:Text;
    private var myTahometerLength: Number = 100;
    private const RPM_ARROW_CENTER_X: Number = 55.0; // 93;//103;
    private const RPM_CENTER_CENTER_Y: Number = 54.7; //126;//133;

    private const SPEED_ARROW_CENTER_X: uint = 100;
    private const SPEED_CENTER_CENTER_Y: uint = 63;

    private const RPM_MIN_ANGLE: Number = (-30 + 180) * Math.PI / 180;
    private const RPM_MAX_ANGLE: Number = (125 + 180) * Math.PI / 180;

    private const SPEED_MIN_ANGLE: Number = (-40 + 180) * Math.PI / 180;
    private const SPEED_MAX_ANGLE: Number = (180 + 40 + 180) * Math.PI / 180;

    private const POINTER_SHIFT:int = 11;
    private const SCALE_MIN_RADIUS: Number = 32;//65;
    private const SCALE_MAX_RADIUS: Number = 37;//70;


    //    private var _initialized:Boolean = false;

    public function TachometerMain()
    {

    }

    //    public function init(car:CarModel):void
    //    {
    //        car = car;
    //        //myCar.addEventListener( Event.CHANGE, updateScale);
    //        updateScale();
    //        _initialized = true;
    //
    //    }


    private function drawArc(beginAngle: Number, endAngle: Number, color: uint):void
    {
        var gr: Graphics = cnvTachometerScale.graphics;
        gr.beginFill(color);
        gr.moveTo(RPM_ARROW_CENTER_X + SCALE_MIN_RADIUS * Math.cos(beginAngle), RPM_CENTER_CENTER_Y + SCALE_MIN_RADIUS * Math.sin(beginAngle));
        gr.lineTo(RPM_ARROW_CENTER_X + SCALE_MAX_RADIUS * Math.cos(beginAngle), RPM_CENTER_CENTER_Y + SCALE_MAX_RADIUS * Math.sin(beginAngle));
        arcTo(SCALE_MAX_RADIUS, beginAngle, endAngle);
        gr.lineTo(RPM_ARROW_CENTER_X + SCALE_MIN_RADIUS * Math.cos(endAngle), RPM_CENTER_CENTER_Y + SCALE_MIN_RADIUS * Math.sin(endAngle));
        arcTo(SCALE_MIN_RADIUS, endAngle, beginAngle);
        gr.endFill();
    }

    public function updateScale():void
    {
        cnvTachometerScale.graphics.clear();
        var greenBegin: Number = _carModel.getEngineParameters().myStartMaxMomentVelocity / _carModel.getEngineParameters().myMaxVelocity * (RPM_MAX_ANGLE - RPM_MIN_ANGLE) + RPM_MIN_ANGLE;
        var greenEnd: Number = _carModel.getEngineParameters().myEndMaxMomentVelocity / _carModel.getEngineParameters().myMaxVelocity * (RPM_MAX_ANGLE - RPM_MIN_ANGLE) + RPM_MIN_ANGLE;
        drawArc(RPM_MIN_ANGLE, greenBegin, 0xFBD52F);
        drawArc(greenBegin, greenEnd, 0x91E120);
        drawArc(greenEnd, RPM_MAX_ANGLE, 0xA30807);
    }

    private function arcTo(radius: Number, beginAngle: Number, endAngle: Number):void
    {
        const COUNT: Number = 100.0;
        for (var i: int = 1; i <= COUNT; i++)
        {
            var angle: Number = (endAngle - beginAngle) * i / COUNT + beginAngle;
            //angle /= Math.PI / 180; //вернуть шестеренку ж)
            cnvTachometerScale.graphics.lineTo(RPM_ARROW_CENTER_X + radius * Math.cos(angle), RPM_CENTER_CENTER_Y + radius * Math.sin(angle));
        }
    }

    public function updateInstruments():void
    {
        if (_carModel == null) return;
        var angle:Number = (Math.abs(_carModel.getRPM()) / _carModel.getMaxRPM()) * (RPM_MAX_ANGLE - RPM_MIN_ANGLE) + RPM_MIN_ANGLE;
        iRPMArrow.rotation = angle * 180 / Math.PI;
        iRPMArrow.x = RPM_ARROW_CENTER_X + iRPMArrow.height / 2 * Math.sin(angle) - POINTER_SHIFT * Math.cos(angle) - 5;
        iRPMArrow.y = RPM_CENTER_CENTER_Y - iRPMArrow.height / 2 * Math.cos(angle) - POINTER_SHIFT * Math.sin(angle) - 5;

        angle = _carModel.myVelocity.length() / _carModel.maxSpeed * (SPEED_MAX_ANGLE - SPEED_MIN_ANGLE) + SPEED_MIN_ANGLE;
        iSpeedArrow.rotation = angle * 180 / Math.PI;
        iSpeedArrow.x = SPEED_ARROW_CENTER_X + iSpeedArrow.height / 2 * Math.sin(angle) - POINTER_SHIFT * Math.cos(angle);
        iSpeedArrow.y = SPEED_CENTER_CENTER_Y - iSpeedArrow.height / 2 * Math.cos(angle) - POINTER_SHIFT * Math.sin(angle);

        var speed:Number = Math.round(_carModel.myVelocity.length() * 3.6);
        lSpeed.text = speed.toString();
        if (speed < 100) lSpeed.text = "0" + speed.toString();
        if (speed < 10) lSpeed.text = "00" + speed.toString();


        lSpeed.text = Math.round(_carModel.myVelocity.length() * 3.6) < 100 ? "0" + Math.round(_carModel.myVelocity.length() * 3.6).toString() : Math.round(_carModel.myVelocity.length() * 3.6).toString();
        //			updateSpeed(Math.round(myCar.myVelocity.length() * 3.6));


        if (_carModel.getTransmission().myNumber > 0)
        {
            lGear.text = _carModel.getTransmission().myNumber.toString();
        }
        else
        {
            lGear.text = "R";
        }
    }

    private function lineTo(gr: Graphics, rpm: Number):void
    {
        var angle:Number = (rpm / _carModel.getMaxRPM()) * Math.PI / 2;
        gr.lineTo(100 - Math.cos(angle) * myTahometerLength, 100 - Math.sin(angle) * myTahometerLength);
    }

    public function get carModel():CarModel
    {
        return _carModel;
    }

    public function set carModel(val:CarModel):void
    {
        _carModel = val;
        updateScale();
        updateInstruments();
    }
}
}