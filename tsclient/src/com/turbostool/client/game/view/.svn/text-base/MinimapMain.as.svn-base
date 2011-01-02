package com.turbostool.client.game.view
{
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.newGraphic.NGCamera;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

import flash.display.BitmapData;
import flash.display.Graphics;
import flash.geom.Matrix;

import mx.containers.Canvas;
import mx.events.FlexEvent;

public class MinimapMain extends Canvas
{
    private var _minimapSize: Number;//215;
    private var _radius: Number
    private var _raceModel:RaceModel;
    private var _offset:Vector2d = new Vector2d(-200, 0);
    private static const BASE_SCALE: Number = 2;

    private var _camera: NGCamera;

    private var cnvObstacle: Canvas;
    private var cnvCars: Canvas;
    public var cnvMain: Canvas;

    private var _screenCarR: Vector2d;


    public function MinimapMain()
    {
        addEventListener(FlexEvent.INITIALIZE, onInitialize);
    }

    private function onInitialize(event:FlexEvent):void
    {
        _minimapSize = cnvMain.width;
        _radius = _minimapSize / 2;

        _screenCarR = new Vector2d(_minimapSize / 2, _minimapSize / 2);
        cnvObstacle = new Canvas();
        cnvCars = new Canvas();
        updateRaceModel();
    }

    private function get myScale(): Number
    {
        return 20 / _camera.myHeight;
    }

    private function drawCar(gr:Graphics, car:Car):void
    {
        var rc:Vector2d = car.myR.numberProduct2d(BASE_SCALE);
        var rcL:Vector2d = car.carModel.myFrame.getYOrt().numberProduct2d(3 / 2 * car.carModel.getChassisLength());
        var rcB:Vector2d = rc.difference2d_shared(rcL);
        var rcE:Vector2d = rc.sum2d(rcL);
        var color: uint = ( car.carModel == _raceModel.localCar.carModel ) ? 0xff3a3a : 0x185001;
        gr.lineStyle(32, color);
        gr.moveTo(rcB.myX, rcB.myY);
        gr.lineTo(rcE.myX, rcE.myY);
        gr.lineStyle();
        gr.moveTo(0, 0);
        gr.lineTo(8, 0);
    }

    private var _zero: Vector2d = new Vector2d(0, 0);
    private var _tempMatrix: Matrix = new Matrix();

    public function draw(/*gr: Graphics, angle:Number*/):void
    {
        if (_raceModel == null || _raceModel.localCar == null)
            return;

        var angle:Number = _camera.myFrame.myAngle;

        var gr: Graphics = cnvCars.graphics;
        gr.clear();
        _screenCarR.difference2d_result(_raceModel.localCar.carModel.myFrame.myR.numberProduct2d(myScale).rotate(-angle), _zero);

        cnvObstacle.rotation = -angle * 180 / Math.PI;
        cnvObstacle.x = _zero.myX;
        cnvObstacle.y = _zero.myY;

        cnvCars.rotation = -angle * 180 / Math.PI;
        cnvCars.x = _zero.myX;
        cnvCars.y = _zero.myY;

        var it:Iterator;
        var cars: Collection = _raceModel.getCars();

        for(var i:int=0; i<cars.length(); i++)
        {
            var car: Car = Car(cars.getItemAt(i));
            drawCar(gr, car);
        }

        _tempMatrix.identity();
        _tempMatrix.scale(myScale / BASE_SCALE, myScale / BASE_SCALE);
        _tempMatrix.rotate(-angle);
        _tempMatrix.translate(_zero.myX, _zero.myY);

        var b:BitmapData = new BitmapData(_minimapSize, _minimapSize, true, 0);

        b.draw(cnvObstacle, _tempMatrix);
        b.draw(cnvCars, _tempMatrix);

        cnvMain.graphics.clear();
        cnvMain.graphics.beginBitmapFill(b);
        cnvMain.graphics.drawCircle(_minimapSize / 2, _minimapSize / 2, _radius);
        cnvMain.graphics.endFill();

    }

    private function drawStartLine(gr: Graphics): void
    {
        var slb:Vector2d = _raceModel.getRaceWorld().getStartLine().myBegin.numberProduct2d(BASE_SCALE);
        var sle:Vector2d = _raceModel.getRaceWorld().getStartLine().myEnd.numberProduct2d(BASE_SCALE);
        gr.lineStyle(10, 0xe17811);
        gr.beginFill(0);
        gr.moveTo(slb.myX, slb.myY);
        gr.lineTo(sle.myX, sle.myY);
        gr.endFill();
    }

    private function drawObstacles(gr: Graphics): void
    {
        var polyIter: Iterator = _raceModel.getRaceWorld().getCollidables().iterator();
        gr.lineStyle(3, 0xe17811);
        while (polyIter.hasNext())
        {
            gr.beginFill(0x000000, 0.6);
            var om: ObstacleModel = polyIter.next() as ObstacleModel;
            var vit:Iterator = om.mySequence.vertexIterator();
            var vv: Vector2d = om.mySequence.lastVertex().numberProduct2d(BASE_SCALE);
            gr.moveTo(vv.myX, vv.myY);
            while (vit.hasNext())
            {
                vv = (vit.next() as Vector2d).numberProduct2d(BASE_SCALE);
                gr.lineTo(vv.myX, vv.myY);
            }
            gr.endFill();
        }
    }

    private function drawCameraPath(gr: Graphics): void
    {
        var vit: Iterator = _raceModel.getRaceWorld().getCameraPath().vertexIterator();
        gr.lineStyle(30, 0xb0c6be);
        var vv: Vector2d = _raceModel.getRaceWorld().getCameraPath().lastVertex().numberProduct2d(BASE_SCALE);
        gr.moveTo(vv.myX, vv.myY);
        while (vit.hasNext())
        {
            vv = (vit.next() as Vector2d).numberProduct2d(BASE_SCALE);
            gr.lineTo(vv.myX, vv.myY);
        }
        gr.endFill();
    }

    private function renderBitmap(gr: Graphics): void
    {
        gr.clear();
        //drawObstacles(gr);
        drawCameraPath(gr);
        drawStartLine(gr);
    }

    private function updateRaceModel(): void
    {
        //todo check
        if (_raceModel == null || _raceModel.getRaceWorld() == null) return;
        renderBitmap(cnvObstacle.graphics);
    }

    public function set raceModel(rm: RaceModel):void
    {
        _raceModel = rm;
        updateRaceModel();
        draw();
    }

    public function get raceModel():RaceModel
    {
        return _raceModel;
    }

    public function set camera(val:NGCamera):void
    {
        _camera = val;
    }

    public function get camera():NGCamera
    {
        return _camera;
    }
}
}