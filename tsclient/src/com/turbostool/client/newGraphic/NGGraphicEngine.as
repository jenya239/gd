package com.turbostool.client.newGraphic
{
import com.turbostool.client.utils.Assert;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Iterator;

import flash.geom.Matrix;

import mx.containers.Canvas;
import mx.core.UIComponent;

public class NGGraphicEngine
{
    private static var _instance: NGGraphicEngine = null;

    protected var _canvas: UIComponent;
		private var _overlay: Canvas = new Canvas();
    private var _camera: NGCamera;
    protected var _graphics: NGGraphics;
    private var _drawables: ArrayList = new ArrayList();
    private var _lastTime: Number = 0;
    private var _tickCount: uint = 0;
    private var _fps: uint = 0;

    public var centerOffsetX: Number = 0;
    public var centerOffsetY: Number = 0;

    public function NGGraphicEngine(camera: NGCamera)
    {
        if (_instance != null)
        {
            throw new TSError("Вызов конструктора синглетона NGGraphicEngine");
        }
        _graphics = new NGGraphics();
        _camera = camera;
        _instance = this;
    }

    public static function get instance(): NGGraphicEngine
    {
        Assert.assertNotNull(_instance);
        return _instance;
    }

    public function get overlay(): Canvas
    {
        return _overlay;
    }

    public function set canvas(value: UIComponent): void
    {
			if( _canvas != null ) _canvas.removeChild( _overlay );
			if( value != null ) value.addChild( _overlay );
			_canvas = value;
			_graphics.graphics = value != null ? value.graphics : null;
    }

    public function get width(): Number
    {
        return _canvas != null ? _canvas.width : 570;
    }

    public function get height(): Number
    {
        return _canvas != null ? _canvas.height : 500;
    }

    private var _baseMatrix: Matrix = new Matrix();
    private var _scaleMatrix: Matrix = new Matrix();

    public function draw(): void
    {

        if (_graphics.graphics == null)
            return;

        _tickCount++;
        var now: Number = Utils.now();
        var delta: Number = Math.abs(now - _lastTime);
        if (delta > 2000)
        {
            _fps = Math.round(1000 * _tickCount / delta);
            _lastTime = now;
            _tickCount = 0;
        }

        _graphics.clear();
        //дно стека преобразваний. все координаты в пикселях

        _baseMatrix.identity();
        _baseMatrix.translate(width / 2 + centerOffsetX, height / 2 + centerOffsetY);
        _graphics.pushTransform(_baseMatrix); // теперь нулевые координаты камеры -- центр канваса
        _graphics.pushTransform(_camera.getScaleTransform()); // переход к системе отсчета в метрах
        //для отсечения
        var halfDiag: Number = Math.sqrt(width * width + height * height) * _camera.myHeight / (2 * _camera.myScreenHeight);
        var it: Iterator = _drawables.iterator();
        while (it.hasNext())
        {
            var d: NGDrawable = it.next() as NGDrawable;
            if (_camera.myHeight < d.myHeight)
            {// отсечение объектов "над камерой"
                continue;
            }
            if (Math.sqrt(d.myRadiusSqr) + halfDiag < _camera.myFrame.myR.difference2d_shared(d.myGeometricCenter).length())
            {
                continue;
            }
            //эффект перспективы необходимо вызывать у каждого объекта свой
            //эффект перспективы
            var scale: Number = _camera.myHeight / (_camera.myHeight - d.myHeight);
            _scaleMatrix.identity();
            _scaleMatrix.scale(scale, scale);
            _graphics.pushTransform(_scaleMatrix);
            //эффект перспективы
            _graphics.pushTransform(_camera.getTranslateTransform()); // учитываем положение камеры
            d.draw(_graphics);
            _graphics.popTransform();
            //эффект перспективы
            _graphics.popTransform();
            //эффект перспективы ENDS
        }
        //выход из системы отсчета в метрах
        _graphics.popTransform();
        _graphics.popTransform();
    }

    public function drawablesCompare(d1: NGDrawable, d2: NGDrawable): int
    {
        var h1: Number = d1.myHeight;
        var h2: Number = d2.myHeight;
        if (h1 != h2)
        {
            return Utils.sign(h1 - h2);
        }
        var l_1: Number = d1.myLayer;
        var l_2: Number = d2.myLayer;
        return Utils.sign(l_1 - l_2);
    }

    public function addDrawable(d: NGDrawable): void
    {
        _drawables.insertSorted(d, drawablesCompare);
        if (d.myShadow != null && !(d is NGShadow))
        {
            _drawables.insertSorted(d.myShadow, drawablesCompare);
        }
    }

    public function sortDrawables(): void
    {
        _drawables.sort(drawablesCompare);
    }

    public function removeAllDrawables(): void
    {
        _drawables.clear();
    }

    public function getCamera(): NGCamera
    {
        return _camera;
    }

//    public function screenToWorld(): Matrix
//    {
//        var m: Matrix = _camera.getInverseTransform();
//        var res: Matrix = new Matrix();
//        res.translate(- width / 2 - centerOffsetX, - height / 2 - centerOffsetY);
//        res.concat(m);
//        return res;
//    }

//    public function worldToScreen(): Matrix
//    {
//        var m: Matrix = new Matrix();
//        m.translate(width / 2 + centerOffsetX, height / 2 + centerOffsetY);
//        var res: Matrix = _camera.getTransform();
//        res.concat(m);
//        return res;
//    }

    public function removeDrawable(d: NGDrawable): void
    {
        _drawables.removeItem(d);
    }

    public function get myFPS(): uint
    {
        return _fps;
    }
}
}