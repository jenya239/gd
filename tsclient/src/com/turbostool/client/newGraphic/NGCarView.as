package com.turbostool.client.newGraphic
{
import com.turbostool.client.Config;
import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.game.components.car.Chassis;
import com.turbostool.client.game.components.car.Wheel;
import com.turbostool.client.utils.Vector2d;

import flash.display.BitmapData;
import flash.filters.DropShadowFilter;
import flash.filters.GlowFilter;
import flash.geom.ColorTransform;
import flash.geom.Matrix;
import flash.geom.Point;

import flash.text.TextField;

import flash.text.TextFormat;

import mx.controls.Label;

public class NGCarView extends NGDrawable
{
    private const TEXTURE_GRID_SIZE: Number = 1;
    private var myNinecarTemplate: BitmapData;
    private var myNinecar: BitmapData;

    private var myCar: CarModel;
    private var myColorProperty: uint;
    private var myCarShadow: BitmapData;
    private var myShadowProperty: NGShadow;
    public var myDebugMode: Boolean;
    private var _lblUserName: Label;

    public function NGCarView(car: CarModel, texture: BitmapData = null) {
			super(car.myGeomFrame, HeightConstants.CAR, LayerConstants.CAR);
			_lblUserName = new Label();
			_lblUserName.setStyle("fontFamily", "Arial");
			_lblUserName.setStyle("fontSize", 12);
			_lblUserName.setStyle("fontWeight", "bold");
			_lblUserName.filters = [new GlowFilter(0x010101, 1, 10, 10, 5)];

			myCar = car;
			myNinecarTemplate = (texture == null) ? new BitmapData(20, 20) : texture;
			initTextures();
			myShadowProperty = NGShadow.createCarShadow(this, myCarShadow);
			myDebugMode = ( Config.initialized ) ? Config.instance.drawDebugData : false;
    }

    public function get lblUserName():Label
    {
        return _lblUserName;
    }

    public function setDisplayName(name: String, cityId: int, level: int):void
    {
			_lblUserName.text = "["+level+"]"+name;
			trace("cityId = " + cityId);
			_lblUserName.setStyle("color", (cityId == 1) ? 0x198bb5 : 0xb52319 );
    }

    override public function get myShadow(): NGShadow {
        return myShadowProperty;
    }

    override public function get myWidth(): Number {
        return myCar.getHullRectangle().myWidth;
    }

    override public function get myLength(): Number {
        return myCar.getHullRectangle().myHeight;
    }

    private function initTextures(): void {
        myNinecar = myNinecarTemplate.clone();

        myCarShadow = myNinecar.clone();
        var myCarShadow2: BitmapData = myNinecar.clone();
        // 0x3d4433
        // 0x18281e
        setTextureColor(myCarShadow, myNinecar, 0, 0x9f);
        //setTextureColor_Fast(myCarShadow2, myNinecar, 0, 0x9f);

    }

    private function getOffset(x: Number, max: Number, max2: Number): Number {
        if (TEXTURE_GRID_SIZE == 1)
        {
            return 0;
        } else
        {
            var delta: Number = max / (TEXTURE_GRID_SIZE - 1);
            var roundedX: Number = Math.round((x + max) / (2 * delta));
            var delta2: Number = max2 / (TEXTURE_GRID_SIZE - 1);
            return roundedX * 2 * delta2 - max2;
        }
    }

    private function getXOffset(): Number {
        return getOffset(myCar.getLateralAngle(), myCar.getMaxLateralAngle(), myWidth * (TEXTURE_GRID_SIZE - 1) / 2);
    }

    private function getYOffset(): Number {
        return getOffset(-myCar.getLongAngle(), myCar.getMaxLongAngle(), myLength * (TEXTURE_GRID_SIZE - 1) / 2);
    }

    public function dirtyHack(ngg: NGGraphics, texture: BitmapData): void {
        ngg.drawShiftedTexturedRect(texture, myWidth, myLength, myWidth * TEXTURE_GRID_SIZE, myLength * TEXTURE_GRID_SIZE, getXOffset(), getYOffset());
    }

		private var _point: Point = new Point();
		private var LABEL_POSITION: Point;
		private function updateLabelPosition(toScreen: Matrix): void{
			if( LABEL_POSITION == null ) LABEL_POSITION = new Point(myWidth * 0.4, myLength * 0.4);
			NGGraphics.transformPoint(toScreen, LABEL_POSITION, _point);
			_lblUserName.x = _point.x;
			_lblUserName.y = _point.y;
		}
	
    override protected function drawContent(ngg: NGGraphics): void {
			updateLabelPosition(ngg.myProductMatrix);
			dirtyHack(ngg, myNinecar);
			if (myDebugMode) {
					drawDebugData(ngg);
			}
    }

    private function drawDebugData(ngg: NGGraphics): void {
        const WHEEL_LEN: Number = 1;
        var force: Vector2d;
        var ch: Chassis = myCar.getChassis();

        var carWr2: Number = ch.myRearPinWidth / 2;
        var carWf2: Number = ch.myFrontPinWidth / 2;
        var carL2: Number = myCar.getChassisLength() / 2;

        drawWheelDebug(myCar.myFrontRight, + carWf2, - carL2, ch.myFrontRight.myFrame.myAngle - ch.myHullFrame.myAngle);
        drawWheelDebug(myCar.myFrontLeft, - carWf2, - carL2, ch.myFrontLeft.myFrame.myAngle - ch.myHullFrame.myAngle);
        drawWheelDebug(myCar.myRearRight, + carWr2, + carL2, 0);
        drawWheelDebug(myCar.myRearLeft, - carWr2, + carL2, 0);

        function drawWheelDebug(wheel: Wheel, wheelX: Number, wheelY: Number, wheelAngle: Number): void {
            var wm: Matrix = new Matrix();
            wm.rotate(wheelAngle);
            wm.translate(wheelX, wheelY);
            ngg.pushTransform(wm);

            if (wheel.myRoadGrip) {
                ngg.drawLine(0, WHEEL_LEN / 2, 0, - WHEEL_LEN / 2, 0x000000, 4);
            } else {
                ngg.drawLine(0, WHEEL_LEN / 2, 0, - WHEEL_LEN / 2, 0xFF0000, 2);
            }
            var localV: Vector2d = new Vector2d(wheelX, wheelY + myCar.getLongShift());
            var y0: Number = myCar.getMaxSuspensionDelta(localV) * 1.1;
            var y: Number = myCar.getSuspensionDelta(localV);
            ngg.drawLine(0, 5 * y0, 0, 5 * y, 0x00FFFF, 6);
            force = wheel.myFrame.getLocalNonLength(wheel.myForce);
            //trace (force);
            force = force.numberProduct2d(15 / (myCar.getWeight()));
            ngg.drawLine(0, 0, force.myX, force.myY, 0x00FF00, 2);

            ngg.popTransform();
        }

        var tvColor: uint = 0xF0F0F0;
        var cm: Matrix = new Matrix();
        cm.translate(0, - myCar.getLongShift());
        ngg.pushTransform(cm);
        if (myCar.getChassis().isRails()) {
            tvColor = 0x0000FF;
            force = myCar.getTotalPinForce().numberProduct2d(4 / myCar.getWeight());
            var fLocal: Vector2d = myCar.myFrame.getLocalNonLength(force);
            ngg.drawLine(0, 0, fLocal.myX, fLocal.myY, 0x00FF00, 2);
        }
        var tv: Vector2d = (myCar.isRudderRotated())
                ? myCar.getTurnVector()
                : Vector2d.createFrom3d(myCar.myFrame.getXOrt().numberProduct(100));
        var tvLocal: Vector2d = myCar.myFrame.getLocalNonLength(tv).invertion2d();
        ngg.drawLine(0, 0, tvLocal.myX, tvLocal.myY, tvColor, 1);
        ngg.popTransform();
    }

    public function get myColor():uint {
        return myColorProperty;
    }

    private static const _zeroPoint: Point = new Point(0, 0);

    public static function setTextureColor(dest: BitmapData, src: BitmapData, color: uint, alfa: uint = 0xff):void
    {

        //var t1: Number = Utils.now();

        var r: Number = ((color >> 16) & 0xFF) / 255;
        var g: Number = ((color >> 8) & 0xFF) / 255;
        var b: Number = (color & 0xFF) / 255;
        var a: Number = alfa / 255;

        var colorTransform: ColorTransform = new ColorTransform(r, g, b, a, 0, 0, 0, 0);

        var colorTransform2: ColorTransform = new ColorTransform();
        colorTransform2.color = color;

        if (dest != src)
        {
            dest.copyPixels(src, dest.rect, _zeroPoint);
        }

        dest.colorTransform(dest.rect, colorTransform);

        //var dt: Number = Utils.now() - t1;
        //trace(Utils.stringFormat("setTextureColor_Fast(...) took {0} msec", dt));
    }

    public function set myColor(setValue: uint): void {
        myColorProperty = setValue;
        //fsetTextureColor(myNinecar, myNinecarTemplate, myColorProperty);

    }

    public function get myTexure(): BitmapData {
        return myNinecarTemplate;
    }
}
}