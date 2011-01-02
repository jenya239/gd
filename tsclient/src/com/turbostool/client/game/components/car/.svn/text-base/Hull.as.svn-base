package com.turbostool.client.game.components.car {
import com.turbostool.client.dynamicEngine.BodyBorder;
import com.turbostool.client.dynamicEngine.ICameraLinkable;
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.geom.Rectangle;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

/**
 * само тело автомобиля. Рассчитывает всю физику
 * должен уметь
 * - взаимодействовать с воздухом
 * - взаимодействовать с гравитацией
 *
 *
 */
public class Hull implements ICameraLinkable {
    private var myRect:Rectangle;
    private var myFrameProperty:LocalFrame2d;
    private var myVelocityProperty:Vector2d;
    private var myAngularVelocityProperty:Number;
    private var myMomentOfInertiaProperty:Number;
    private var myMomentOfForceProperty:Number;
    private var myMassProperty:Number;
    private var myForceProperty:Vector2d;
    private var myGeomFrameProperty:LocalFrame2d;//do not use directly
    private var mySavedGeomFrameProperty:LocalFrame2d;//do not use directly
    private var myGlobalPolygonProperty:Polygon;
    private var myPreviousGlobalPolygonProperty:Polygon;
    private var myPressingCoefProperty:Number;
    private var myAreaOfCutProperty:Number;
    private var myWidthHalfProperty:Number;
    private var myLengthHalfProperty:Number;
    private var myBorder:BodyBorder;

    private var myChassis:Chassis;

    private var mySavedFrame:LocalFrame2d;

    public function Hull(mass:Number, zInertia:Number, width:Number, length:Number) {
        myRect = new Rectangle(Vector2d.getZero(), width, length);
        myFrameProperty = new LocalFrame2d(new Vector2d(0, 0), 0);
        myVelocityProperty = Vector2d.getZero();
        myAngularVelocityProperty = 0;
        myMomentOfInertiaProperty = zInertia;
        myMomentOfForceProperty = 0;
        mySavedFrame = new LocalFrame2d(new Vector2d(0, 0), 0);
        myMassProperty = mass;
        myForceProperty = new Vector2d(0, 0);
        //myWeightProperty = 0;
        myPressingCoefProperty = 0;
        myAreaOfCutProperty = 1;
        myGeomFrameProperty = new LocalFrame2d(new Vector2d(0, 0), 0);
        mySavedGeomFrameProperty = new LocalFrame2d(new Vector2d(0, 0), 0);
        myGlobalPolygonProperty = new Polygon();
        myGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myPreviousGlobalPolygonProperty = new Polygon();
        myPreviousGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myPreviousGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myPreviousGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myPreviousGlobalPolygonProperty.addVertex(new Vector2d(0, 0));
        myWidthHalfProperty = width / 2;
        myLengthHalfProperty = length / 2;
        myBorder = new BodyBorder();
    }

    public function calcCoordinates(dt:Number):void {
        myFrame.myAngle += myAngularVelocity * dt;
        myFrame.myR.addMultiplied(myVelocity, dt);
        myChassis.calcCoordinates(dt);
    }

    public function nullForces():void {
        myForce.setZero();
        myMomentOfForce = 0;
        myForce.addMultiplied(myVelocity, - myAreaOfCutProperty * Utils.AIR_DENSITY * myVelocity.length());
    }

    public function calcVelocity(dt:Number):void {
        myAngularVelocity += myMomentOfForce * dt / myMomentOfInertia;
        myVelocity.addMultiplied(myForce, dt / myMass);
    }

    public function saveState():void {
        myFrame.copyTo(mySavedFrame);
    }

    public function rollback():void {
        mySavedFrame.copyTo(myFrame);
        myChassis.updateWheelFrames();
        myChassis.updateWheelVelocity();
    }

    public function get myFrame():LocalFrame2d {
        return myFrameProperty;
    }

    public function get myVelocity():Vector2d {
        return myVelocityProperty;
    }

    public function set myVelocity(v:Vector2d):void {
        myVelocityProperty = v;
    }

    public function get myMomentOfInertia():Number
    {
        return myMomentOfInertiaProperty;
    }

    public function set myMomentOfInertia(setValue:Number):void {
        myMomentOfInertiaProperty = setValue;
    }

    public function get myAngularVelocity():Number {
        return myAngularVelocityProperty;
    }

    public function set myAngularVelocity(value:Number):void {
        myAngularVelocityProperty = value;
    }

    public function get myMomentOfForce():Number {
        return myMomentOfForceProperty;
    }

    public function set myMomentOfForce(setValue:Number):void {
        myMomentOfForceProperty = setValue;
    }

    public function get myMass():Number {
        return myMassProperty;
    }

    public function set myMass(setValue:Number):void {
        myMassProperty = setValue;
    }

    public function get myForce():Vector2d
    {
        return myForceProperty;
    }

    public function getRectangle():Rectangle {
        return myRect;
    }

    public function setChassis(chassis:Chassis):void {
        myChassis = chassis;
    }

    public function get myWeight():Number {
        return myMassProperty * Utils.G + myVelocity.sqrLength() * Utils.AIR_DENSITY * myPressingCoefProperty;
    }

    public function get myPressingCoef():Number {
        return myPressingCoefProperty;
    }

    public function get myAreaOfCut():Number {
        return myAreaOfCutProperty;
    }

    public function set myPressingCoef(c:Number):void {
        myPressingCoefProperty = c;
    }

    public function set myAreaOfCut(c:Number):void {
        myAreaOfCutProperty = c;
    }

    public function get myGlobalPolygon():Polygon {
        return myGlobalPolygonProperty;
    }

    public function get myPreviousGlobalPolygon():Polygon {
        return myPreviousGlobalPolygonProperty;
    }

    public function updatePolygons():void {
        var globalArray:Array = myPreviousGlobalPolygonProperty.getVertices().toArray();
        var currArray:Array = myRect.getVertices().toArray();
        var currentGlobalArray:Array = myGlobalPolygonProperty.getVertices().toArray();
        var curr:Vector2d;
        var res:Vector2d;
        for (var i:uint = 0; i < 4; i++) {
            curr = currArray[i];
            res = globalArray[i];
            curr.copyTo2d(res);
            res.rotate(mySavedGeomFrame.myAngle);
            res.add2d(mySavedGeomFrame.myR);
            //
            res = currentGlobalArray[i];
            curr.copyTo2d(res);
            res.rotate(myGeomFrame.myAngle);
            res.add2d(myGeomFrame.myR);
        }
    }

    private function compareBorderAndFrame(border:BodyBorder, frame:LocalFrame2d):void {
        var sin:Number = Math.sin(frame.myAngle);
        var cos:Number = Math.cos(frame.myAngle);
        var l_2:Number = myLengthHalfProperty; //need optimization. Создать поле length в Hull
        var w2:Number = myWidthHalfProperty;
        var rectAbsMaxX:Number;
        var rectAbsMaxY:Number;
        if (sin >= 0 && cos > 0) {
            rectAbsMaxX = w2 * cos + l_2 * sin;
            rectAbsMaxY = l_2 * cos + w2 * sin;
        } else if (sin > 0 && cos <= 0) {
            rectAbsMaxX = l_2 * sin - w2 * cos;
            rectAbsMaxY = - l_2 * cos + w2 * sin;
        } else if (sin <= 0 && cos < 0) {
            rectAbsMaxX = -w2 * cos - l_2 * sin;
            rectAbsMaxY = -l_2 * cos - w2 * sin;
        } else {
            rectAbsMaxX = -l_2 * sin + w2 * cos;
            rectAbsMaxY = + l_2 * cos - w2 * sin;
        }
        border.myMaxX = Math.max(frame.myR.myX + rectAbsMaxX, border.myMaxX);
        border.myMinX = Math.min(frame.myR.myX - rectAbsMaxX, border.myMinX);
        border.myMaxY = Math.max(frame.myR.myY + rectAbsMaxY, border.myMaxY);
        border.myMinY = Math.min(frame.myR.myY - rectAbsMaxY, border.myMinY);
    }


    public function updateBorder():void {
        myBorder.myMaxX = Number.NEGATIVE_INFINITY;
        myBorder.myMaxY = Number.NEGATIVE_INFINITY;
        myBorder.myMinX = Number.POSITIVE_INFINITY;
        myBorder.myMinY = Number.POSITIVE_INFINITY;
        compareBorderAndFrame(myBorder, myGeomFrame);
        compareBorderAndFrame(myBorder, mySavedGeomFrame);
    }

    public function getBorder():BodyBorder {
        return myBorder;
    }

    public function get myGeomFrame():LocalFrame2d {
        return     getShiftedFrame(myFrame, myGeomFrameProperty);
    }

    public function get mySavedGeomFrame():LocalFrame2d {
        return     getShiftedFrame(mySavedFrame, mySavedGeomFrameProperty);
    }

    private function getShiftedFrame(massCenter:LocalFrame2d, geomCenter:LocalFrame2d):LocalFrame2d {
        var alpha:Number = massCenter.myAngle;
        var sin:Number = Math.sin(alpha);
        var cos:Number = Math.cos(alpha);
        var rx:Number = massCenter.myR.myX - myChassis.myMassCenterLongShift * sin; //здесь myFrame - центр масс
        var ry:Number = massCenter.myR.myY + myChassis.myMassCenterLongShift * cos;
        geomCenter.setXYU(rx, ry, alpha);
        return geomCenter;
    }

    public function set myLengthHalf(value:Number):void {
        myLengthHalfProperty = value;
    }

    public function set myWidthHalf(value:Number):void {
        myWidthHalfProperty = value;
    }

    public function get myLength():Number {
        return myLengthHalfProperty * 2;
    }

    public function get myWidth(): Number {
        return myWidthHalfProperty * 2;
    }

}
}