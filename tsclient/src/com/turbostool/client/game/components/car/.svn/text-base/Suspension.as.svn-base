package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.Iterator;

public class Suspension{
    private const MAX_LATERAL_ANGLE: Number = 12 * Math.PI / 180;
    private const MAX_LONG_ANGLE: Number = 2 * Math.PI / 180;

    private var myXRotating:SuspensionRotating;
    private var myYRotating:SuspensionRotating;
    private var myEqMatrix:Matrix2d;
    private var myHeight:Number; //растояние по Z от центра масс авто до точки касания с дорогой
    private var myDeflectionRate:Number;
    private var myEquilibriumMoment:Vector2d; //нужен для определения положения равновесия подвески
    private var myTempWheelMoment:Vector2d;
    private var myChassisProperty:Chassis;
    private var myHullProperty:Hull;

    public function Suspension(chassis:Chassis, hull:Hull,
                               height:Number = 0.4,
                               deflectionRate:Number = 18

*
    1000,
            dampingCoef
:
    Number = 5000,
            xInertia
:
    Number = 500,
            yInertia
:
    Number = 300
)
{
    myChassisProperty = chassis;
    myHullProperty = hull;
    myXRotating = new SuspensionRotating(xInertia, MAX_LONG_ANGLE, dampingCoef);
    myYRotating = new SuspensionRotating(yInertia, MAX_LATERAL_ANGLE, dampingCoef);
    myHeight = height;
    myStiffness = deflectionRate;
    myEquilibriumMoment = Vector2d.getZero();
    myTempWheelMoment = new Vector2d(0, 0);
    updateMassCenter();
}

    public function get myXMomentOfInertia():Number {
        return myXRotating.myMomentOfInertia;
    }

    public function updateMassCenter():void {
        myEqMatrix = getEqMatrix();
    }

    public function get myYMomentOfInertia():Number {
        return myYRotating.myMomentOfInertia;
    }

    public function set myXMomentOfInertia(moment:Number):void {
        myXRotating.myMomentOfInertia = moment;
    }

    public function set myYMomentOfInertia(moment:Number):void {
        myYRotating.myMomentOfInertia = moment;
    }

    public function get myXDamping():Number {
        return myXRotating.myDampingCoefficient;
    }

    public function fullStop(): void {
        calcEquilibrium();
        myXRotating.toEquilibrium();
        myYRotating.toEquilibrium();
        myXRotating.myAngularVelocity = 0;
        myYRotating.myAngularVelocity = 0;
    }

    public function set myXDamping(coef:Number):void {
        myXRotating.myDampingCoefficient = coef;
    }

    public function get myYDamping():Number {
        return myYRotating.myDampingCoefficient;
    }

    public function set myYDamping(coef:Number):void {
        myYRotating.myDampingCoefficient = coef;
    }

    public function calcCoordinates(dt:Number):void {
        calcEquilibrium();
        myXRotating.calcCoordinates(dt);
        myYRotating.calcCoordinates(dt);
    }

    public function get myStiffness():Number {
        return myDeflectionRate;
    }

    public function set myStiffness(coef:Number):void {
        myDeflectionRate = coef;
        myXRotating.setDeflectionRate(coef, myChassis.myLength / 2);
        myYRotating.setDeflectionRate(coef, (myChassis.myRearPinWidth + myChassis.myFrontPinWidth) / 4);
    }

    public function getDeltaZ(r:Vector2d):Number {
        return - r.myX * myYRotating.myAngle + r.myY * myXRotating.myAngle;
    }

    public function getMaxDeltaZ(r:Vector2d):Number {
        return Math.abs(r.myX * myYRotating.getMaxAngle())
                + Math.abs(r.myY * myXRotating.getMaxAngle());
    }

    public function nullForces():void {
        myXRotating.nullForces();
        myYRotating.nullForces();
        var xMoment:Number = 0;
        var yMoment:Number = 0;

        for (var i:int = 0; i < Chassis.WHEEL_COUNT; i++) {
            var wheel:Wheel = myChassis.getWheelByIndex(i);
            var r:Vector2d = myHull.myGeomFrame.getLocal(wheel.myR);
            var wz:Number = getDeltaZ(r); //+rw.z
            var fz:Number = - wz * myDeflectionRate;
            var f0:Number = (myHull.myWeight / Chassis.WHEEL_COUNT) * (r.myY - myChassis.myMassCenterLongShift) / r.myY;
            var newWeight:Number = f0 - fz;
            wheel.setWeight(newWeight);
            xMoment += fz * r.myY;
            yMoment += - fz * r.myX;
        }
        myXRotating.myMomentOfForce = xMoment;
        myYRotating.myMomentOfForce = yMoment;
    }

    /**
     * учитываем момент сил от сил трения с дорогой
     */
    private function considerWheelForce(wheel:Wheel, x:Number, y:Number):void {
        var localF:Vector2d = myChassis.myHullFrame.getLocalNonLength(wheel.myForce);
        myTempWheelMoment.myX = - localF.myY * myHeight;
        myTempWheelMoment.myY = myHeight * localF.myX;
        myEquilibriumMoment.addMultiplied(myTempWheelMoment, -1);
        myXRotating.myMomentOfForce += myTempWheelMoment.myX;
        myYRotating.myMomentOfForce += myTempWheelMoment.myY;
    }

    public function calcForces():void {
        myEquilibriumMoment.setZero();
        myXRotating.calcForces();
        myYRotating.calcForces();
        considerWheelForce(myChassis.myFrontLeft, -myChassis.myFrontPinWidth / 2, - myChassis.myLength / 2);
        considerWheelForce(myChassis.myFrontRight, myChassis.myFrontPinWidth / 2, - myChassis.myLength / 2);
        considerWheelForce(myChassis.myRearLeft, -myChassis.myRearPinWidth / 2, myChassis.myLength / 2);
        considerWheelForce(myChassis.myRearRight, myChassis.myRearPinWidth / 2, myChassis.myLength / 2);
        if (myChassis.isRails()) {
            var fpLocal:Vector2d = myChassis.myHullFrame.getLocalNonLengthNonSafe(
                    myChassis.getTotalPinForceGlobal()
                    );
            myXRotating.myMomentOfForce -= myHeight * fpLocal.myY;
            myYRotating.myMomentOfForce += myHeight * fpLocal.myX;
        }
    }

    public function calcVelocity(dt:Number):void {
        myXRotating.calcVelocity(dt);
        myYRotating.calcVelocity(dt);
    }

    public function rollback():void {
        myXRotating.rollback();
        myYRotating.rollback();
    }

    public function saveState():void {
        myXRotating.saveState();
        myYRotating.saveState();
    }

    public function getLongAngle():Number {
        return myXRotating.myAngle;
    }

    public function getMaxLongAngle():Number {
        return myXRotating.getMaxAngle();
    }

    public function getLateralAngle():Number {
        return myYRotating.myAngle;
    }

    public function getMaxLateralAngle():Number {
        return myYRotating.getMaxAngle();
    }

    public function getLongEquilibrium():Number {
        return myXRotating.myEquilibriumAngle;
    }

    public function getLateralEquilibrium():Number {
        return myYRotating.myEquilibriumAngle;
    }

    public function getEqMatrix():Matrix2d {
        var m:Matrix2d = new Matrix2d();
        for (var i:int = 0; i < Chassis.WHEEL_COUNT; i++) {
            var wheel:Wheel = myChassis.getWheelByIndex(i);
            var r:Vector2d = myChassis.myHullFrame.getLocal(wheel.myR);
            r.myY += myChassis.myMassCenterLongShift;
            m.myA11 += r.myY * r.myX;
            m.myA12 -= Utils.sqr(r.myY);
            m.myA21 += Utils.sqr(r.myX);
        }
        m.myA22 = - m.myA11;
        return m;
    }

    public function calcEquilibrium():void {
        //try
        var right:Vector2d = new Vector2d(
                myEquilibriumMoment.myX / myDeflectionRate,
                - myEquilibriumMoment.myY / myDeflectionRate
                );
        //trace();
        //trace(right);
        var angles:Vector2d;
        try {
            angles = Utils.solveSystem2d(myEqMatrix, right);
            myYRotating.myEquilibriumAngle = angles.myX;
            myXRotating.myEquilibriumAngle = angles.myY;
        } catch (e:TSError) {
            trace();
            trace(e);
            trace(getEqMatrix());
            trace(myChassis.myRudderAngle);
            trace('turnVector ' + myChassis.getTurnVector());
            trace('pinForce ' + myChassis.getTotalPinForce());
            trace('pinForceGlobal ' + myChassis.getTotalPinForceGlobal());
            trace('force ' + myChassis.getHull().myForce);
            trace('vel ' + myChassis.getHull().myVelocity);
            trace('frame ' + myChassis.getHull().myFrame);
            var it:Iterator = myChassis.getWheels().iterator();
            while (it.hasNext()) {
                var w:Wheel = it.next() as Wheel;
                trace('wheel');
                trace(w.myFrame);
                //trace(myChassis.getLocalWheelR(w));
            }
        }
        //trace(getEqMatrix());
        //trace(angles);
    }

    public function setLateralAngle(val:Number):void {
        myYRotating.myAngle = val;
    }

    public function setLongAngle(val:Number):void {
        myXRotating.myAngle = val;
    }

    public function getLateralMoment():Number {
        return myYRotating.myMomentOfForce;
    }

    public function getLongMoment():Number {
        return myXRotating.myMomentOfForce;
    }

    public function getLateralAngularVelocity():Number {
        return myYRotating.myAngularVelocity;
    }

    public function getHeight(): Number {
        return myHeight;
    }

    public function setHeight(h: Number): void {
        myHeight = h;
    }

    public function setLateralAngularVelocity(vel:Number): void {
        myYRotating.myAngularVelocity = vel;
    }

    public function getLongAngularVelocity():Number {
        return myXRotating.myAngularVelocity;
    }

    public function setLongAngularVelocity(vel:Number): void {
        myXRotating.myAngularVelocity = vel;
    }

    public function get myChassis():Chassis {
        return myChassisProperty;
    }

    public function set myChassis(chassis:Chassis):void {
        myChassisProperty = chassis;
    }

    public function get myHull():Hull {
        return myHullProperty;
    }

    public function set myHull(hull:Hull):void {
        myHullProperty = hull;
    }

}
}