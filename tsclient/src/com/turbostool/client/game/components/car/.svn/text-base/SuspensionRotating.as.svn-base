package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Assert;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;

public class SuspensionRotating{
    ////// + RotatingDynamic
    private var myAngleProperty:Number;
    private var myAngularVelocityProperty:Number;
    private var myMomentOfInertiaProperty:Number;
    private var myMomentOfForceProperty:Number;

    private var mySavedAngle:Number;
    private var mySavedAngularVelocity:Number;

    ///////

    private var myMaxAngle:Number;
    private var myAngDeflectionRate:Number;
    private var myDamping:Number;
    private var myEquilibriumAngleProperty:Number;
    private var myOldAngle:Number;
    private var mySavedOldAngle:Number;

    public function SuspensionRotating(momentOfInertia:Number, maxAngle:Number,
                                       dampingCoef:Number = 10

*
    1000
)
{
    myAngleProperty = 0;
    myAngularVelocityProperty = 0;
    myMomentOfInertiaProperty = momentOfInertia;
    myMomentOfForceProperty = 0;
    mySavedAngle = 0;
    mySavedAngularVelocity = 0;
    saveState();
    //
    Assert.assertTrue(maxAngle > 0);
    myEquilibriumAngleProperty = 0;
    myMaxAngle = Math.abs(maxAngle);
    myDamping = dampingCoef;
    myOldAngle = myAngle;
    saveState();
}

    /**
     * ВОобще говоря угловой коэф. жесткости в подвеске нужен только для оценки периода колебаний.
     * Чтобы если он слишком маленький не расчитывать движение с использованием dt как обычно,
     * а установить положение равновесия
     */
    public function setDeflectionRate(deflectionRate:Number, r:Number):void {
        myAngDeflectionRate = deflectionRate * r * r;
    }

    public function set myAngle(setValue:Number):void {
        myAngleProperty = Utils.getNear(setValue, - myMaxAngle, myMaxAngle);
    }

    public function calcForces():void {
        myMomentOfForce -= myDamping * myAngularVelocity;
    }

    public function getMaxAngle():Number {
        return myMaxAngle;
    }

    public function get myEquilibriumAngle():Number {
        return myEquilibriumAngleProperty;
    }

    public function set myEquilibriumAngle(setValue:Number):void {
        myEquilibriumAngleProperty = setValue;
    }

    public function get myDampingCoefficient():Number {
        return myDamping;
    }

    public function set myDampingCoefficient(coef:Number):void {
        myDamping = coef;
    }

    public function saveState():void {
        mySavedAngle = myAngle;
        mySavedAngularVelocity = myAngularVelocity;
        mySavedOldAngle = myOldAngle;
    }

    public function rollback():void {
        myAngle = mySavedAngle;
        myAngularVelocity = mySavedAngularVelocity;
        myOldAngle = mySavedOldAngle;
    }

    public function toEquilibrium():void {
        myAngle = myEquilibriumAngle;
        myAngularVelocity = 0;
    }

    public function calcCoordinates(dt:Number):void {
        var T:Number = 2 * Math.PI * Math.sqrt(myMomentOfInertia / myAngDeflectionRate);
        if (dt > 0.1 * T) {
            toEquilibrium();
        } else {
            myAngle += myAngularVelocity * dt;
        }
    }

    //  RotatingDynamic

    public function calcVelocity(dt:Number):void
    {
        if (Utils.equal(0, myMomentOfInertia))
            throw new TSError('Невозможно посчитать угловое ' +
                              'ускорение при действии момента сил на нулевой момент инерции');
        myAngularVelocity += myMomentOfForce * dt / myMomentOfInertia;
    }

    public function get myMomentOfInertia():Number
    {
        return myMomentOfInertiaProperty;
    }

    public function set myMomentOfInertia(setValue:Number):void {
        myMomentOfInertiaProperty = setValue;
    }

    public function get myAngle():Number
    {
        return myAngleProperty;
    }

    public function get myAngularVelocity():Number
    {
        return myAngularVelocityProperty;
    }

    public function set myAngularVelocity(setValue:Number):void
    {
        var prevAngularVelocity:Number = myAngularVelocityProperty;
        myAngularVelocityProperty = setValue;
        myAngularVelocityProperty = (prevAngularVelocity * myAngularVelocityProperty < 0)
                ? 0
                : myAngularVelocityProperty;
    }

    public function get myMomentOfForce():Number {
        return myMomentOfForceProperty;
    }

    public function set myMomentOfForce(setValue:Number):void {
        myMomentOfForceProperty = setValue;
    }

    public function nullForces():void {
        myMomentOfForce = 0;
    }
}
}