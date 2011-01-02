package com.turbostool.client.game.components.car {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.utils.StateDataError;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

public class Wheel {
    private static const MAX_GRIP_VELOCITY:Number = 0.1;

    private var mySlidingFrictionProperty:Number;
    private var myStaticFrictionProperty:Number;
    private var myRetardingTorqueProperty:Number;
    private var myEngineTorqueProperty:Number;

    private var myWeightProperty:Number;
    private var myVelocityProperty:Vector2d;
    private var myGlobalFrameProperty:LocalFrame2d;
    private var myPinAngularVelocityProperty:Number;
    private var myPinMomentOfForceProperty:Number;
    private var myPinMomentOfInertiaProperty:Number;
    private var myRadiusProperty:Number;
    private var myRoadGripProperty:Boolean;
    private var myForceProperty:Vector2d;

    public function Wheel(radius:Number = 0.22, pinMomentOfInertia:Number = 0.4) {
        //was: thick:Number=0.1
        //super(radius, thick, pinMomentOfInertia, mass, zMomentOfInertia);
        if (Utils.isZero(pinMomentOfInertia))
            throw new TSError('Невозможно посчитать угловое ' +
                              'ускорение при действии момента сил на нулевой момент инерции');
        myPinMomentOfInertiaProperty = pinMomentOfInertia;
        mySlidingFrictionProperty = 1.3;
        myStaticFrictionProperty = 1.5;
        myEngineTorqueProperty = 0;
        myRetardingTorqueProperty = 0;

        myVelocityProperty = new Vector2d(0, 0);
        myGlobalFrameProperty = new LocalFrame2d(new Vector2d(0, 0), 0);
        myPinAngularVelocityProperty = 0;
        myRadiusProperty = radius;
        myPinMomentOfForceProperty = 0;
        myWeightProperty = 0;
        myRoadGripProperty = true;
        myForceProperty = Vector2d.getZero();
    }

    public function get myRoadGrip():Boolean {
        return myRoadGripProperty;
    }

    public function setSliding():void {
        myRoadGripProperty = false;
        calcForces();
    }

    public function get myVelocity():Vector2d {
        return myVelocityProperty;
    }

    public function tryDefinePinAngular(): void {
        //если на колесо есть привод и оно проскальзывает вдоль оси, то угловую скорость подстраивать не надо
        //if( !Utils.isZero(myEngineTorqueProperty) && slidingBecauseOfNormalVelocity() ){
        //	return;
        //}
        //далее идет проверка надо ли подстраивать угловую скорость под линейную
        //грубо говоря, если сила трения сильнее, то надо
        var friction:Number = ( myVelocity.getScalarProjectionByAngle(myGlobalFrameProperty.myAngle) > MAX_GRIP_VELOCITY )
                ? mySlidingFrictionProperty
                : myStaticFrictionProperty;
        var maxFriction:Number = myWeightProperty * friction;
        if (maxFriction < 0) {
            throw new TSError();
        }
        if (maxFriction < Utils.EPSILON) {
            return;
        }
        var fy:Number = ( myRetardingTorqueProperty + myEngineTorqueProperty) / myRadiusProperty;
        if (fy > maxFriction || fy < -maxFriction) {
            return;
        }
        definePinAngularVelocity();
    }

    public function set myVelocity(setValue:Vector2d):void {
        myVelocityProperty = setValue;
        tryDefinePinAngular();
    }

    public function get myEngineTorque():Number {
        return myEngineTorqueProperty;
    }

    public function set myEngineTorque(t:Number):void {
        myEngineTorqueProperty = t;
    }

    public function get myRetardingTorque():Number {
        return myRetardingTorqueProperty;
    }

    public function set myRetardingTorque(retardingTorque:Number):void {
        myRetardingTorqueProperty = retardingTorque;
    }

    public function get myAngle():Number {
        return myGlobalFrameProperty.myAngle;
    }

    public function get myR():Vector2d {
        return myFrame.myR;
    }

    /**
     * moment действующий на колесо со стороны дороги
     */
    private function getForceY(moment:Number):Number {
        return - moment / myRadius;
    }

    /**
     * сила приложенная к колесу со стороны дороги
     */
    private function getRoadMoment(forceY:Number):Number {
        return - forceY * myRadius;
    }

    public function get mySlidingFriction():Number {
        return mySlidingFrictionProperty;
    }

    public function set mySlidingFriction(slidingFriction:Number):void {
        mySlidingFrictionProperty = slidingFriction;
    }

    public function get myStaticFriction():Number {
        return myStaticFrictionProperty;
    }

    public function set myStaticFriction(staticFriction:Number):void {
        myStaticFrictionProperty = staticFriction;
    }

    //fx - проекция силы со стороны дороги  на направление оси вращиня
    public function getMaxFXWhenRoadGrip() : Number {
        if (! myRoadGrip) {
            throw new StateDataError();
        }
        var fy:Number = Math.abs((myRetardingTorqueProperty + myEngineTorqueProperty) / myRadiusProperty);
        var maxFriction:Number = myWeightProperty * myStaticFrictionProperty;
        if (fy > maxFriction || maxFriction <= 0) { //в этом случае нет сцепления с дорогой
            //throw new StateDataError();
            return 0;
        }
        return Math.sqrt(maxFriction * maxFriction - fy * fy);
    }

    public function getMaxFXIfRoadGrip(): Number {
        var fy:Number = Math.abs((myRetardingTorqueProperty + myEngineTorqueProperty) / myRadiusProperty);
        var maxFriction:Number = myWeightProperty * myStaticFrictionProperty;
        if (fy > maxFriction || maxFriction <= 0) { //в этом случае нет сцепления с дорогой
            return 0;
        }
        return Math.sqrt(maxFriction * maxFriction - fy * fy);
    }

    public function calcVelocity(dt:Number):void {
        var friction:Number = ( getPinVelocity() > MAX_GRIP_VELOCITY )
                ? mySlidingFriction
                : myStaticFriction;
        var maxFriction:Number = myWeight * friction;
        var fy:Number = getForceY(- (myRetardingTorque + myEngineTorque));
        if (Math.abs(fy) > maxFriction) {
            var oldContactVy:Number = getContactVY();
            myPinAngularVelocity += myPinMomentOfForce * dt / myPinMomentOfInertia;
            if (oldContactVy * getContactVY() < 0) {
                definePinAngularVelocity();
            }
        }
    }

    private function definePinAngularVelocity():void {
        myPinAngularVelocityProperty = myVelocityProperty.getScalarProjectionByAngle(myGlobalFrameProperty.myAngle + Math.PI / 2) / myRadiusProperty;
    }

    public function calcForces():void {
        if (myRoadGrip) {
            //вообще-то так должно быть для всех случаев, кроме
            //самого жесткого проскальзывания...
            //getPinRotating().myMomentOfForce = 0;
            myPinMomentOfForce = 0;
        } else {
            var roadForce:Number = getSumRoadForceWhenSliding().getScalarProjectionByAngle(myFrame.myAngle + Math.PI / 2);
            myPinMomentOfForce = myEngineTorqueProperty + myRetardingTorqueProperty + getRoadMoment(roadForce);
        }
        myForceProperty = ( myRoadGrip )
                ? getForceYWhenRoadGrip(myForceProperty)
                : getSumRoadForceWhenSliding();
    }

    public function getForceYWhenRoadGrip(vector:Vector2d):Vector2d {
        if (! myRoadGrip) {
            //throw new StateDataError();
        }
        var fy:Number = 0;
        if (myRetardingTorqueProperty + myEngineTorqueProperty != 0) {
            fy = (myRetardingTorqueProperty + myEngineTorqueProperty) / myRadiusProperty;
        }
        vector.myX = - fy * Math.sin(myGlobalFrameProperty.myAngle);
        vector.myY = fy * Math.cos(myGlobalFrameProperty.myAngle);
        return vector;
    }

    //используется в Chassis
    // - getRearForceWhenCrab
    // - checkForceForRails
    public function getSlidingForceYWhenRails(vector:Vector2d):Vector2d {
        var fy:Number = Utils.sign(myRetardingTorqueProperty + myEngineTorqueProperty)
                * myWeightProperty * mySlidingFrictionProperty;
        vector.myX = - fy * Math.sin(myGlobalFrameProperty.myAngle);
        vector.myY = fy * Math.cos(myGlobalFrameProperty.myAngle);
        return vector;
    }

    //используется в
    // - Chassis.getFX
    // - Wheel.getSumRoadForceWhenSliding
    private function getSlidingForceWhenBigMoment():Vector2d {
        if (myRoadGrip) {
            throw new StateDataError();
        }
        var v:Vector2d = getContactVelocity();
        //но ведь это же неправда!
        //здесь должно стоять условие, что момент от дороги больше остальных!
        if (v.isZero()) {
            return myFrame.getYOrt().multiply2d(
                    Utils.sign(myEngineTorqueProperty + myRetardingTorqueProperty)
                            * mySlidingFrictionProperty * myWeightProperty
                    );
        }
        return v.multiply2d(- mySlidingFriction * myWeight / v.length());
    }

    private function getTorqueForceY(): Number {
        return getForceY(- (myRetardingTorque + myEngineTorque));
    }

    public function getSumRoadForceWhenSliding():Vector2d {
        if (myRoadGrip) {
            throw new StateDataError();
        }
        var fy:Number = getTorqueForceY();
        var maxFriction:Number = myWeight * mySlidingFrictionProperty; //refactoring extract metod
        var pinVelocity:Number = getPinVelocity();
        //Зачем здесь стоят условия
        // - (Math.abs(pinVelocity) > MAX_GRIP_VELOCITY)
        // далее
        // - if ( Utils.nonZero(getContactVY()) ){
        //		return getSlidingForce();
        //	}
        // ???
        if ((Math.abs(fy) < maxFriction)// && (Math.abs(pinVelocity) > MAX_GRIP_VELOCITY)
                ) {
            /*
             * Fy * R = - (ne + nb);
             * Fx^2 + Fy^2 = ...
             * Fx *= - Utils.sign(pinVelocity);
             */
            //if ( Utils.nonZero(getContactVY()) ){
            //	return getSlidingForceWhenBigMoment();
            //}
            var sin:Number = Math.sin(myFrame.myAngle);
            var cos:Number = Math.cos(myFrame.myAngle);
            var fx:Number = Math.sqrt(maxFriction * maxFriction - fy * fy);
            fx *= - Utils.sign(pinVelocity);
            return new Vector2d(fx * cos - fy * sin,
                    fx * sin + fy * cos);
        } else {
            return getSlidingForceWhenBigMoment();
        }
    }

    public function get mySlidingForceLength(): Number {
        return myWeight * mySlidingFriction;
    }

    public function getMaxPinForceLength(): Number {
        var maxForce: Number;
        var fy: Number = getTorqueForceY();
        var maxFriction: Number = myWeight * ((myRoadGrip) ? myStaticFriction : mySlidingFriction);
        if (fy > maxFriction) {
            if (myRoadGrip) {
                throw new StateDataError();
            } else {
                maxForce = Math.abs(getSlidingForceWhenBigMoment().getScalarProjectionByAngle(myAngle));
            }
        } else {
            maxForce = Math.sqrt(maxFriction * maxFriction - fy * fy);
        }
        return maxForce;
    }

    public function getPinVelocity():Number {
        return  myVelocity.getScalarProjectionByAngle(myGlobalFrameProperty.myAngle);
    }

    public function getContactVY():Number {
        return myVelocity.getScalarProjectionByAngle(myGlobalFrameProperty.myAngle + Math.PI / 2)
                - myPinAngularVelocity * myRadius;
    }

    public function setVelosityPinZero():void {
        myVelocity.setProjectionToAngle(myFrame.myAngle + Math.PI / 2);
    }

    public function get myFrame():LocalFrame2d {
        return myGlobalFrameProperty;
    }

    public function get myPinAngularVelocity():Number {
        return myPinAngularVelocityProperty;
    }

    public function set myPinAngularVelocity(w:Number):void {
        myPinAngularVelocityProperty = w;
    }

    public function get myRadius():Number {
        return myRadiusProperty;
    }

    public function setWeight(weight:Number):void {
        if (weight >= 0) {
            myWeightProperty = weight;
        } else {
            myWeightProperty = 0;
        }
    }

    public function gripBecauseOfMoment():Boolean {
        return myStaticFrictionProperty * myWeightProperty >= Math.abs(getForceY(myRetardingTorqueProperty + myEngineTorqueProperty));
    }

    public function slidingBecauseOfNormalVelocity():Boolean {
        var sin:Number = Math.sin(myGlobalFrameProperty.myAngle);
        var cos:Number = Math.cos(myGlobalFrameProperty.myAngle);
        var pv:Number = myVelocityProperty.myX * cos + myVelocityProperty.myY * sin;
        return Math.abs(pv) > MAX_GRIP_VELOCITY;
    }

    public function setGrip():void {
        myRoadGripProperty = true;
        calcForces();
    }


    public function getContactVelocity():Vector2d {
        return new Vector2d(myPinAngularVelocity * myRadius * Math.sin(myFrame.myAngle) + myVelocity.myX,
                - myPinAngularVelocity * myRadius * Math.cos(myFrame.myAngle) + myVelocity.myY);
    }

    public function get myPinMomentOfInertia():Number {
        return myPinMomentOfInertiaProperty;
    }

    public function set myPinMomentOfInertia(w:Number):void {
        myPinMomentOfInertiaProperty = w;
    }

    public function get myPinMomentOfForce():Number {
        return myPinMomentOfForceProperty;
    }

    public function set myPinMomentOfForce(w:Number):void {
        myPinMomentOfForceProperty = w;
    }

    public function get myForce():Vector2d {
        return myForceProperty;
    }

    public function get myWeight(): Number {
        return myWeightProperty;
    }

}
}