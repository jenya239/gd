package com.turbostool.client.game.components.car {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.utils.Assert;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;

/**
 * Хранит в жесткой связи позиции колес
 */
public class Chassis{
    private static const FRONT: int = 0;
    private static const REAR: int = 1;

    private static const MAX_NORM_VELOCITY:Number = 1;
    public static const WHEEL_COUNT:uint = 4;

    public static const FRONT_RIGHT:uint = 0;
    public static const REAR_RIGHT:uint = 1;
    public static const REAR_LEFT:uint = 2;
    public static const FRONT_LEFT:uint = 3;

    private var myWheels:Array;
    private var myHull:Hull;
    private var myMassCenterLongShiftProperty:Number;//растояние от центра масс авто до геометрического центра (+) это центр масс спереди

    private var mySavedRudderAngle:Number;
    private var myRudderAngleProperty:Number;
    private var myWheelsLength:Number;
    private var myOldVTang:Number;
    private var myOldNormVelocity:Number;
    private var myOldRudderSign:int;
    private var myOldVxMinusLW: Number;
    private var myTempFrame:LocalFrame2d;

    private var myRearWidth: Number;
    private var myFrontWidth: Number;


    /* изменить
     одно из передних колес может быть сцеплено?
     */

    /* добавить
     isRails - если все сцеплены
     is
     */

    private function createWheel(r:Vector2d):Wheel {
        var wheel:Wheel;
        wheel = new Wheel();
        wheel.myFrame.myR = r;
        return wheel;
    }

    public function Chassis(hull:Hull, wheelsWidth:Number, wheelsLength:Number, massShift:Number = 0) {
        myHull = hull;
        myRudderAngleProperty = 0;
        myMassCenterLongShiftProperty = massShift;
        myWheelsLength = wheelsLength;
        myRearWidth = wheelsWidth;
        myFrontWidth = wheelsWidth;
        myWheels = new Array();
        for (var i:int = 0; i < WHEEL_COUNT; i++) {
            myWheels[i] = new Wheel();
        }
        updateWheelFrames();
        myOldNormVelocity = 0;
        myOldVTang = 0;
        myOldRudderSign = 0;
        myOldVxMinusLW = 0;
        mySavedRudderAngle = 0;
        myHull.setChassis(this);
        myTempFrame = new LocalFrame2d(Vector2d.getZero());
    }

    public function get myRearLeft():Wheel {
        return myWheels[REAR_LEFT];
    }

    public function get myRearRight():Wheel {
        return myWheels[REAR_RIGHT];
    }

    public function get myFrontRight():Wheel {
        return myWheels[FRONT_RIGHT];
    }

    public function get myFrontLeft():Wheel {
        return myWheels[FRONT_LEFT];
    }

    public function getWheels():Collection {
        var array:ArrayList = new ArrayList();
        for (var i:int = 0; i < WHEEL_COUNT; i++) {
            array.addItem(myWheels[i]);
        }
        return array;
    }

    /**
     * Тот кто в Hull меняет globalFrame должен вызывать этот метод
     */
    public function updateWheelFrames():void {
        var alpha:Number = myHull.myFrame.myAngle;
        var sin:Number = Math.sin(alpha);
        var cos:Number = Math.cos(alpha);
        var rx:Number = myHull.myFrame.myR.myX - myMassCenterLongShift * sin; //здесь myFrame - центр масс
        var ry:Number = myHull.myFrame.myR.myY + myMassCenterLongShift * cos;
        //var w2:Number = myWidth / 2;
        var wr2:Number = myRearWidth / 2;
        var wf2:Number = myFrontWidth / 2;
        var l_2:Number = myLength / 2;

        var frame:LocalFrame2d;

        frame = (myWheels[FRONT_RIGHT] as Wheel).myFrame;
        frame.myR.myX = rx + wf2 * cos + l_2 * sin;
        frame.myR.myY = ry + wf2 * sin - l_2 * cos;
        frame = (myWheels[FRONT_LEFT] as Wheel).myFrame;
        frame.myR.myX = rx - wf2 * cos + l_2 * sin;
        frame.myR.myY = ry - wf2 * sin - l_2 * cos;
        updateFrontWheelAngles();

        frame = (myWheels[REAR_LEFT] as Wheel).myFrame;
        frame.myR.myX = rx - wr2 * cos - l_2 * sin;
        frame.myR.myY = ry - wr2 * sin + l_2 * cos;
        frame.myAngle = alpha;

        frame = (myWheels[REAR_RIGHT] as Wheel).myFrame;
        frame.myR.myX = rx + wr2 * cos - l_2 * sin;
        frame.myR.myY = ry + wr2 * sin + l_2 * cos;
        frame.myAngle = alpha;
    }

    //need optimization
    public function updateWheelVelocity():void {
        var w:Number = myHull.myAngularVelocity;
        var massCenter:Vector2d = myHull.myFrame.myR;
        for (var i:int = 0; i < WHEEL_COUNT; i++) {
            var wheel:Wheel = myWheels[i];
            wheel.myVelocity.setXY(- (wheel.myR.myY - massCenter.myY ) * w, (wheel.myR.myX - massCenter.myX ) * w);
            wheel.myVelocity = wheel.myVelocity.add2d(myHull.myVelocity);
        }
    }


    public function get myRudderAngle():Number {
        return myRudderAngleProperty;
    }

    private function updateFrontWheelAngles(): void {
        var ctan: Number = 1 / Math.tan(myRudderAngle);
        var a0: Number = Math.atan2(myLength, myLength * ctan - myFrontPinWidth / 2);
        var a3: Number = Math.atan2(myLength, myLength * ctan + myFrontPinWidth / 2);
        if (myRudderAngle < 0) {
            a0 -= Math.PI;
            a3 -= Math.PI;
        }
        myFrontLeft.myFrame.myAngle = myHull.myFrame.myAngle + a3;
        myFrontRight.myFrame.myAngle = myHull.myFrame.myAngle + a0;
    }

    public function set myRudderAngle(setValue:Number):void {
        myRudderAngleProperty = setValue;
        updateFrontWheelAngles();
    }

    private function getTangVector2d(v:Vector2d):Vector2d {
        //var temp:Vector2d = new Vector2d(0,0);
        if (isRudderRotated()) {

            return Vector2d.createFrom3d(
                    v.getOrtogComponentBy(getTurnVector())
                    );

            /*v.copyTo2d(temp);
             return temp.setProjectionToAngle(getTurnVectorAngle()+ Math.PI/2);*/
        }
        /*
         return Vector2d.createFrom3d(
         v.projection(myHull.myFrame.getYOrt())
         );
         */
        var temp:Vector2d = new Vector2d(0, 0);
        v.copyTo2d(temp);
        return temp.setProjectionToAngle(myHull.myFrame.myAngle + Math.PI / 2);
    }

    public function getTangForce():Vector2d {
        return getTangVector2d(myHull.myForce); //будут баги см в HUll
    }

    public function getNormForce():Vector2d {
        return Vector2d.createFrom3d(
                myHull.myForce.difference(getTangForce())
                );
    }

    public function getTangVelocity():Vector2d {
        return getTangVector2d(myHull.myVelocity);
    }

    public function calcCoordinates(dt:Number):void {
        if (isCrab()) {
            var l: Number = myLength / 2 + myMassCenterLongShift;
            var vx: Number = myHull.myVelocity.getScalarProjectionByAngle(myHullFrame.myAngle);
            //myHull.myAngularVelocity = vx / l; //  CRAB_TEMP
            var debugSpeed: Number = myRearLeft.getPinVelocity();
        }
        updateWheelFrames();
        updateWheelVelocity();
    }

    public function nullForces():void {
    }

    public function isRails(): Boolean {
        return (
                ( myFrontLeft.myRoadGrip || myFrontRight.myRoadGrip )
                        && ( myRearLeft.myRoadGrip || myRearRight.myRoadGrip )
                );
    }

    private function isCrab(): Boolean {
        return (
                ! myFrontLeft.myRoadGrip
                        && ! myFrontRight.myRoadGrip
                        && myRearLeft.myRoadGrip
                        && myRearRight.myRoadGrip );
    }

    private function isFullSliding(): Boolean {
        return !isRails() && !isCrab();
    }

    private function setSlidingIfMoment(): void {
        var i: int;
        var wheel:Wheel;
        for (i = 0; i < WHEEL_COUNT; i++) {
            wheel = myWheels[i] as Wheel;
            if (wheel.myRoadGrip && !wheel.gripBecauseOfMoment()) {
                wheel.setSliding();
            }
        }
    }

    private function setSlidingIfBigNormalVelocity(): void {
        var i: int;
        var wheel:Wheel;
        for (i = 0; i < WHEEL_COUNT; i++) {
            wheel = myWheels[i] as Wheel;
            if (wheel.myRoadGrip && wheel.slidingBecauseOfNormalVelocity()) {
                wheel.setSliding();
            }
        }
    }

    private function calcPinForcesWhenRails(): Array {
        Assert.assertTrue(isRails());
        if (!isRudderRotated()) {
            return [0, 0];
        }
        var a: Number = myHull.myMass * myHull.myVelocity.sqrLength() / getTurnVector().length();
        var b: Number = myHull.myMomentOfInertia * getAngularAcceleration();
        var l: Number = myLength / 2 + myMassCenterLongShift;
        var l1: Number = myLength - l;
        var cos: Number = Math.cos(myRudderAngleProperty);
        var frontForce: Number = (b + a * l) / (cos * l1 + l);
        var rearForce: Number = (a * l1 * cos - b) / (cos * l1 + l);
        return [frontForce, rearForce];
    }

    private function calcMaxPinForces(): Array {
        var maxFrontForce: Number;
        var maxRearForce: Number;
        maxFrontForce = myFrontLeft.getMaxPinForceLength() + myFrontRight.getMaxPinForceLength();
        maxRearForce = myRearLeft.getMaxPinForceLength() + myRearRight.getMaxPinForceLength();
        return [maxFrontForce, maxRearForce];
    }

    private function getRearForceWhenCrab(): Number {
        myFrontLeft.calcForces();
        myFrontRight.calcForces();
        var f03x: Number = myFrontLeft.myForce.sum2d(myFrontRight.myForce).getScalarProjectionByAngle(myHullFrame.myAngle);
        //На самом деле в f03x должны входить все известные боковые силы, действующие на автомобиль
        var I: Number = myHull.myMomentOfInertia;
        var l: Number = myLength / 2 + myMassCenterLongShift;
        var m: Number = myHull.myMass;
        var k: Number = I / (I + m * Utils.sqr(l));
        var knownMoment: Number = 0;
        for (var i:int = 0; i < WHEEL_COUNT; i++) {
            var wheel:Wheel = myWheels[i] as Wheel;
            var force: Vector2d = new Vector2d(0, 0);
            if (wheel.gripBecauseOfMoment()) {
                force = wheel.getForceYWhenRoadGrip(force);
            } else {
                force = wheel.getSlidingForceYWhenRails(force);
            }
            knownMoment += wheel.myR.difference2d(myHullFrame.myR).vectorProductZ(force);
        }
        //var f12x: Number = k * ((m * l / I) * knownMoment - f03x); //<-- кажется здесь ошибка!
        //поправим формулу

        var w: Number = myHull.myAngularVelocity;
        var vy: Number = myHull.myVelocity.getScalarProjectionByAngle(myHullFrame.myAngle + Math.PI / 2);
        var f12x_new: Number = k * ((m * l / I) * knownMoment - m * vy * w - f03x);

        //trace("f12x     = "+f12x);
        //trace("f12x_new = "+f12x_new);

        return f12x_new;
    }

    private function checkRearSlidingWhenCrab(): void {
        Assert.assertTrue(isCrab());
        //вычисляем rearMaxForceX
        var rearMaxForceX: Number = myRearLeft.getMaxPinForceLength() + myRearRight.getMaxPinForceLength();
        //если getRearForceWhenCrab() больше rearMaxForceX
        if (getRearForceWhenCrab() > rearMaxForceX) {
            //устанавливаем и задним колесам проскальзывание
            myRearLeft.setSliding();
            myRearRight.setSliding();
        }
    }

    private function railsByVelocityOscillation(): Boolean {
        var curTang:Number = (isRudderRotated()) ? getVTang() //такая условная херня для проверок
                : myHull.myAngularVelocity; // <-- это ошибка? - нет!
        //: myConstraint.myVelocity.scalarProjection(myConstraint.myFrame.getYOrt()); <-- это ошибка
        var curNorm:Number = (isRudderRotated()) ? getNormVelocity()
                : myHull.myVelocity.getScalarProjectionByAngle(myHull.myFrame.myAngle);
        var rails: Boolean = false;
        if (true
                && !isRails()
                && (getRudderSign() == myOldRudderSign)
                && (Utils.nonZero(myOldVTang) || Utils.nonZero(myOldNormVelocity))
                && (Utils.sign(myOldVTang) * Utils.sign(curTang) < 0)
            //&& (Utils.sign(myOldNormVelocity) * Utils.sign(curNorm) <= 0)
                && ( ( Math.abs(curNorm) < MAX_NORM_VELOCITY )
                || ( curNorm * myOldNormVelocity < 0 ) )
                ) {
            rails = true;
            myOldVTang = 0;
            myOldNormVelocity = 0;
        }
        myOldRudderSign = getRudderSign();
        if (!isRails()) {
            myOldVTang = curTang;
            myOldNormVelocity = curNorm;
        }
        return rails;
    }

    private function crabByVelocityOscillation(): Boolean {
        //если сейчас fullSliding то проверим не начался ли снос
        var vx: Number = myHull.myVelocity.getScalarProjectionByAngle(myHullFrame.myAngle);
        var l: Number = myLength / 2 + myMassCenterLongShift;
        var curDiff: Number = vx - l * myHull.myAngularVelocity;
        if (myOldVxMinusLW * curDiff < 0) {
            myOldVxMinusLW = curDiff;
            return true;
        } else {
            //BUG? разве это не в calcVelocity должно быть?
            myOldVxMinusLW = curDiff;
        }
        return false;
    }

    //calcForces в колесе опирается на собственное roadGrip и
    //вычисляет силу со стороны дороги по установленным engine и break torque, то
    //есть с шасси это связано только через roadGrip
    //Еще надо придумать
    // - как разбить это на методы
    // - написать тесты
    // - трейсить, дебажить и визуализировать
    //в общем и целом идея какая
    //проверяем каждый раз возможно ли состояние меньшим числом степеней свободы, если да - то все
    //иначе возможно ли состояние это оставить, если да - то все
    //иначе можно ли лишь чуть уменьшить число степеней свободы,
    //или вообще полное проскальзывание
    //ведь переход между состояниями должен произойти только один, да?
    //вообще, если по осцилляциям - переход, а по силам не получается - это всегда подозрительно
    public function newUpdateGripConfiguration():void {
        var savedFrontRightGrip: Boolean;
        var savedRearRightGrip: Boolean;
        var savedRearLeftGrip: Boolean;
        var savedFrontLeftGrip: Boolean;

        function saveGripConfiguration(): void {
            savedFrontRightGrip = myFrontRight.myRoadGrip;
            savedRearRightGrip = myRearRight.myRoadGrip;
            savedRearLeftGrip = myRearLeft.myRoadGrip;
            savedFrontLeftGrip = myFrontLeft.myRoadGrip;
        }

        function revertGripConfiguration(): void {
            if (savedFrontRightGrip) {
                myFrontRight.setGrip();
            } else {
                myFrontRight.setSliding();
            }
            if (savedRearRightGrip) {
                myRearRight.setGrip();
            } else {
                myRearRight.setSliding();
            }
            if (savedRearLeftGrip) {
                myRearLeft.setGrip();
            } else {
                myRearLeft.setSliding();
            }
            if (savedFrontLeftGrip) {
                myFrontLeft.setGrip();
            } else {
                myFrontLeft.setSliding();
            }
        }

        function trySetRailsWhenVelocityOscillation(): void {
            saveGripConfiguration();
            //всем колесам,
            for (i = 0; i < WHEEL_COUNT; i++) {
                wheel = myWheels[i] as Wheel;
                //у которых позволяет момент
                if (wheel.gripBecauseOfMoment()) {
                    //ставим сцепление
                    wheel.setGrip();
                }
            }
            //если получились рельсы
            if (isRails()) {
                //вычисляем силы, действующие на оси
                var pinForces: Array = calcPinForcesWhenRails();
                //вычисляем максимально допустимые силы, действующие на оси
                var maxPinForces: Array = calcMaxPinForces();
                //если хотя бы одну из осей сносит
                if ((pinForces[FRONT] > maxPinForces[FRONT]) || (pinForces[REAR] > maxPinForces[REAR])) {
                    //тем колесам у которых мы изменили сцепление - откатываем изменение
                    revertGripConfiguration();
                }
            } else {
                //тем колесам у которых мы изменили сцепление - откатываем изменение
                revertGripConfiguration();
            }
        }

        var i: int;
        var wheel:Wheel;
        //для всех колес
        for (i = 0; i < WHEEL_COUNT; i++) {
            wheel = myWheels[i] as Wheel;
            //если момент и нормальная скорость
            if (wheel.gripBecauseOfMoment() && !wheel.slidingBecauseOfNormalVelocity()) {
                //делаем сцепление
                wheel.setGrip();
            }
        }
        //а точно теперь "свич" надо делать по получившимся сцеплениям?
        if (isRails()) {
            //по всем колесам проверяем, а не проскальзывание ли в результате момента
            setSlidingIfMoment();
            //если остались рельсы
            if (isRails()) {
                //вычисляем силы, действующие на оси
                var pinForces: Array = calcPinForcesWhenRails();
                //вычисляем максимально допустимые силы, действующие на оси
                var maxPinForces: Array = calcMaxPinForces();
                //если снесло заднюю ось
                if (pinForces[REAR] > maxPinForces[REAR]) {
                    //делаем задним колесам проскальзывание
                    myRearLeft.setSliding();
                    myRearRight.setSliding();
                    //и передним для которых большая нормальная скорость - делаем проскальзывание
                    if (myFrontLeft.slidingBecauseOfNormalVelocity()) myFrontLeft.setSliding();
                    if (myFrontRight.slidingBecauseOfNormalVelocity()) myFrontRight.setSliding();
                }
                //если снесло переднюю ось
                if (pinForces[FRONT] > maxPinForces[FRONT]) {
                    //делаем передним колесам проскальзывание
                    myFrontLeft.setSliding();
                    myFrontRight.setSliding();
                    //если в результате снос
                    if (isCrab()) {
                        checkRearSlidingWhenCrab();
                    }
                }
                //иначе, если стал снос
            } else if (isCrab()) {
                checkRearSlidingWhenCrab();
            } else {
                //всем колесам у которых проскальзывание в результате норм. скорости ставим sliding
                setSlidingIfBigNormalVelocity();
            }
        } else if (isCrab()) {
            //по всем колесам проверяем, а не проскальзывание ли в результате момента
            setSlidingIfMoment();
            setSlidingIfBigNormalVelocity(); //если бы в сносе все было правильно этой бы строчки здесь не было
            //если остался снос
            if (isCrab()) {
                //если по осцилляциям - рельсы
                if (railsByVelocityOscillation()) {
                    trySetRailsWhenVelocityOscillation();
                } else {
                    checkRearSlidingWhenCrab();
                }
            }
            //вот этого else сейчас здесь нет, так как нет соответсвия между
            //просчетом сил и скоротсей для сноса
            //else{
            //всем колесам у которых проскальзывание в результате норм. скорости ставим sliding
            //setSlidingIfBigNormalVelocity();
            //}
        } else {
            setSlidingIfMoment();//логично, чтобы здесь была эта строчка.
            //НО судя по всему было соображение, из-за которого ее не было. Не могу вспомнить
            if (railsByVelocityOscillation()) {
                trySetRailsWhenVelocityOscillation();
            }
            //если рельсы не получились и по осцилляциям - снос
            if (isFullSliding() && crabByVelocityOscillation()) {
                saveGripConfiguration();
                //для задних колес, если возможно по моменту устанавливаем сцепление
                if (myRearLeft.gripBecauseOfMoment()) myRearLeft.setGrip();
                if (myRearRight.gripBecauseOfMoment()) myRearRight.setGrip();
                //для передних - насильно делаем проскальзывание
                myFrontLeft.setSliding();
                myFrontRight.setSliding();
                //если в результате снос
                if (isCrab()) {
                    //вычисляем rearMaxForceX
                    var rearMaxForceX: Number = myRearLeft.getMaxPinForceLength() + myRearRight.getMaxPinForceLength();
                    //если getRearForceWhenCrab() больше rearMaxForceX
                    if (getRearForceWhenCrab() > rearMaxForceX) {
                        revertGripConfiguration();
                    }
                } else {
                    //иначе - откатываем измененные сцепления
                    revertGripConfiguration();
                }
            }
            if (isFullSliding()) {
                //всем колесам у которых проскальзывание в результате норм. скорости ставим sliding
                setSlidingIfBigNormalVelocity();
            }
        }
    }

    public function calcForces():void {
        //updateGripConfiguration();
        newUpdateGripConfiguration();
        var i: int;
        var wheel:Wheel;
        for (i = 0; i < WHEEL_COUNT; i++) {
            wheel = myWheels[i] as Wheel;
            wheel.tryDefinePinAngular();
            wheel.calcForces();
        }

        if (isCrab()) {
            var l: Number = myLength / 2 + myMassCenterLongShift;
            var f12x: Number = getRearForceWhenCrab();
            var addForce: Vector2d = (new Vector2d(f12x, 0)).rotate(myHullFrame.myAngle);
            myHull.myForce.add2d(addForce);
            myRearLeft.myForce.addMultiplied(addForce, 1 / 2); //for debug?
            myRearRight.myForce.addMultiplied(addForce, 1 / 2); //for debug?
            //myHull.myMomentOfForce += f12x * l;
        }
        //considerWheelForce должен быть сейчас после if(  isCrab() ){,
        //поскольку не работает myHull.myAngularVelocity = vx / l;
        //хотя с другой стороны ведь момент и так учитывался
        //в закоменченной строчке myHull.myMomentOfForce += f12x * l;
        //но силы видимо не учитывались...
        considerWheelForce(myRearLeft);
        considerWheelForce(myRearRight);
        considerWheelForce(myFrontLeft);
        considerWheelForce(myFrontRight);
    }

    private function considerWheelForce(wheel:Wheel):void {
        myHull.myForce.add2d(wheel.myForce);
        var myX:Number = wheel.myFrame.myR.myX - myHullFrame.myR.myX;
        var myY:Number = wheel.myFrame.myR.myY - myHullFrame.myR.myY;
        myHull.myMomentOfForce += myX * wheel.myForce.myY - wheel.myForce.myX * myY;
    }

    public function calcVelocity(dt:Number):void {
        myOldRudderSign = getRudderSign();
        myOldVTang = (isRudderRotated()) ? getVTang() : myHull.myAngularVelocity;
        myOldNormVelocity = (isRudderRotated()) ? getNormVelocity() : myHull.myVelocity.getScalarProjectionByAngle(myHull.myFrame.myAngle);

        var previousVTang: Number = myHull.myVelocity.scalarProduct(myHullFrame.getYOrt());
        if (isRails()) { //когда есть рельсы
            var aTau:Vector2d = getTangForce().multiply2d(1 / myHull.myMass);
            myHull.myVelocity.addMultiplied(aTau, dt);
            myHull.myVelocity = getTangVelocity();
            if (isRudderRotated()) {
                var turnVector:Vector2d = getTurnVector();
                Assert.assertFalse(turnVector.isZero());
                myHull.myAngularVelocity = Utils.isZero(turnVector.myX)
                        ? - myHull.myVelocity.myX / turnVector.myY
                        : myHull.myVelocity.myY / turnVector.myX; //требуется тест
            } else {
                myHull.myAngularVelocity = 0;
            }
        } else {
            var vx: Number = myHull.myVelocity.getScalarProjectionByAngle(myHullFrame.myAngle);
            var l: Number = myLength / 2 + myMassCenterLongShift;
            var curDiff: Number = vx - l * myHull.myAngularVelocity;
            myHull.calcVelocity(dt);
            /* myOldVxMinusLW * curDiff < 0
             || ( myRearLeft.myRoadGrip && myRearRight.myRoadGrip )*/
            if (isCrab()) {
                //myHull.myAngularVelocity = vx / l;
                curDiff = 0;
                var debugSpeed: Number = myRearLeft.getPinVelocity();
            }
            myOldVxMinusLW = curDiff;
            //не учитывается поперечная сила, действующая со стороны дороги
            //на колесо в состоянии сцепления с дорогой. И, соответсвенно, момент.
        }
        var currVTang: Number = myHull.myVelocity.scalarProduct(myHullFrame.getYOrt());
        //trace(currVTang);
        //if( previousVTang * currVTang < 0 ){
        //	fullStop();
        //	return;
        //}
        updateWheelVelocity();
    }

    public function saveState():void {
        mySavedRudderAngle = myRudderAngle;
    }

    public function rollback():void {
        myRudderAngle = mySavedRudderAngle;
    }

    public function step(dt:Number):void {
    }

    public function isRudderRotated():Boolean {
        //return Utils.nonZero(myRudderAngle);
        return myRudderAngleProperty < -Utils.EPSILON || myRudderAngleProperty > Utils.EPSILON;
    }

    /**
     * Вектор из центра поворота к ц.м. авто
     */
    public function getTurnVector():Vector2d {
        if (!isRudderRotated()) {
            throw new TSError('Колеса расположены прямо! Радиус поворота бесконечен!');
        }
        var v:Vector2d = new Vector2d(- myLength / Math.tan(myRudderAngle), - myLength / 2 - myMassCenterLongShift);
        return myHull.myFrame.getGlobalNonLength(v);
    }

    public function getTurnVectorAngle():Number {
        return myRearLeft.myAngle +
               Math.atan(Math.tan(myRudderAngle + myHullFrame.myAngle - myRearRight.myAngle) * (0.5 + myMassCenterLongShiftProperty / myLength)) - Math.PI;
    }

    public function getTotalPinForce():Number {
        var rc:Vector2d = getTurnVector();
        return - myHull.myMass * getTangVelocity().sqrLength() / rc.length()
                - myHull.myForce.scalarProjection(rc);
    }

    public function getAngularAcceleration():Number {
        Assert.assertTrue(isRudderRotated());
        var rc:Vector2d = getTurnVector();
        var ft:Number = myHull.myForce.getScalarOrtogonal(rc);//здесь будет баг=)
        return ft / (myHull.myMass * rc.length());
    }

    public function getVTang():Number {
        if (!isRudderRotated()) {
            //throw new TSError('Знак тангенциальной состовляющей неоднозначен');
            return myHull.myVelocity.scalarProduct(myHullFrame.getYOrt());
        }
        var tv:Vector2d = getTurnVector();
        //var ortog:Vector2d = tv.ortogonal2d();
        var ortog:Vector2d = tv.rotate(Math.PI);
        return myHull.myAngularVelocity * tv.length()
                - myHull.myVelocity.scalarProjection(ortog);
    }

    public function getNormVelocity():Number {
        if (myHull.myVelocity.isZero()) {
            return 0;
        }
        if (isRudderRotated()) {
            return myHull.myVelocity.getScalarProjectionByAngle(getTurnVectorAngle());
        } else {
            return myHull.myVelocity.length() * Math.cos(myHull.myFrame.myAngle - myHull.myVelocity.getAngle());
        }

    }

    public function setTurningVelocity():void {
        var v:Vector2d = getTangVelocity();
        myHull.myVelocity = v;
        if (isRudderRotated()) {
            var tv:Vector2d = getTurnVector();
            myHull.myAngularVelocity = v.getScalarOrtogonal(tv) / tv.length();
        } else {
            myHull.myAngularVelocity = 0;
        }
    }

    private function getRudderSign():int {
        return Utils.sign(myRudderAngle);
    }

    private function setRails():void {
        for (var i:int = 0; i < WHEEL_COUNT; i++) {
            var wheel:Wheel = myWheels[i];
            if (wheel.gripBecauseOfMoment()) {
                wheel.setGrip();
            }
        }
        setTurningVelocity();
    }

    public function getTotalPinForceGlobal():Vector2d {
        if (isRudderRotated()) {
            return getTurnVector().setLength(getTotalPinForce());
        }
        return getNormForce().multiply2d(-1);
    }

    /**
     * Проверка в заносе не пора ли выйти из него. С историей
     * а почему сначала идет проверка на снос, а не на рельсы?
     */
    public function checkTurning():void {
        //если сейчас fullSliding то проверим не начался ли снос
        var vx: Number = myHull.myVelocity.getScalarProjectionByAngle(myHullFrame.myAngle);
        var l: Number = myLength / 2 + myMassCenterLongShift;
        var curDiff: Number = vx - l * myHull.myAngularVelocity;
        if (( !myRearLeft.myRoadGrip || !myRearRight.myRoadGrip)
            //&& myRearLeft.gripBecauseOfMoment() && myRearRight.gripBecauseOfMoment()
                && myOldVxMinusLW * curDiff < 0) {
            myRearLeft.setGrip();
            myRearRight.setGrip();
            myOldVxMinusLW = curDiff;
            return;
        } else {
            myOldVxMinusLW = curDiff;
        }

        var curTang:Number = (isRudderRotated()) ? getVTang() //такая условная херня для проверок
                : myHull.myAngularVelocity; // <-- это ошибка? - нет!
        //: myConstraint.myVelocity.scalarProjection(myConstraint.myFrame.getYOrt()); <-- это ошибка
        var curNorm:Number = (isRudderRotated()) ? getNormVelocity()
                : myHull.myVelocity.getScalarProjectionByAngle(myHull.myFrame.myAngle);
        if (true
                && !isRails()
                && (getRudderSign() == myOldRudderSign)
                && (Utils.nonZero(myOldVTang) || Utils.nonZero(myOldNormVelocity))
                && (Utils.sign(myOldVTang) * Utils.sign(curTang) < 0)
            //&& (Utils.sign(myOldNormVelocity) * Utils.sign(curNorm) <= 0)
                && (Math.abs(curNorm) < MAX_NORM_VELOCITY)
                ) {
            setRails();
            myOldVTang = 0;
            myOldNormVelocity = 0;
        }
        myOldRudderSign = getRudderSign();
        if (!isRails()) {
            myOldVTang = curTang;
            myOldNormVelocity = curNorm;
        }
    }

    public function get myLength():Number {
        return myWheelsLength;
    }

    public function set myLength(value:Number):void {
        myWheelsLength = value;
        updateWheelFrames();
        updateWheelVelocity();
    }

    public function get myFrontPinWidth(): Number {
        return myFrontWidth;
    }

    public function get myRearPinWidth(): Number {
        return myRearWidth;
    }

    public function set myRearPinWidth(value:Number):void {
        myRearWidth = value;
        updateWheelFrames();
        updateWheelVelocity();
    }

    public function set myFrontPinWidth(value:Number):void {
        myFrontWidth = value;
        updateWheelFrames();
        updateWheelVelocity();
    }

    public function getHull():Hull {
        return myHull;
    }

    public function getWheelByIndex(index:int):Wheel {
        return myWheels[index] as Wheel;
    }

    public function get myHullFrame():LocalFrame2d {
        return myHull.myFrame;
    }

    public function get myMassCenterLongShift():Number {
        return myMassCenterLongShiftProperty;
    }

    public function set myMassCenterLongShift(center:Number):void {
        myMassCenterLongShiftProperty = center;
        updateWheelFrames();
        updateWheelVelocity();
    }

    public function get myStaticFriction():Number {
        return myRearLeft.myStaticFriction;
    }

    public function get mySlidingFriction():Number {
        return myRearLeft.mySlidingFriction;
    }

    public function set myStaticFriction(coef:Number):void {
        myRearLeft.myStaticFriction = coef;
        myRearRight.myStaticFriction = coef;
        myFrontLeft.myStaticFriction = coef;
        myFrontRight.myStaticFriction = coef;
    }

    public function set mySlidingFriction(coef:Number):void {
        myRearLeft.mySlidingFriction = coef;
        myRearRight.mySlidingFriction = coef;
        myFrontLeft.mySlidingFriction = coef;
        myFrontRight.mySlidingFriction = coef;
    }

    public function fullStop():void {
        myHull.myVelocity.setXY(0, 0);
        myHull.myAngularVelocity = 0;
        updateWheelVelocity();
    }

    public function getStateName(): String {
        if (isRails()) {
            return "рельсы";
        }
        if (isCrab()) {
            return "снос";
        }
        return "полное проскальзывание";
    }

}
}