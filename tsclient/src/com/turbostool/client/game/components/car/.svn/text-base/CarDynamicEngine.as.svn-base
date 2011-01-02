package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

public class CarDynamicEngine{
    private var mySuspensionProperty:Suspension;
    private var myBrakesProperty:Brakes;
    private var myDiffProperty:Differential;
    private var myTransmissonProperty:Transmission;
    private var myChassisProperty:Chassis;
    private var myHullProperty:Hull;
    private var myRudderProperty:RudderControl;
    private var myUseABSProperty:Boolean = true;
    private var myUseESPProperty:Boolean = true;
    private var myABS:ABS;
    private var myESP:ESP;
    private var myEngineControlProperty:EngineControl;
    private var myClutchProperty: Boolean = true;

    public function CarDynamicEngine(width:Number, length:Number) {
        myHullProperty = new Hull(780, 300, width, length);
        myChassisProperty = new Chassis(myHullProperty, width, length, 0);
        mySuspensionProperty = new Suspension(myChassis, myHull);
        var engine:CarEngine = new CarEngine();
        myTransmissonProperty = new Transmission(engine,
                new Array(4.5, -5, -4, -3, -2),
                5//base gear
                );
        myDiffProperty = new Differential(myChassisProperty, 0.5);
        myRudderProperty = new RudderControl(myChassis, Math.PI / 6, 4 * Math.PI / 3);
        myRudderProperty.myAction = RudderControl.NONE;
        myBrakesProperty = new Brakes(myChassis, 900);
        myChassis.calcForces();
        mySuspension.calcForces();
        mySuspension.nullForces();
        myESP = new ESP(myChassis.getWheels());
        myABS = new ABS(myChassis.getWheels());
        myEngineControlProperty = new EngineControl(engine);
    }

    public function calcForces():void {

        myBrakes.calcForces();
        if (myUseABS) {
            myABS.correctBrakeTorque();
        }

        if (myClutch) {
            myTransmission.setEngineVelocity(myDiff.getAverageAngularVelocity());
            myTransmission.checkGear();
            myDiff.setTorque(myTransmission.getRollerTorque());
            if (myUseESP) {
                myESP.correctEngineTorque();
            }
        }

        //А где считаются поперечные силы от колес в случае неЗаноса? ///////////////////////////////
        //и момент
        myChassis.calcForces(); //ATTENTION: ORDER IS IMPORTANT!
        //
        //myHull.calcForces();
        mySuspension.calcForces();

    }

    public function calcVelocity(dt:Number):void {
        myChassis.calcVelocity(dt);
        mySuspension.calcVelocity(dt);

        myChassis.myFrontLeft.calcVelocity(dt);
        myChassis.myFrontRight.calcVelocity(dt);
        myChassis.myRearLeft.calcVelocity(dt);
        myChassis.myRearRight.calcVelocity(dt);

        myRudder.calcVelocity(dt);
        myEngineControlProperty.calcVelocity(dt);
        myBrakes.calcVelocity(dt);
        if (!myClutch) myCarEngine.step(dt);
    }

    public function saveState():void {
        myHull.saveState();
        mySuspension.saveState();
        myChassis.saveState();
        //engine
    }

    public function rollback():void {
        myChassis.rollback();
        myHull.rollback();
        mySuspension.rollback();
        //engine
    }

    public function step(dt:Number):void {
    }

    public function calcCoordinates(dt:Number):void {
        myRudder.calcCoordinates(dt);
        myHull.calcCoordinates(dt);
        mySuspension.calcCoordinates(dt);
        myEngineControlProperty.calcCoordinates(dt);
        myHull.updateBorder();
        myHull.updatePolygons();
        myBrakes.calcCoordinates(dt);
    }

    public function nullForces():void {
        myHull.nullForces();
        mySuspension.nullForces();
    }

    public function get mySuspension():Suspension {
        return mySuspensionProperty;
    }

    public function get myBrakes():Brakes {
        return myBrakesProperty;
    }

    public function get myDiff():Differential {
        return myDiffProperty;
    }

    public function get myTransmission():Transmission {
        return myTransmissonProperty;
    }

    public function get myChassis():Chassis {
        return myChassisProperty;
    }

    [Bindable]
    public function get myCarEngine():CarEngine {
        return myTransmission.getEngine();
    }

    public function set myCarEngine(value: CarEngine): void
    {
        return myTransmission.setEngine(value);
    }

    public function get myHull():Hull {
        return myHullProperty;
    }

    public function get myRudder():RudderControl {
        return myRudderProperty;
    }

    public function get myEngineControl():EngineControl {
        return myEngineControlProperty;
    }

    public function get myUseABS(): Boolean {
        return myUseABSProperty;
    }

    public function get myUseESP(): Boolean {
        return myUseESPProperty;
    }

    public function set myUseABS(useABS:Boolean): void {
        myUseABSProperty = useABS;
    }

    public function set myUseESP(useESP:Boolean): void {
        myUseESPProperty = useESP;
    }

    public function get myClutch(): Boolean {
        return myClutchProperty;
    }

    public function set myClutch(clutch:Boolean): void {
        if (myClutchProperty && !clutch) myDiff.setTorque(0);
        if (!myClutchProperty && clutch) {
            if (myHull.myVelocity.isZero()) {
                var eKin: Number = myCarEngine.myMomentOfInertia * Utils.sqr(myCarEngine.myAngularVelocity);
                eKin *= 0.5;
                var dV: Number = Math.sqrt(eKin / myHull.myMass);
                //trace(dV);
                myHull.myVelocity.add2d(myHull.myFrame.getGlobalNonLength(new Vector2d(0, -dV)));
            }
        }
        myClutchProperty = clutch;
    }

}
}