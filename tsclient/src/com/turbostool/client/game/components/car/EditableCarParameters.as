package com.turbostool.client.game.components.car {
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;
import flash.events.*;
import mx.controls.Alert;

[Bindable]
public class EditableCarParameters extends EventDispatcher{

    private var myCar: Car;

    private function get myModel():CarModel {
        return myCar.carModel;
    }

    public function EditableCarParameters(car: Car) {
        myCar = car;
    }

    public function get myHullLength():Number
    {
        return myModel.getHull().getRectangle().myHeight;
    }

    public function set myHullLength(setValue:Number):void {
        myModel.getHull().getRectangle().myHeight = setValue;
        myModel.getHull().myLengthHalf = setValue / 2;
    }

    public function get myHullWidth():Number
    {
        return myModel.getHull().getRectangle().myWidth;
    }

    public function set myHullWidth(setValue:Number):void
    {
        myModel.getHull().getRectangle().myWidth = setValue;
        myModel.getHull().myWidthHalf = setValue / 2;
    }

    public function get myFrontPinWidth(): Number {
        return myModel.getChassis().myFrontPinWidth;
    }

    public function set myFrontPinWidth(setValue: Number): void {
        myModel.getChassis().myFrontPinWidth = setValue;
    }

    public function get myRearPinWidth(): Number {
        return myModel.getChassis().myRearPinWidth;
    }

    public function set myRearPinWidth(setValue: Number): void {
        myModel.getChassis().myRearPinWidth = setValue;
    }

    public function get myChassisLength(): Number {
        return myModel.getChassis().myLength;
    }

    public function set myChassisLength(setValue: Number): void {
        myModel.getChassis().myLength = setValue;
    }

    public function get myCarMass(): Number {
        return myModel.getHull().myMass;
    }

    public function set myCarMass(setValue: Number): void {
        myModel.getHull().myMass = setValue;
    }

    public function get myMomentOfInertia(): Number {
        return myModel.getHull().myMomentOfInertia;
    }

    public function set myMomentOfInertia(setValue: Number): void {
        myModel.getHull().myMomentOfInertia = setValue;
    }

    public function get myMinTurnRadius(): Number {
        return myModel.myMinTurnRadius;
    }

    public function set myMinTurnRadius(setValue: Number): void {
        myModel.myMinTurnRadius = setValue;
    }

    public function get myRearRelative(): Number {
        return myModel.myDifferential.myRearRelativeTorque;
    }

    public function set myRearRelative(setValue: Number): void {
        myModel.myDifferential.myRearRelativeTorque = setValue;
    }

    public function get myABS():Boolean {
        return myDynamicEngine.myUseABS;
    }

    public function set myABS(abs:Boolean): void {
        myDynamicEngine.myUseABS = abs;
    }

    public function get myESP():Boolean {
        return myDynamicEngine.myUseESP;
    }

    public function set myESP(esp:Boolean): void {
        myDynamicEngine.myUseESP = esp;
    }

    private function get myDynamicEngine():CarDynamicEngine {
        return myCar.carModel.myCarDynamicEngine;
    }

    private function angularVelocityToRevs(angVelocity:Number):Number {
        return angVelocity * 60 / (Math.PI * 2);
    }

    private function revsToAngularVelocity(revs:Number):Number {
        return revs * Math.PI * 2 / 60;
    }

    public function set myEngineParametersXML(plotXml:XML):void {
        myParameters.myEndMaxMomentVelocity = revsToAngularVelocity(parseFloat(new String(plotXml.attribute('endMaxMomentVelocity'))));
        myParameters.myEndTorque = parseFloat(new String(plotXml.attribute('endTorque')));
        myParameters.myMaxTorque = parseFloat(new String(plotXml.attribute('maxTorque')));
        myParameters.myMaxVelocity = revsToAngularVelocity(parseFloat(new String(plotXml.attribute('maxVelocity'))));
        myParameters.myStartMaxMomentVelocity = revsToAngularVelocity(parseFloat(new String(plotXml.attribute('startMaxMomentVelocity'))));
        myParameters.myStartTorque = parseFloat(new String(plotXml.attribute('startTorque')));
    }

    public function get myEngineParametersXML():XML {
        return new XML(
                '<engineParameters '
                        + 'endMaxMomentVelocity="' + angularVelocityToRevs(myParameters.myEndMaxMomentVelocity) + '" '
                        + 'endTorque="' + myParameters.myEndTorque + '" '
                        + 'maxTorque="' + myParameters.myMaxTorque + '" '
                        + 'maxVelocity="' + angularVelocityToRevs(myParameters.myMaxVelocity) + '" '
                        + 'startMaxMomentVelocity="' + angularVelocityToRevs(myParameters.myStartMaxMomentVelocity) + '" '
                        + 'startTorque="' + myParameters.myStartTorque + '" '
                        + ' />'
                );
    }

    public function get myMassCenterHeight(): Number {
        return myDynamicEngine.mySuspension.getHeight();
    }

    public function set myMassCenterHeight(height: Number):void {
        myDynamicEngine.mySuspension.setHeight(height);
    }

    /**
     * процент веса авто, приходящийся на переднюю ось
     */
    public function get myFrontAxleWeightPercent(): Number {
        var s: Number = myDynamicEngine.myChassis.myMassCenterLongShift;
        return (0.5 - s / myChassisLength) * 100;
    }

    /**
     * процент веса авто, приходящийся на переднюю ось
     */
    public function set myFrontAxleWeightPercent(percent: Number):void {
        myDynamicEngine.myChassis.myMassCenterLongShift = myChassisLength * (50 - percent) / 100;
        myDynamicEngine.mySuspension.updateMassCenter();
        //myDynamicEngine.myHull.
    }

    public function get myXMomentOfInertia():Number {
        return myDynamicEngine.mySuspension.myXMomentOfInertia;
    }

    public function set myXMomentOfInertia(moment:Number):void {
        myDynamicEngine.mySuspension.myXMomentOfInertia = moment;
    }

    public function get myYMomentOfInertia():Number {
        return myDynamicEngine.mySuspension.myYMomentOfInertia;
    }

    public function set myYMomentOfInertia(moment:Number):void {
        myDynamicEngine.mySuspension.myYMomentOfInertia = moment;
    }

    public function get myStiffness():Number {
        return myDynamicEngine.mySuspension.myStiffness;
    }

    public function set myStiffness(coef:Number):void {
        myDynamicEngine.mySuspension.myStiffness = coef;
    }

    public function get myXDamping():Number {
        return myDynamicEngine.mySuspension.myXDamping;
    }

    public function set myXDamping(coef:Number):void {
        myDynamicEngine.mySuspension.myXDamping = coef;
    }

    public function get myYDamping():Number {
        return myDynamicEngine.mySuspension.myYDamping;
    }

    public function set myYDamping(coef:Number):void {
        myDynamicEngine.mySuspension.myYDamping = coef;
    }

    public function get myHandMoment():Number {
        return myDynamicEngine.myBrakes.myHandMoment;
    }

    public function get myBrakeMoment():Number {
        return myDynamicEngine.myBrakes.myBrakeMoment;
    }

    public function set myHandMoment(moment:Number):void {
        myDynamicEngine.myBrakes.myHandMoment = moment;
    }

    public function set myBrakeMoment(moment:Number):void {
        myDynamicEngine.myBrakes.myBrakeMoment = moment;
    }

    public function get myPressingCoef():Number {
        return myDynamicEngine.mySuspension.myHull.myPressingCoef;
    }

    public function get myAreaOfCut():Number {
        return myDynamicEngine.mySuspension.myHull.myAreaOfCut;
    }

    public function set myPressingCoef(c:Number):void {
        myDynamicEngine.mySuspension.myHull.myPressingCoef = c;
    }

    public function set myAreaOfCut(c:Number):void {
        myDynamicEngine.mySuspension.myHull.myAreaOfCut = c;
    }

    public function get myRudderAngVelocity():Number {
        return myDynamicEngine.myRudder.myBaseVelocity;
    }

    public function set myRudderAngVelocity(vel:Number):void {
        myDynamicEngine.myRudder.myBaseVelocity = vel;
    }

    public function get myRudderReturnVelocity():Number {
        return myDynamicEngine.myRudder.myBaseReturnVelocity;
    }

    public function set myRudderReturnVelocity(vel:Number):void {
        myDynamicEngine.myRudder.myBaseReturnVelocity = vel;
    }

    public function get myBaseAccelerationVelocity():Number {
        return myDynamicEngine.myEngineControl.myBaseAccelerationVelocity;
    }

    public function set myBaseAccelerationVelocity(setValue:Number):void {
        myDynamicEngine.myEngineControl.myBaseAccelerationVelocity = setValue;
    }


    public function get myGearCoefs():XML {
        var gearsArray:Array = myDynamicEngine.myTransmission.myTransmissionCoefs;
        var xmlString :String = '<gearCoefs>';
        for (var i:int = 0; i < gearsArray.length; i++) {
            xmlString += '<item><id>' + i + '</id>';
            xmlString += '<coef>' + Math.abs(gearsArray[i]) + '</coef>';
            xmlString += '</item>';
        }
        var magicNumber:Number = 8;//max count of gear
        for (var j:int = gearsArray.length; i < magicNumber; i++) {
            xmlString += '<item><id>' + i + '</id>';
            xmlString += '<coef>' + 0 + '</coef>';
            xmlString += '</item>';
        }
        xmlString += '</gearCoefs>';
        return new XML(xmlString);
    }

    public function set myGearCoefs(gearCoefs:XML):void {
        var coefsArray:Array = new Array();
        var items:XMLList = gearCoefs.children();
        for each(var item:XML in items) {
            var id:Number = new Number(item.child('id')[0]);
            var coef:Number = Math.abs(new Number(item.child('coef')[0]));
            if (id != 0) {
                coef = -coef;
            }
            coefsArray[id] = coef;
        }
        var isZero:Boolean = false;
        for (var i:int = 0; i < coefsArray.length; i++) {
            if (isZero && coefsArray[i] != 0) {
                throw new TSError('gear incorrect');
            }
            if (coefsArray[i] == 0) {
                isZero = true;
            }
        }
        var array:Array = new Array();
        for (var j:int = 0; j < coefsArray.length; j++) {
            if (coefsArray[j] != 0) {
                array[j] = coefsArray[j];
            }
        }
        myDynamicEngine.myTransmission.myTransmissionCoefs = array;
    }

    public function save(): String {
        var xml:XML = new XML('<carParameters/>');
        // да, это пиздец. мне стыдно.
        xml.appendChild(new XML('<color>' + myCar.myColor + '</color>'));
        xml.appendChild(new XML('<hullLength>' + myHullLength + '</hullLength>'));
        xml.appendChild(new XML('<hullWidth>' + myHullWidth + '</hullWidth>'));
        xml.appendChild(new XML('<chassisLength>' + myChassisLength + '</chassisLength>'));
        xml.appendChild(new XML('<rearPinWidth>' + myRearPinWidth + '</rearPinWidth>'));
        xml.appendChild(new XML('<frontPinWidth>' + myFrontPinWidth + '</frontPinWidth>'));
        xml.appendChild(new XML('<carMass>' + myCarMass + '</carMass>'));
        xml.appendChild(new XML('<momentOfInertia>' + myMomentOfInertia + '</momentOfInertia>'));
        xml.appendChild(new XML('<minTurnRadius>' + myMinTurnRadius + '</minTurnRadius>'));
        xml.appendChild(new XML('<rearRelative>' + myRearRelative + '</rearRelative>'));
        xml.appendChild(new XML('<absOn>' + myABS + '</absOn>'));
        xml.appendChild(new XML('<espOn>' + myESP + '</espOn>'));
        xml.appendChild(new XML('<massCenterHeight>' + myMassCenterHeight + '</massCenterHeight>'));
        xml.appendChild(new XML('<frontAxleWeightPercent>' + myFrontAxleWeightPercent + '</frontAxleWeightPercent>'));
        xml.appendChild(myGearCoefs);
        xml.appendChild(new XML('<suspensionMoments>' + myXMomentOfInertia + " " + myYMomentOfInertia + '</suspensionMoments>'));
        xml.appendChild(new XML('<damping>' + myXDamping + " " + myYDamping + '</damping>'));
        xml.appendChild(new XML('<stiffness>' + myStiffness + '</stiffness>'));
        xml.appendChild(new XML('<brakeMoment>' + myBrakeMoment + '</brakeMoment>'));
        xml.appendChild(new XML('<handMoment>' + myHandMoment + '</handMoment>'));
        xml.appendChild(new XML('<cutArea>' + myAreaOfCut + '</cutArea>'));
        xml.appendChild(new XML('<pressingCoef>' + myPressingCoef + '</pressingCoef>'));
        xml.appendChild(new XML('<rudderVel>' + myRudderAngVelocity + '</rudderVel>'));
        xml.appendChild(new XML('<accelerationVel>' + myBaseAccelerationVelocity + '</accelerationVel>'));
        xml.appendChild(new XML('<rudderReturnVelocity>' + myRudderReturnVelocity + '</rudderReturnVelocity>'));
        xml.appendChild(new XML('<baseGear>' + myBaseGear + '</baseGear>'));
        xml.appendChild(myEngineParametersXML);
        xml.appendChild(new XML('<slidingFriction>' + mySlidingFriction + '</slidingFriction>'));
        xml.appendChild(new XML('<staticFriction>' + myStaticFriction + '</staticFriction>'));
        xml.appendChild(new XML('<retardingVelocity>' + myRetardingVelocity + '</retardingVelocity>'));
        return xml.toString();
    }

    public function load(savedParameters: String, throwNoParamException: Boolean, showAlert: Boolean): Array {
        var xml:XML = new XML(savedParameters);
        var noParams: Array = new Array();
        var name: String;

        // hullWidth
        if (xml.hasOwnProperty('hullWidth')) {
            myHullWidth = new Number(xml.child('hullWidth'));
        } else {
            noParams.push('hullWidth');
            myHullWidth = 2.0;
        }

        // hullLength
        if (xml.hasOwnProperty('hullLength')) {
            myHullLength = new Number(xml.child('hullLength'));
        } else {
            noParams.push('hullLength');
            myHullLength = 4.0;
        }

        // rearPinWidth
        name = 'rearPinWidth';
        if (xml.hasOwnProperty(name)) {
            myRearPinWidth = new Number(xml.child(name));
        } else {
            noParams.push(name);
            myRearPinWidth = 1.4;
        }

        // frontPinWidth
        name = 'frontPinWidth';
        if (xml.hasOwnProperty(name)) {
            myFrontPinWidth = new Number(xml.child(name));
        } else {
            noParams.push(name);
            myFrontPinWidth = 1.4;
        }

        // chassisLength
        if (xml.hasOwnProperty('chassisLength')) {
            myChassisLength = new Number(xml.child('chassisLength'));
        } else {
            noParams.push('chassisLength');
            myChassisLength = 2.5;
        }

        // carMass
        if (xml.hasOwnProperty('carMass')) {
            myCarMass = new Number(xml.child('carMass'));
        } else {
            noParams.push('carMass');
            myCarMass = 1000;
        }

        if (xml.hasOwnProperty('momentOfInertia')) {
            myMomentOfInertia = new Number(xml.child('momentOfInertia'));
        } else {
            noParams.push('momentOfInertia');
            myMomentOfInertia = 200;
        }

        if (xml.hasOwnProperty('minTurnRadius')) {
            myMinTurnRadius = new Number(xml.child('minTurnRadius'));
        } else {
            noParams.push('minTurnRadius');
            myMinTurnRadius = 5;
        }

        if (xml.hasOwnProperty('rearRelative')) {
            myRearRelative = new Number(xml.child('rearRelative'));
        } else {
            noParams.push('rearRelative');
            myRearRelative = 0;
        }

        if (xml.hasOwnProperty('absOn')) {
            myABS = Utils.str2bool(new String(xml.absOn));
        } else {
            noParams.push('absOn');
            myABS = true;
        }

        if (xml.hasOwnProperty('espOn')) {
            myESP = Utils.str2bool(new String(xml.espOn));
        } else {
            noParams.push('espOn');
            myESP = true;
        }

        if (xml.hasOwnProperty('massCenterHeight')) {
            myMassCenterHeight = new Number(xml.child('massCenterHeight'));
        } else {
            noParams.push('massCenterHeight');
            myMassCenterHeight = 0.4;
        }

        if (xml.hasOwnProperty('frontAxleWeightPercent')) {
            myFrontAxleWeightPercent = new Number(xml.child('frontAxleWeightPercent'));
        } else {
            noParams.push('frontAxleWeightPercent');
            myFrontAxleWeightPercent = 40;
        }

        if (xml.hasOwnProperty('gearCoefs')) {
            myGearCoefs = xml.elements('gearCoefs')[0];
        } else {
            noParams.push('gearCoefs');
            myGearCoefs = new XML("<gearCoefs><item><id>0</id><coef>3.25</coef></item><item><id>1</id><coef>3.61</coef></item><item> <id>2</id><coef>2.05</coef></item><item><id>3</id><coef>1.37</coef></item><item><id>4</id><coef>0.97</coef></item><item><id>5</id><coef>0.83</coef></item><item><id>6</id><coef>0</coef></item><item><id>7</id><coef>0</coef></item></gearCoefs>");
        }

        if (xml.hasOwnProperty('engineParameters')) {
            myEngineParametersXML = xml.elements('engineParameters')[0];
        } else {
            noParams.push('engineParameters');
            myEngineParametersXML = new XML('<engineParameters endMaxMomentVelocity="5700" endTorque="110" maxTorque="133" maxVelocity="6000" startMaxMomentVelocity="3000" startTorque="50"/>');
        }

        if (xml.hasOwnProperty('suspensionMoments')) {
            var moments:Vector2d = Vector2d.parseTo2dVector(new String(xml.child('suspensionMoments')));
            myXMomentOfInertia = moments.myX;
            myYMomentOfInertia = moments.myY;
        } else {
            noParams.push('suspensionMoments');
            myXMomentOfInertia = 500;
            myYMomentOfInertia = 300;
        }

        if (xml.hasOwnProperty('damping')) {
            var damping:Vector2d = Vector2d.parseTo2dVector(new String(xml.child('damping')));
            myXDamping = damping.myX;
            myYDamping = damping.myY;
        } else {
            noParams.push('damping');
            myXDamping = 5000;
            myYDamping = 5000;
        }

        if (xml.hasOwnProperty('stiffness')) {
            myStiffness = new Number(xml.child('stiffness'));
        } else {
            noParams.push('stiffness');
            myStiffness = 19000;
        }

        if (xml.hasOwnProperty('handMoment')) {
            myHandMoment = new Number(xml.child('handMoment'));
        } else {
            noParams.push('handMoment');
            myHandMoment = 1000;
        }

        if (xml.hasOwnProperty('brakeMoment')) {
            myBrakeMoment = new Number(xml.child('brakeMoment'));
        } else {
            noParams.push('brakeMoment');
            myBrakeMoment = 900;
        }

        if (xml.hasOwnProperty('pressingCoef')) {
            myPressingCoef = new Number(xml.child('pressingCoef'));
        } else {
            noParams.push('pressingCoef');
            myPressingCoef = 0;
        }

        if (xml.hasOwnProperty('cutArea')) {
            myAreaOfCut = new Number(xml.child('cutArea'));
        } else {
            noParams.push('cutArea');
            myAreaOfCut = 1;
        }

        if (xml.hasOwnProperty('rudderVel')) {
            myRudderAngVelocity = new Number(xml.child('rudderVel'));
        } else {
            noParams.push('rudderVel');
            myRudderAngVelocity = 1;
        }

        if (xml.hasOwnProperty('accelerationVel')) {
            myBaseAccelerationVelocity = new Number(xml.child('accelerationVel'));
        } else {
            noParams.push('accelerationVel');
            myBaseAccelerationVelocity = 1;
        }

        if (xml.hasOwnProperty('rudderReturnVelocity')) {
            myRudderReturnVelocity = new Number(xml.child('rudderReturnVelocity'));
        } else {
            noParams.push('rudderReturnVelocity');
            myRudderReturnVelocity = 4;
        }

        if (xml.hasOwnProperty('baseGear')) {
            myBaseGear = new Number(xml.child('baseGear'));
        } else {
            noParams.push('baseGear');
            myBaseGear = 3.65;
        }

        if (xml.hasOwnProperty('staticFriction')) {
            myStaticFriction = new Number(xml.child('staticFriction'));
        } else {
            noParams.push('staticFriction');
            myStaticFriction = 1.4;
        }

        if (xml.hasOwnProperty('slidingFriction')) {
            mySlidingFriction = new Number(xml.child('slidingFriction'));
        } else {
            noParams.push('slidingFriction');
            mySlidingFriction = 1.3;
        }

        if (xml.hasOwnProperty('retardingVelocity')) {
            myRetardingVelocity = new Number(xml.child('retardingVelocity'));
        } else {
            noParams.push('retardingVelocity');
            myRetardingVelocity = 0.5;
        }

        if (noParams.length > 0) {
            if (showAlert) {
                Alert.show("Missing params in car file:\n" + Utils.arrayToString(noParams, "\n"));
            }
            if (throwNoParamException) {
                //	throw new TSError("Missing params in car file:\n" + Utils.arrayToString(noParams, "\n"));
            }
        }

        // color
        name = 'color';
        if (xml.hasOwnProperty(name)) {
            myCar.myColor = uint("0x" + xml.child(name));
        } else {
            noParams.push(name);
            myCar.myColor = 0xFFFFFF;
        }

        dispatchEvent(new Event(Event.CHANGE));

        return noParams;
    }

    public function get myParameters():EngineParameters {
        return myModel.engine.myParameters;
    }

    public function set myParameters(v:EngineParameters):void {
        myModel.engine.myParameters = v;
        myCar.carModel.updateCar();
    }

    public function get myBaseGear():Number {
        return myCar.carModel.getTransmission().myBaseGear;
    }

    public function set myBaseGear(coef:Number):void {
        myCar.carModel.getTransmission().myBaseGear = coef;
    }

    public function get mySlidingFriction():Number {
        return myCar.carModel.myCarDynamicEngine.myChassis.mySlidingFriction;
    }

    public function get myStaticFriction():Number {
        return myCar.carModel.myCarDynamicEngine.myChassis.myStaticFriction;
    }

    public function set mySlidingFriction(coef:Number):void {
        myCar.carModel.myCarDynamicEngine.myChassis.mySlidingFriction = coef;
    }

    public function set myStaticFriction(coef:Number):void {
        myCar.carModel.myCarDynamicEngine.myChassis.myStaticFriction = coef;
    }

    public function set myRetardingVelocity(coef:Number):void {
        myCar.carModel.getBrakes().myBaseVelocity = coef;
    }

    public function get myRetardingVelocity():Number {
        return myCar.carModel.getBrakes().myBaseVelocity;
    }

    public function get myName(): String {
        return myCar.getName();
    }
}
}