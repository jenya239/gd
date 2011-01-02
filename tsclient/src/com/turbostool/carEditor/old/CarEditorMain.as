package com.turbostool.client.carEditor {
import com.turbostool.client.event.CarLoadedEvent;
import com.turbostool.client.game.components.car.*;

import flash.events.Event;
import flash.events.MouseEvent;
import flash.net.URLLoader;
import flash.net.URLRequest;
import flash.net.URLRequestMethod;

import mx.controls.*;
import mx.core.Application;

public class CarEditorMain {
    private var myApplication:Application;
    private var myCarEditorScreen: CarEditorScreen;
    private var myEditable: EditableCarParameters;
    private var myBaseURL:String;

    public function CarEditorMain(app:Application, editable: EditableCarParameters, baseUrl:String) {
        myBaseURL = baseUrl;
        myApplication = app;
        myCarEditorScreen = myApplication['cesEditorScreen'] as CarEditorScreen
        myEditable = editable;

        carParametersChangeHandler(null);
        registerListeners();
    }

    private function get myTIHullWidth():TextInput {
        return myCarEditorScreen['tiHullWidth'] as TextInput;
    }

    private function get myTIHullLength():TextInput {
        return myCarEditorScreen['tiHullLength'] as TextInput;
    }

    private function get myTIRearWidth():TextInput {
        return myCarEditorScreen['tiRearWidth'] as TextInput;
    }

    private function get myTIFrontWidth():TextInput {
        return myCarEditorScreen['tiFrontPinWidth'] as TextInput;
    }

    private function get myTIChassisLength():TextInput {
        return myCarEditorScreen['tiChassisLength'] as TextInput;
    }

    private function get myTICarMass():TextInput {
        return myCarEditorScreen['tiCarMass'] as TextInput;
    }

    private function get myTIMomentOfInertia():TextInput {
        return myCarEditorScreen['tiMomentOfInertia'] as TextInput;
    }

    private function get myTITorqueFillValue():TextInput {
        return myCarEditorScreen['tiTorqueFillValue'] as TextInput;
    }

    private function get myTIMinTurnRadius():TextInput {
        return myCarEditorScreen['tiMinTurnRadius'] as TextInput;
    }

    private function get myBApply():Button {
        return myApplication['bApply'] as Button;
    }

    private function get myBSave():Button {
        return myCarEditorScreen['bSave'] as Button;
    }

    private function get myTICarName():TextInput {
        return myCarEditorScreen['tiCarName'] as TextInput;
    }

    private function get myCBCarList():ComboBox {
        return myCarEditorScreen['cbCarList'] as ComboBox;
    }

    private function get myCBRearRelativeTorque():ComboBox {
        return myCarEditorScreen['cbRearRelative'] as ComboBox;
    }

    private function get myDGEngineTorque():DataGrid {
        return myCarEditorScreen['dgEngineTorques'] as DataGrid;
    }

    private function get myCBABS():ComboBox {
        return myCarEditorScreen['cbABS'] as ComboBox;
    }

    private function get myCBESP():ComboBox {
        return myCarEditorScreen['cbESP'] as ComboBox;
    }

    private function get myDGGear():DataGrid {
        return myCarEditorScreen['dgGear'] as DataGrid;
    }

    private function get myTIBaseGear():TextInput {
        return myCarEditorScreen['tiEngineCoef'] as TextInput;
    }

    private function get myTIRetardingVelocity():TextInput {
        return myCarEditorScreen['tiRetardingVelocity'] as TextInput;
    }

    /*
     private function get myTIMassCenterLongShift():TextInput {
     return myCarEditorScreen['tiMassCenterLongShift'] as TextInput;
     }//*/

    /**
     * процент веса авто, приходящийся на переднюю ось
     */
    private function get myTIFrontAxleWeightPercent():TextInput {
        return myCarEditorScreen['tiFrontAxleWeightPercent'] as TextInput;
    }

    private function get myTIMassCenterHeight():TextInput {
        return myCarEditorScreen['tiMassCenterHeight'] as TextInput;
    }

    private function get myTIXMoment():TextInput {
        return myCarEditorScreen['tiXMoment'] as TextInput;
    }

    private function get myTIYMoment():TextInput {
        return myCarEditorScreen['tiYMoment'] as TextInput;
    }

    private function get myTIStiffness() :TextInput {
        return myCarEditorScreen['tiStiffness'] as TextInput;
    }

    private function get myTIXDamping():TextInput {
        return myCarEditorScreen['tiXDamping'] as TextInput;
    }

    private function get myTIYDamping():TextInput {
        return myCarEditorScreen['tiYDamping'] as TextInput;
    }

    private function get myTIBrakeMoment():TextInput {
        return myCarEditorScreen['tiBrakeMoment'] as TextInput;
    }

    private function get myTIHandMoment():TextInput {
        return myCarEditorScreen['tiHandMoment'] as TextInput;
    }

    private function get myTIPressing():TextInput {
        return myCarEditorScreen['tiPressing'] as TextInput;
    }

    private function get myTIArea():TextInput {
        return myCarEditorScreen['tiArea'] as TextInput;
    }

    private function get myTIRudderAngVelocity():TextInput {
        return myCarEditorScreen['tiRudderAngVelocity'] as TextInput;
    }

    private function get myTIRudderReturnVelocity():TextInput {
        return myCarEditorScreen['tiRudderReturnVelocity'] as TextInput;
    }

    private function get myTIGas():TextInput {
        return myCarEditorScreen['tiGas'] as TextInput;
    }

    private function get myEPEditor(): EngineParameterEditor {
        return myCarEditorScreen['epEditor'] as EngineParameterEditor;
    }

    private function get myTISlidingFriction():TextInput {
        return myCarEditorScreen['tiSlidingFriction'] as TextInput;
    }

    private function get myTIStaticFriction():TextInput {
        return myCarEditorScreen['tiStaticFriction'] as TextInput;
    }

    private function registerListeners():void {
        myBApply.addEventListener(MouseEvent.CLICK, applyChangesHandler);
        myBSave.addEventListener(MouseEvent.CLICK, saveClickHandler);
        myEditable.addEventListener(Event.CHANGE, carParametersChangeHandler);
        myCBCarList.addEventListener(Event.CHANGE, loadCar);
        myCBCarList.addEventListener(MouseEvent.MOUSE_DOWN, carListClickHandler);
    }

    private function applyChangesHandler(event:MouseEvent):void {
        var l:Number = new Number(myTIHullLength.text) / 1000;
        var w:Number = new Number(myTIHullWidth.text) / 1000;
        var che_l:Number = new Number(myTIChassisLength.text) / 1000;
        var rearWidth:Number = new Number(myTIRearWidth.text) / 1000;
        var frontWidth:Number = new Number(myTIFrontWidth.text) / 1000;
        var mass:Number = new Number(myTICarMass.text);
        var MoI:Number = new Number(myTIMomentOfInertia.text);
        var mtr:Number = new Number(myTIMinTurnRadius.text);
        var rear:Number = myCBRearRelativeTorque.selectedIndex / 2;
        var abs:Boolean = ((myCBABS.selectedIndex == 0  ) ? false : true);
        var esp:Boolean = ((myCBESP.selectedIndex == 0  ) ? false : true);
        var fawp: Number = new Number(myTIFrontAxleWeightPercent.text);
        var mch: Number = new Number(myTIMassCenterHeight.text);

        var xMoment:Number = new Number(myTIXMoment.text);
        var yMoment:Number = new Number(myTIYMoment.text);
        var stiffness:Number = new Number(myTIStiffness.text);
        var xDamping:Number = new Number(myTIXDamping.text);
        var yDamping:Number = new Number(myTIYDamping.text);
        var handMoment:Number = new Number(myTIHandMoment.text);
        var brakeMoment:Number = new Number(myTIBrakeMoment.text);
        var pressingCoef:Number = new Number(myTIPressing.text);
        var cutArea:Number = new Number(myTIArea.text);
        var rudderAngVelocity:Number = new Number(myTIRudderAngVelocity.text);
        var gas:Number = new Number(myTIGas.text);
        var returnVelocity:Number = new Number(myTIRudderReturnVelocity.text);
        var baseGear:Number = new Number(myTIBaseGear.text);
        var staticFriction:Number = new Number(myTIStaticFriction.text);
        var slidingFriction:Number = new Number(myTISlidingFriction.text);
        var retardingVelocity:Number = new Number(myTIRetardingVelocity.text);

        var xmlGear:XML = new XML('<gearCoefs/>');
        for each (var xmlNode1:XML in myDGGear.dataProvider) {
            xmlGear.appendChild(xmlNode1);
        }

        if ((l > 0) && (w > 0) &&
            (che_l != 0) && (rearWidth != 0) && (frontWidth != 0) &&
            (mass > 0) && (MoI > 0) &&
            (mtr > 0.1) && ( rear >= 0 ) &&
            (fawp >= 0) && (fawp <= 100)) {
            myEditable.myHullLength = l;
            myEditable.myHullWidth = w;
            myEditable.myChassisLength = che_l;
            myEditable.myRearPinWidth = rearWidth;
            myEditable.myFrontPinWidth = frontWidth;
            myEditable.myCarMass = mass;
            myEditable.myMomentOfInertia = MoI;
            myEditable.myMinTurnRadius = mtr;
            myEditable.myRearRelative = rear;
            myEditable.myGearCoefs = xmlGear;
            myEditable.myABS = abs;
            myEditable.myESP = esp;
            //				myEditable.myMassCenter = massCenter;
            myEditable.myMassCenterHeight = mch;
            myEditable.myFrontAxleWeightPercent = fawp;

            myEditable.myXMomentOfInertia = xMoment;
            myEditable.myYMomentOfInertia = yMoment;
            myEditable.myXDamping = xDamping;
            myEditable.myYDamping = yDamping;
            myEditable.myStiffness = stiffness;
            myEditable.myHandMoment = handMoment;
            myEditable.myBrakeMoment = brakeMoment;
            myEditable.myPressingCoef = pressingCoef;
            myEditable.myAreaOfCut = cutArea;
            myEditable.myRudderAngVelocity = rudderAngVelocity;
            myEditable.myBaseAccelerationVelocity = gas;
            myEditable.myRudderReturnVelocity = returnVelocity;
            myEditable.myParameters = myEPEditor.myEngineParameters;
            myEditable.myBaseGear = baseGear;
            myEditable.myStaticFriction = staticFriction;
            myEditable.mySlidingFriction = slidingFriction;
            myEditable.myRetardingVelocity = retardingVelocity;
        }
    }

    private function saveClickHandler(event:MouseEvent):void {
        //Alert.show(myDGEngineTorque.dataProvider.toString());
        applyChangesHandler(null);
        //Alert.show(myBaseURL);
        var request:URLRequest = new URLRequest(myBaseURL + 'main.php?requestType=carSave&carName=' + myTICarName.text);

        request.contentType = "text/xml";
        request.data = myEditable.save();
        request.method = URLRequestMethod.POST;
        var loader:URLLoader = new URLLoader();
        loader.addEventListener(Event.COMPLETE, saveCompletedHandler);
        try {
            loader.load(request);
        } catch (error:ArgumentError) {
            trace("An ArgumentError has occurred.");
        } catch (error:SecurityError) {
            trace("A SecurityError has occurred.");
        }
        lockEditor();
        function saveCompletedHandler(event:Event):void {
            var listLoader:URLLoader = new URLLoader();
            listLoader.addEventListener(Event.COMPLETE, carListHandler);
            var request:URLRequest = new URLRequest(myBaseURL + 'main.php?requestType=carList');
            try {
                listLoader.load(request);
            } catch (error:ArgumentError) {
                Alert.show("An ArgumentError has occurred.");
            } catch (error:SecurityError) {
                Alert.show("A SecurityError has occurred.");
            }
            function carListHandler(event:Event):void {
                var s:String = listLoader.data;
                var cars:Array = s.split(',');
                myCBCarList.dataProvider = cars;
                unlockEditor();
            }
        }
    }

    private function carListClickHandler(e: MouseEvent): void {
        saveClickHandler(null);
        myCBCarList.removeEventListener(MouseEvent.MOUSE_DOWN, carListClickHandler);
    }

    private function loadCar(event:Event):void {
        if (myCBCarList.selectedIndex == -1) {
            return;
        }
        lockEditor();

        var carLoader: CarLoader = new CarLoader();
        carLoader.addEventListener(CarLoadedEvent.CAR_LOADED, carDataHandler);

        var carName: String = myCBCarList.selectedLabel;
        carLoader.loadCarByName(carName);

        function carDataHandler(event: CarLoadedEvent):void {
            carLoader.removeEventListener(CarLoadedEvent.CAR_LOADED, carDataHandler);
            myTICarName.text = carName;
            myEditable.load(event.carXml, false, true);
            myCBCarList.selectedIndex = -1;
            unlockEditor();
        }
    }

    private function carParametersChangeHandler(event:Event):void {
        myTIHullWidth.text = (myEditable.myHullWidth * 1000).toString();
        myTIHullLength.text = (myEditable.myHullLength * 1000).toString();
        myTIRearWidth.text = (myEditable.myRearPinWidth * 1000).toString();
        myTIFrontWidth.text = (myEditable.myFrontPinWidth * 1000).toString();
        myTIChassisLength.text = (myEditable.myChassisLength * 1000).toString();
        myTICarMass.text = myEditable.myCarMass.toString();
        myTIMomentOfInertia.text = myEditable.myMomentOfInertia.toString();
        myTIMinTurnRadius.text = myEditable.myMinTurnRadius.toString();
        var val:int = new int(Math.round(myEditable.myRearRelative * 2));
        myCBRearRelativeTorque.selectedIndex = val;
        myCBABS.selectedIndex = myEditable.myABS ? 1 : 0;
        myCBESP.selectedIndex = myEditable.myESP ? 1 : 0;
        myDGGear.dataProvider = myEditable.myGearCoefs.children();
        myTIMassCenterHeight.text = myEditable.myMassCenterHeight.toString();
        myTIFrontAxleWeightPercent.text = myEditable.myFrontAxleWeightPercent.toString();
        myTIXMoment.text = myEditable.myXMomentOfInertia.toString();
        myTIYMoment.text = myEditable.myYMomentOfInertia.toString();
        myTIStiffness.text = myEditable.myStiffness.toString();
        myTIXDamping.text = myEditable.myXDamping.toString();
        myTIYDamping.text = myEditable.myYDamping.toString();
        myTIBrakeMoment.text = myEditable.myBrakeMoment.toString();
        myTIHandMoment.text = myEditable.myHandMoment.toString();
        myTIPressing.text = myEditable.myPressingCoef.toString();
        myTIArea.text = myEditable.myAreaOfCut.toString();
        myTIRudderAngVelocity.text = myEditable.myRudderAngVelocity.toString();
        myTIGas.text = myEditable.myBaseAccelerationVelocity.toString();
        myTIRudderReturnVelocity.text = myEditable.myRudderReturnVelocity.toString();
        myEPEditor.myEngineParameters = myEditable.myParameters;
        myTIBaseGear.text = myEditable.myBaseGear.toString();
        myTISlidingFriction.text = myEditable.mySlidingFriction.toString();
        myTIStaticFriction.text = myEditable.myStaticFriction.toString();
        myTIRetardingVelocity.text = myEditable.myRetardingVelocity.toString();
    }

    private function lockEditor():void {
        myCarEditorScreen.enabled = false;
    }

    private function unlockEditor():void {
        myCarEditorScreen.enabled = true;
    }

}
}