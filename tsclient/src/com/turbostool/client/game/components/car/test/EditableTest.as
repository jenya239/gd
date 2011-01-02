package com.turbostool.client.game.components.car.test
{

import com.turbostool.client.game.components.car.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;

public class EditableTest extends TestCase {
    public function EditableTest(methodName:String = null) {
        super(methodName);
    }

    public function testEditable():void {
        var car: Car = new Car();
        var ecp: EditableCarParameters = car.getEditableCarParameters();

        ecp.myCarMass = 16;
        assertTrue(ecp.myCarMass, Utils.equal(16, ecp.myCarMass));

        ecp.myChassisLength = 156;
        assertTrue(ecp.myChassisLength, Utils.equal(156, ecp.myChassisLength));

        ecp.myRearPinWidth = 1.6;
        assertTrue(ecp.myRearPinWidth, Utils.equal(1.6, ecp.myRearPinWidth));

        ecp.myFrontPinWidth = 1.6;
        assertTrue(ecp.myFrontPinWidth, Utils.equal(1.6, ecp.myFrontPinWidth));

        ecp.myHullLength = 116;
        assertTrue(ecp.myHullLength, Utils.equal(116, ecp.myHullLength));

        ecp.myHullWidth = 816;
        assertTrue(ecp.myHullWidth, Utils.equal(816, ecp.myHullWidth));

        ecp.myMomentOfInertia = 163;
        assertTrue(ecp.myMomentOfInertia, Utils.equal(163, ecp.myMomentOfInertia));

        /*
         ecp.myEngineTorque = 33;
         assertTrue(ecp.myEngineTorque, Utils.equal(33, ecp.myEngineTorque));

         ecp.myMaxRudderAngle = 25;
         assertTrue(ecp.myMaxRudderAngle, Utils.equal(25, ecp.myMaxRudderAngle));
         //*/
        ecp.myMinTurnRadius = 1234;
        assertTrue(ecp.myMinTurnRadius, Utils.equal(1234, ecp.myMinTurnRadius));

        //////////////
        ecp.myRearRelative = 1;
        assertTrue(ecp.myRearRelative, Utils.equal(1, ecp.myRearRelative));

        ecp.myABS = false;
        assertFalse(ecp.myABS, ecp.myABS);

        ecp.myESP = false;
        assertFalse(ecp.myESP, ecp.myESP);
        /*
         var masCenter:Vector3d = Vector3d.parseTo3dVector("0.46772 0 0.45");
         ecp.myMassCenter = masCenter;
         assertTrue(ecp.myMassCenter, masCenter.equals(ecp.myMassCenter));
         //*/
        ecp.myMassCenterHeight = 0.45;
        assertTrue(ecp.myMassCenterHeight, Utils.equal(0.45, ecp.myMassCenterHeight));

        ecp.myFrontAxleWeightPercent = 78;
        assertTrue(ecp.myFrontAxleWeightPercent, Utils.equal(78, ecp.myFrontAxleWeightPercent));

        var gearXML:XML = new XML("" +
                                  "<gearCoefs>" +
                                  "<item>" +
                                  "<id>0</id>" +
                                  "<coef>3.2</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>1</id>" +
                                  "<coef>3.39</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>2</id>" +
                                  "<coef>0</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>3</id>" +
                                  "<coef>0</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>4</id>" +
                                  "<coef>0</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>5</id>" +
                                  "<coef>0</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>6</id>" +
                                  "<coef>0</coef>" +
                                  "</item>" +
                                  "<item>" +
                                  "<id>7</id>" +
                                  "<coef>0</coef>" +
                                  "</item>" +
                                  "</gearCoefs>");
        ecp.myGearCoefs = gearXML;
        assertTrue(ecp.myGearCoefs, gearXML.toString() == (ecp.myGearCoefs.toString()));

        var suspMoments:Vector2d = Vector2d.parseTo2dVector("235 123");
        ecp.myXMomentOfInertia = suspMoments.myX;
        ecp.myYMomentOfInertia = suspMoments.myY;
        assertTrue(ecp.myXMomentOfInertia, Utils.equal(ecp.myXMomentOfInertia, suspMoments.myX));
        assertTrue(ecp.myYMomentOfInertia, Utils.equal(ecp.myYMomentOfInertia, suspMoments.myY));

        var damp:Vector2d = Vector2d.parseTo2dVector("235 123");
        ecp.myXDamping = damp.myX;
        ecp.myYDamping = damp.myY;
        assertTrue(ecp.myXDamping, Utils.equal(ecp.myXDamping, damp.myX));
        assertTrue(ecp.myYDamping, Utils.equal(ecp.myYDamping, damp.myY));

        ecp.myStiffness = 123;
        assertTrue(ecp.myStiffness, Utils.equal(ecp.myStiffness, 123));

        ecp.myBrakeMoment = 128;
        assertTrue(ecp.myBrakeMoment, Utils.equal(ecp.myBrakeMoment, 128));

        ecp.myHandMoment = 138;
        assertTrue(ecp.myHandMoment, Utils.equal(ecp.myHandMoment, 138));

        ecp.myAreaOfCut = 12;
        assertTrue(ecp.myAreaOfCut, Utils.equal(ecp.myAreaOfCut, 12));

        ecp.myPressingCoef = 23;
        assertTrue(ecp.myPressingCoef, Utils.equal(ecp.myPressingCoef, 23));

        ecp.myRudderAngVelocity = 34;
        assertTrue(ecp.myRudderAngVelocity, Utils.equal(ecp.myRudderAngVelocity, 34));

        ecp.myBaseAccelerationVelocity = 9;
        assertTrue(ecp.myBaseAccelerationVelocity, Utils.equal(ecp.myBaseAccelerationVelocity, 9));

        /////////////
        var saved: String = ecp.save();
        ecp = (new Car()).getEditableCarParameters();

        ecp.load(saved, false, false);

        assertTrue(ecp.myCarMass, Utils.equal(16, ecp.myCarMass));
        assertTrue(ecp.myChassisLength, Utils.equal(156, ecp.myChassisLength));
        assertTrue(ecp.myRearPinWidth, Utils.equal(1.6, ecp.myRearPinWidth));
        assertTrue(ecp.myFrontPinWidth, Utils.equal(1.6, ecp.myFrontPinWidth));
        assertTrue(ecp.myHullLength, Utils.equal(116, ecp.myHullLength));
        assertTrue(ecp.myHullWidth, Utils.equal(816, ecp.myHullWidth));
        assertTrue(ecp.myMomentOfInertia, Utils.equal(163, ecp.myMomentOfInertia));
        /*
         assertTrue(ecp.myEngineTorque, Utils.equal(33, ecp.myEngineTorque));
         assertTrue(ecp.myMaxRudderAngle, Utils.equal(25, ecp.myMaxRudderAngle));
         //*/
        assertTrue(ecp.myMinTurnRadius, Utils.equal(1234, ecp.myMinTurnRadius));
        ///////////////////////
        assertTrue(ecp.myRearRelative, Utils.equal(1, ecp.myRearRelative));
        assertFalse(ecp.myABS, ecp.myABS == true);
        assertFalse(ecp.myESP, ecp.myESP == true);

        assertTrue(ecp.myMassCenterHeight, Utils.equal(0.45, ecp.myMassCenterHeight));
        assertTrue(ecp.myFrontAxleWeightPercent, Utils.equal(78, ecp.myFrontAxleWeightPercent));

        assertTrue(ecp.myGearCoefs, gearXML.toString() == (ecp.myGearCoefs.toString()));
        assertTrue(ecp.myXMomentOfInertia, Utils.equal(ecp.myXMomentOfInertia, suspMoments.myX));
        assertTrue(ecp.myYMomentOfInertia, Utils.equal(ecp.myYMomentOfInertia, suspMoments.myY));
        assertTrue(ecp.myXDamping, Utils.equal(ecp.myXDamping, damp.myX));
        assertTrue(ecp.myYDamping, Utils.equal(ecp.myYDamping, damp.myY));
        assertTrue(ecp.myStiffness, Utils.equal(ecp.myStiffness, 123));
        assertTrue(ecp.myBrakeMoment, Utils.equal(ecp.myBrakeMoment, 128));
        assertTrue(ecp.myHandMoment, Utils.equal(ecp.myHandMoment, 138));
        assertTrue(ecp.myAreaOfCut, Utils.equal(ecp.myAreaOfCut, 12));
        assertTrue(ecp.myPressingCoef, Utils.equal(ecp.myPressingCoef, 23));
        assertTrue(ecp.myRudderAngVelocity, Utils.equal(ecp.myRudderAngVelocity, 34));
        assertTrue(ecp.myBaseAccelerationVelocity, Utils.equal(ecp.myBaseAccelerationVelocity, 9));
        ///////////////////////
    }

    public function testEngineParametersXML():void {
        var car: Car = new Car();
        var ecp: EditableCarParameters = car.getEditableCarParameters();

        var s: String = '<engineParameters '
                + 'endMaxMomentVelocity="1" '
                + 'endTorque="2" '
                + 'maxTorque="3" '
                + 'maxVelocity="4" '
                + 'startMaxMomentVelocity="8" '
                + 'startTorque="6"/>';
        ecp.myEngineParametersXML = new XML(s);
        var p: EngineParameters = car.carModel.engine.myParameters;
        assertEquals(1 * Math.PI / 30, p.myEndMaxMomentVelocity);
        assertEquals(2, p.myEndTorque);
        assertEquals(3, p.myMaxTorque);
        assertEquals(4 * Math.PI / 30, p.myMaxVelocity);
        assertEquals(8 * Math.PI / 30, p.myStartMaxMomentVelocity);
        assertEquals(6, p.myStartTorque);

        var xml: XML = ecp.myEngineParametersXML;
        assertEquals(s, xml.toXMLString());
    }

}
}