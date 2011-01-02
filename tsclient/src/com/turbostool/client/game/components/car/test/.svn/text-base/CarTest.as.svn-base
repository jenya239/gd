package com.turbostool.client.game.components.car.test   {

import com.turbostool.client.dynamicEngine.*;
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.game.RaceWorld;
import com.turbostool.client.game.components.car.*;
import com.turbostool.client.geom.*;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.ArrayList;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

public class CarTest extends TestCase {
    private var myModel:CarModel;
    private var myCar:Car;
    private var myEngine:DynamicEngine;

    public function CarTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new CarTest("testCarRoad"));
        return ts;
    }

    override public function setUp():void {
        myCar = new Car();
        myModel = myCar.carModel;
        var raceWorld: RaceWorld = new RaceWorld(new ArrayList(), new Piece2d(new Vector2d(0, 1), new Vector2d(1, 0)), new Vector2dSequence(), "ss", new ArrayList());
        myEngine = new DynamicEngine();
        var raceModel: RaceModel = myEngine.raceModel;
        raceModel.setRaceWorld(raceWorld);
        raceModel.getCars().addItem(myCar);
    }

    private function get carController():CarController {
        return myCar.getController();
    }

    private function setUpCarRoad():void {
        setUp();
        var pol:Polygon = new Polygon();
        pol.addVertex(new Vector2d(-10, -10));
        pol.addVertex(new Vector2d(1000, -10));
        pol.addVertex(new Vector2d(1000, 10));
        pol.addVertex(new Vector2d(-10, 10));
        myModel.myHull.myFrame.setXYU(0, 0, Math.PI / 2);
    }

    public function testCarRoad():void {
        const eps:Number = 0.1;
        setUpCarRoad();
        carController.onControl(new CarControlEvent(true, RudderControl.NONE, false, false, 0, CarControlEvent.GEAR_NONE));
        var dvs:Array = new Array(0);
        for (var j:int = 0; now < 0.1; j++) {
            var prev:Number = myModel.myVelocity.myX;
            var lastQuant: Number = 0;
            for (var now: Number = 0.01; now < 1; now += 0.01) {
                lastQuant = myEngine.step(now, lastQuant, 0.01, true);
            }
            dvs[j] = myModel.myVelocity.myX - prev;
        }
        for each (var dv:Number in dvs) {
            assertTrue(dv, Math.abs(dv - dvs[0]) < eps);
        }
    }


}
}