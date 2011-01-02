package com.turbostool.client.game.components.car.test {

import com.turbostool.client.game.components.car.*;
import com.turbostool.client.geom.*;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.Collection;

import flexunit.framework.TestCase;

public class CarModelTest extends TestCase
{
    public function CarModelTest(methodName:String = null) {
        super(methodName);
    }

    public function testMyMass():void {
        var car:CarModel = new CarModel(1, 2);
        car.setHullMass(700);
        assertTrue(car.getHull().myMass, Utils.equal(700, car.getHull().myMass));
    }

    public function testCarModel():void {
        /*var car:CarModel = new CarModel(1, 2);
         car.myHull.setFrame(new LocalFrame2d(new Vector2d(10, 20), Math.PI / 2));
         assertTrue(car.myFrame,
         car.myFrame.equals(
         new LocalFrame2d(new Vector2d(10, 20), Math.PI / 2)
         )
         );
         car.myHull.addForce(new Vector2d(10, 30), new Vector2d(30, 0));
         car.myHull.calcVelocity(1);
         assertTrue(car.myAngularVelocity, Utils.equal(
         car.myAngularVelocity,
         - 300/ car.myMomentOfInertia
         ));
         var test:Vector3d = (new Vector2d(30, 0)).numberProduct2d(1 / car.myMass);
         assertTrue(car.myVelocity + ' ' + test, car.myVelocity.equals(test));

         car.setHullMass(700);
         car.myVelocity = Vector2d.getZero();
         car.myHull.nullForces();
         assertTrue(Utils.inRange(car.myMass, 700, 800));
         assertTrue(grav.interacts(car.myHull));
         assertTrue(car.myHull.interacts(grav));
         car.myHull.doInteraction(grav);
         car.myHull.calcForces();
         car.myHull.calcVelocity(1);
         assertTrue(car.myAngularVelocity, Utils.equal(
         car.myAngularVelocity,
         - 300/ car.myMomentOfInertia
         ));
         test = (new Vector3d(2, 3, 4)).numberProduct(700);
         assertTrue(test + ' ' + car.myHull.myForce, test.equals(car.myHull.myForce));
         test = new Vector3d(2, 3, 4);
         assertTrue(car.myVelocity + ' ' +test, car.myVelocity.equals(
         test
         ));*/
    }

    public function testControlable():void {
        var car:CarModel = new CarModel(1, 2);
        assertFalse(car.myAccelerate);
        car.myAccelerate = true;
        assertTrue(car.myAccelerate);
        car.myAccelerate = false;
        assertFalse(car.myAccelerate);
        assertEquals(RudderControl.NONE, car.myRudderControl.myAction);
        car.myRudderControl.myAction = RudderControl.LEFT;
        assertEquals(RudderControl.LEFT, car.myRudderControl.myAction);
        //car.myHull.setFrame(new LocalFrame2d(new Vector2d(), ));
        car.myHull.myFrame.setXYU(10, 20, Math.PI / 2);
        car.myVelocity = new Vector2d(1, 5);
        var res:Number = car.getCarDirectionVelocity();
        assertTrue(res, Utils.equal(1, res));
    }

    public function testParties():void {
        var car:CarModel = new CarModel(1, 2);
        var dyns:Collection = car.getDynamics();
        var colls:Collection = car.getCollidables();
        assertEquals(1, dyns.length());
        assertEquals(1, colls.length());
        assertTrue(dyns.getItemAt(0) is CarDynamicEngine);
        assertTrue(colls.contains(car));
    }

    /*public function testOnRoad():void {
     var carC: Car = new Car();
     var car:CarModel = carC.carModel;
     car.getHull().getRectangle().myWidth = 1;
     car.getHull().getRectangle().myHeight = 2;
     car.getChassis().getHull().myFrame.myR.setXY(5, 5);
     car.myAccelerate = true;
     var raceWorld: RaceWorld = new RaceWorld(new ArrayList(), new Piece2d(new Vector2d(0, 1), new Vector2d(1, 0)), new Vector2dSequence(), "ss", new ArrayList());
     var engine:DynamicEngine = new DynamicEngine();
     var raceModel: RaceModel = engine.raceModel;
     raceModel.setRaceWorld(raceWorld);
     raceModel.getCars().addItem(carC);
     engine.step(1, 0, 1, true);
     engine.step(2, 1, 1, true);
     engine.step(3, 2, 1, true);
     engine.step(4, 3, 1, true);
     assertFalse(car.myR, car.myR.equals(new Vector2d(5, 5)));
     }*/

    public function testWeight():void {
        var car:CarModel = new CarModel(1, 2);
        car.getHull().myMass = 690;
        assertTrue("weight = " + car.getWeight(), Utils.equal(car.getWeight(), 690 * Utils.G));
    }

    public function testGlobalPolygon():void {
        var car:CarModel = new CarModel(2, 4);
        car.myFrame.setXYU(1, 2, Math.PI / 2);
        var polygon:Polygon = new Polygon();
        polygon.addVertex(new Vector2d(3, 1));
        polygon.addVertex(new Vector2d(3, 3));
        polygon.addVertex(new Vector2d(-1, 3));
        polygon.addVertex(new Vector2d(-1, 1));
        car.getChassis().getHull().updatePolygons();
        assertTrue(car.myGlobalPolygon.toString(), polygon.equals(car.myGlobalPolygon));
    }
}
}