package com.turbostool.client.game.route.test
{

import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;

public class ObstacleModelTest extends TestCase    {

    public function ObstacleModelTest(methodName:String = null) {
        super(methodName);
    }

    public function testTryBodyCollision():void {
        var piece:Vector2dSequence = new Vector2dSequence();
        piece.addVertex(new Vector2d(0, 0));
        piece.addVertex(new Vector2d(10, 0));
        var obstacle:ObstacleModel = new ObstacleModel(piece);
        var car:CarModel = new CarModel(2, 4);
        car.myCarDynamicEngine.saveState();
        car.myFrame.setXYU(0, 3, 0);
        car.myCarDynamicEngine.myHull.updateBorder();
        car.myCarDynamicEngine.myHull.updatePolygons();
        assertTrue('1', obstacle.tryBodyCollision(car));
        car.myCarDynamicEngine.myHull.updateBorder();
        car.myCarDynamicEngine.myHull.updatePolygons();
        assertTrue('2', obstacle.tryBodyCollision(car));
        car.myFrame.setXYU(3, 3, 0);
        car.myCarDynamicEngine.saveState();
        car.myFrame.setXYU(6, 3, 0);
        car.myCarDynamicEngine.myHull.updateBorder();
        car.myCarDynamicEngine.myHull.updatePolygons();
        assertFalse('3', obstacle.tryBodyCollision(car));
        piece = new Vector2dSequence();
        piece.addVertex(new Vector2d(0, 0));
        piece.addVertex(new Vector2d(10, 0));
        obstacle = new ObstacleModel(piece);
        car.myFrame.setXYU(0, 4, 0);
        car.myCarDynamicEngine.saveState();
        car.myFrame.setXYU(3, 1, 0);
        car.myCarDynamicEngine.myHull.updateBorder();
        car.myCarDynamicEngine.myHull.updatePolygons();
        assertTrue('4', obstacle.tryBodyCollision(car));
        car.myFrame.setXYU(0, 4, 0);
        car.myCarDynamicEngine.saveState();
        car.myFrame.setXYU(3, 7, 0);
        car.myCarDynamicEngine.myHull.updateBorder();
        car.myCarDynamicEngine.myHull.updatePolygons();
        assertFalse('5', obstacle.tryBodyCollision(car));
    }

}
}