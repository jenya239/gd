package com.turbostool.client.dynamicEngine.test {

import com.turbostool.client.dynamicEngine.AreaStorage;
import com.turbostool.client.dynamicEngine.BodyBorder;
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;

public class AreaStorageTest extends TestCase    {

    public function testAddObstacle(): void {
        var storage100: AreaStorage = new AreaStorage(100);
        var storage10: AreaStorage = new AreaStorage(10);

        var sequence15_50 : Vector2dSequence = new Vector2dSequence();
        sequence15_50.addVertex(new Vector2d(15, 15));
        sequence15_50.addVertex(new Vector2d(50, 50));
        var obstacle15_50: ObstacleModel = new ObstacleModel(sequence15_50);

        var sequence_minus_15_50 : Vector2dSequence = new Vector2dSequence();
        sequence_minus_15_50.addVertex(new Vector2d(-15, -15));
        sequence_minus_15_50.addVertex(new Vector2d(-50, -50));
        var obstacle_minus_15_50 : ObstacleModel = new ObstacleModel(sequence_minus_15_50);

        storage100.addObstacle(obstacle15_50);
        storage10.addObstacle(obstacle15_50);

        //fail("rewrite!!!");


        assertEquals(0, storage10.getNearestObstacles(createSmallBorder(90, 90)).length());
        assertEquals(1, storage100.getNearestObstacles(createSmallBorder(90, 90)).length());

        storage100.addObstacle(obstacle_minus_15_50);
        storage10.addObstacle(obstacle_minus_15_50);

        assertEquals(0, storage10.getNearestObstacles(createSmallBorder(110, 110)).length());
        assertEquals(0, storage100.getNearestObstacles(createSmallBorder(110, 110)).length());
    }

    private function createSmallBorder(x: Number, y: Number): BodyBorder {
        var b: BodyBorder = new BodyBorder();
        b.myMinX = x;
        b.myMaxX = x + 1;
        b.myMinY = y;
        b.myMaxY = y + 1;
        return b;
    }

}
}