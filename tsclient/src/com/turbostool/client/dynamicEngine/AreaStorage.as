package com.turbostool.client.dynamicEngine
{
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.Iterator;

public class AreaStorage
{
    private static const GRID_SIZE: Number = 10;
    private var myGridSize: Number;

    private var myAreas:  Array = new Array();

    public function AreaStorage(gridSize: int = GRID_SIZE) {
        myGridSize = gridSize;
    }

    private var key : String = new String();

    private var area:  Area = new Area();

    private function getArea(x: int, y: int): Area {
        if (myAreas[x] == null) {
            myAreas[x] = new Array();
        }
        if (myAreas[x][y] != null) {
            return myAreas[x][y] as Area;
        } else {
            var area: Area = new Area();
            myAreas[x][y] = area;
            return area;
        }//*/
    }

    public function addObstacle(obstacle: ObstacleModel): void {
        var piece2d : Piece2d = obstacle.mySequence.pieceIterator().next() as Piece2d;
        var x1: int = Math.floor(piece2d.myBegin.myX / myGridSize);
        var x2: int = Math.floor(piece2d.myEnd.myX / myGridSize);
        var y1: int = Math.floor(piece2d.myBegin.myY / myGridSize);
        var y2: int = Math.floor(piece2d.myEnd.myY / myGridSize);
        //trace ('[' + obstacle + ']: {' + x1 + '; ' + y1 + '}; {' + x2 + '; ' + y2 + '}');
        for (var i: int = Math.min(x1, x2); i <= Math.max(x1, x2); i++) {
            for (var j: int = Math.min(y1, y2); j <= Math.max(y1, y2); j++) {
                getArea(i, j).addObstacle(obstacle);
                //trace ('    adding [' + obstacle + '] at {' + i + '; ' + j + '}');
            }
        }
    }


    private var result: ArrayList = new ArrayList();
    private var resultSet: Array = new Array();

    public function getNearestObstacles(border: BodyBorder): Collection {
        result.clear();
        resultSet = new Array();
        addObstacles(border.myMaxX, border.myMaxY);
        addObstacles(border.myMinX, border.myMaxY);
        addObstacles(border.myMaxX, border.myMinY);
        addObstacles(border.myMinX, border.myMaxY);
        return result;
    }

    private function addObstacles(inX:Number, inY:Number): void {
        var x: Number = Math.floor(inX / myGridSize);
        var y: Number = Math.floor(inY / myGridSize);
        var it:Iterator = getArea(x, y).getObstacles().iterator();
        while (it.hasNext()) {
            var om: ObstacleModel = it.next() as ObstacleModel;
            if (resultSet[om.getMyId()] == null) {
                resultSet[om.getMyId()] = om;
                result.addItem(om);
            }
        }
    }


    private function clear(): void {
        myAreas.clear();
    }
}
}