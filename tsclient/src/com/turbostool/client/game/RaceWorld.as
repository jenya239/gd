package com.turbostool.client.game
{
import com.turbostool.client.dynamicEngine.AreaStorage;
import com.turbostool.client.dynamicEngine.BodyBorder;
import com.turbostool.client.game.route.ObstacleModel;
import com.turbostool.client.geom.*;
import com.turbostool.client.newGraphic.NGDrawable;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.*;

public class RaceWorld {
    private var myEnvironmentComponents:Collection;
    private var myStartLinePiece:Piece2d;
    private var myPath:Vector2dSequence;
    private var myCameraPath: Vector2dSequence;
    private var myRouteName:String;
    private var myObstacleModels:ArrayList;
    private var myMinimapObstacleModels:ArrayList;
    private var myAreaStorage: AreaStorage = new AreaStorage();
    private var myNGDrawables: ArrayList;
    private var myQueueStorage: TraceQueueStorage = new TraceQueueStorage();
    private var _isReverse: Boolean = false;

    public function get isReverse():Boolean
    {
        return _isReverse;
    }

    public function RaceWorld(environmentComponents:Collection, startLinePiece:Piece2d, path:Vector2dSequence, routeName:String, obstacleModels:ArrayList)
    {
        myEnvironmentComponents = environmentComponents;
        myStartLinePiece = startLinePiece;
        myPath = path;
        myRouteName = routeName;
        myObstacleModels = obstacleModels;
        myNGDrawables = new ArrayList();
    }

    public function getName():String
    {
        return myRouteName;
    }

    public function getComponents():Collection
    {
        var list:ArrayList = new ArrayList();

        list.addCollection(myEnvironmentComponents);
        //list.addItem(this);

        return list;
    }

    public function getNGDrawables(): ReadOnlyCollection
    {
        return new ReadOnlyCollection(myNGDrawables);
    }

    public function addNGDrawable(drawable: NGDrawable): void
    {
        myNGDrawables.addItem(drawable);
    }

    public function addNGDrawables(drawables: Collection): void
    {
        var it: Iterator = drawables.iterator();
        while (it.hasNext())
        {
            var drawable: NGDrawable = it.next() as NGDrawable;
            addNGDrawable(drawable);
        }
    }

    public function getEnvironmentComponents():Collection
    {
        return myEnvironmentComponents;
    }

    public function getStartLine():Piece2d
    {
        return myStartLinePiece;
    }

    public function getStartPositionFrame():LocalFrame2d
    {
        const rast:Number = 2.6;
        var ort:Ort2d = myStartLinePiece.getPieceVector().ortogonal2d().ort2d();
        var r:Vector2d = myStartLinePiece.myBegin.sum2d(myStartLinePiece.getPieceVector().numberProduct2d(1 / 2).difference2d(ort.numberProduct2d(rast)));
        var angle:Number = myStartLinePiece.getPieceVector().getAngle() - Math.PI;
        return new LocalFrame2d(r, angle);
    }

    public function getPath():Vector2dSequence
    {
        return myPath;
    }

    public function getCameraPath():Vector2dSequence
    {
        return  myCameraPath == null ? myPath : myCameraPath;
    }

    public function setCameraPath(camera:Vector2dSequence):void
    {
        myCameraPath = camera;
    }

    public function setStartLine(piece:Piece2d):void
    {
        myStartLinePiece = piece;
    }

    public function getDynamics():Collection
    {
        return new ArrayList();
    }

    public function getCollidables():Collection
    {
        return myObstacleModels;
        //return myMinimapObstacleModels;
    }

    public function addObstacleModel(obstacle: ObstacleModel): void
    {
        var iterator:  Iterator = obstacle.mySequence.pieceIterator();
        while (iterator.hasNext())
        {
            var piece: Piece2d = iterator.next() as Piece2d;
            var seq: Vector2dSequence = new Vector2dSequence();
            seq.addVertex(piece.myBegin);
            seq.addVertex(piece.myEnd);
            myAreaStorage.addObstacle(new ObstacleModel(seq));
        }
        myObstacleModels.addItem(obstacle);
    }

    public function addMinimapObstacleModel(obstacle: ObstacleModel): void
    {
        var iterator:  Iterator = obstacle.mySequence.pieceIterator();
        while (iterator.hasNext())
        {
            var piece: Piece2d = iterator.next() as Piece2d;
            var seq: Vector2dSequence = new Vector2dSequence();
            seq.addVertex(piece.myBegin);
            seq.addVertex(piece.myEnd);
            myAreaStorage.addObstacle(new ObstacleModel(seq));
        }
        myMinimapObstacleModels.addItem(obstacle);
    }

    public function getObstacles(border: BodyBorder): Collection
    {
        return myAreaStorage.getNearestObstacles(border);
    }

    public function addTrace(begin: Vector2d, end: Vector2d): void
    {
        if (begin.equals(end)) return;
        var center: Vector2d = begin.sum2d(end).multiply2d(0.5);
        var queue: TraceQueue = myQueueStorage.getQueue(center);
        queue.setItem(begin, end);
    }

    public function set myReverse(val: Boolean): void
    {
        if (val == _isReverse) return;
        _isReverse = val;
        var temp: Vector2d = myStartLinePiece.myBegin.clone() as Vector2d;
        myStartLinePiece.myEnd.copyTo2d(myStartLinePiece.myBegin);
        temp.copyTo2d(myStartLinePiece.myEnd);
        myCameraPath.reverse();
    }

}
}