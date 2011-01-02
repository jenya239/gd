package com.turbostool.client.game.route {

import com.turbostool.client.dynamicEngine.*;
import com.turbostool.client.game.components.car.CarModel;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.geom.Vector2dKeeper;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Iterator;

public class ObstacleModel{
    private var myGeom:Vector2dSequence;
    private var myPieces:ArrayList;
    private var myBodyBorder:BodyBorder;
    private var myTempPiece:Piece2d;
    private var myVectorKeeper:Vector2dKeeper;

    private static var instCount: Number = 0;
    private var id: Number;

    public function getMyId(): Number {
        return id;
    }

    public function ObstacleModel(geom:Vector2dSequence) {
        id = instCount;
        instCount++;

        myGeom = geom;
        myPieces = new ArrayList();
        var it:Iterator = geom.pieceIterator();
        while (it.hasNext()) {
            var piece:Object = it.next();
            myPieces.addItem(piece);
        }
        myBodyBorder = new BodyBorder();
        myTempPiece = new Piece2d(new Vector2d(0, 0), new Vector2d(0, 0));
        myVectorKeeper = new Vector2dKeeper();
    }

    /**
     * Расчет результата столкновения машины со стеной
     * @param collisionPoint глобальные координаты точки столкновения (скорее всего это один из углов кузова авто)
     * @param normal нормаль к отрезку стены. Куда направлена - хз
     * @param body скорее всего это кузов автомобиля
     * @param piece отрезок соединяющий текущее положение столкнувшегося угла кузова с положением в пред. тик
     */
    private static const CRITICAL_ANGLE: Number = 45;
    private static const WASTE_COEF: Number = 0.5;

    public function collideBody(collisionPoint:Vector2d, normal:Ort2d, body: CarModel, piece:Piece2d):void {
        var cos: Number = Math.abs(body.myOrientation.scalarProduct(normal));

        var wallOrt: Vector2d = normal.ortogonal2d(); //optimization передать сразу wallOrt, все равно
        //в начале normal вычисляется через него
        //если проекция направления тела на wallOrt положительная, то тело должно стать направлено
        //вдоль wallOrt, иначе в другую сторону
        var proj: Number = wallOrt.scalarProduct(body.myOrientation);
        var angle0: Number = body.myFrame.myAngle;
        if (proj > 0) {
            body.setOrientationBy(wallOrt);
        } else {
            body.setOrientationBy(wallOrt.invertion2d());
        }

        var temp: Vector2d = Vector2d.createFrom3d(body.myVelocity.getOrtogComponentBy(normal));
        if (cos > Math.cos((90 - CRITICAL_ANGLE) * Math.PI / 180)) {
            body.myVelocity = temp.numberProduct2d(body.myVelocity.length() * WASTE_COEF / temp.length());
        } else {
            body.myVelocity = temp;
        }

        //посчитаем на какое растояние нужно сдвинуть авто вдоль оси, чтобы авто задом сразу не сталкивалось
        //var alpha: Number = body.myFrame.myAngle - angle0;
        //var dist: Number = body.myHull.myLength / 2 + (body.myHull.myWidth / 2) * (Math.cos(alpha) - 1) / Math.sin(alpha);
        //body.myFrame.myR.addMultiplied(body.myOrientation, dist);

        //trace(body.myOrientation.getAngleTo(normal) * 180 / Math.PI);
        //body.myFrame.myAngle = normal.getAngle();
        //body.myVelocity.setZero();
        //body.myAngularVelocity = 0;
        //body.fullStop();
        /*
         if ( normal.scalarProductDifference(piece.myEnd,piece.myBegin) * body.myVelocity.scalarProduct(normal) > Utils.EPSILON ){
         body.myVelocity.addMultiplied(body.myVelocity.setProjectionToVector(normal),-(1+myWasteCoef));
         }else{
         body.myAngularVelocity = 0;
         }*/
    }

    public function tryBodyCollision(body:CarModel): Vector2d {
        myBodyBorder = body.getBorder();
        var currPolygon:Polygon = body.myGlobalPolygon;
        var prevPolygon:Polygon = body.myPreviousGlobalPolygon;
        var pieces:Array = myPieces.toArray();
        var wallPiece:Piece2d;
        for (var i:uint = 0; i < pieces.length; i++) {
            wallPiece = pieces[i];
            if (myBodyBorder.canIntersect(wallPiece)) {
                var arr: Array = tryBuildCollisionParams(body, wallPiece, currPolygon, prevPolygon);
                if (arr != null) {
                    body.rollback();
                    collideBody(arr[0], arr[1], arr[2], arr[3]);
                    return arr[0];
                }
            }
        }
        return null;
    }

    /**
     *
     * @param body
     * @param wallPiece
     * @param currPol полигон-прямоугольник кузова в глобальной СО
     * @param prevPol -||- в предыдущий тик
     * @return collisionPoint, normal, body, intersectionPiece
     */
    private function tryBuildCollisionParams(body:CarModel, wallPiece:Piece2d, currPol:Polygon, prevPol:Polygon): Array {
        var currArray:Array = currPol.getVertices().toArray();
        var prevArray:Array = prevPol.getVertices().toArray();
        for (var i:uint = 0; i < currArray.length; i++) {
            prevArray[i].copyTo2d(myTempPiece.myBegin);
            currArray[i].copyTo2d(myTempPiece.myEnd);
            if (wallPiece.intersects(myTempPiece, null, myVectorKeeper)) {
                //body.rollback();
                //collideBody(myTempPiece.myBegin, wallPiece.getPieceVector().ortogonal2d().ort2d(), body, myTempPiece);
                //return true;
                return new Array(myTempPiece.myBegin, wallPiece.getPieceVector().ortogonal2d().ort2d(), body, myTempPiece);
            }
        }
        for (var j:uint = 1; j <= 4; j++) {
            currArray[j - 1].copyTo2d(myTempPiece.myBegin);
            currArray[j % 4].copyTo2d(myTempPiece.myEnd);
            if (wallPiece.intersects(myTempPiece, null, myVectorKeeper)) {

                var normal:Ort2d = myTempPiece.getPieceVector().ortogonal2d().ort2d();
                var currCenter:Vector2d = new Vector2d(0, 0);
                var prevCenter:Vector2d = new Vector2d(0, 0);
                currCenter.setXY(
                        (myTempPiece.myBegin.myX + myTempPiece.myEnd.myX) / 2,
                        (myTempPiece.myBegin.myY + myTempPiece.myEnd.myY) / 2
                        );
                prevCenter.setXY(
                        (prevArray[j - 1].myX + prevArray[j % 4].myX) / 2,
                        (prevArray[j - 1].myY + prevArray[j % 4].myY) / 2
                        );
                var piece:Piece2d = new Piece2d(prevCenter, currCenter);
                //body.rollback();
                //collideBody(myTempPiece.myBegin, normal, body, piece);
                //return true;
                return new Array(myTempPiece.myBegin, normal, body, piece);
            }
        }
        return null;
    }

    public function get mySequence(): Vector2dSequence {
        return myGeom;
    }

    public function get fuckToString():String {
        var result: String = '{ ';
        var it: Iterator = myGeom.vertexIterator();
        while (it.hasNext()) {
            var v: Vector2d = it.next() as Vector2d;
            result += v.toString() + ' ';
        }
        result += '}';
        return result;
    }

}
}