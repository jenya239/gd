package com.turbostool.client.geom {
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

public class Piece2d {
    public var myBegin:Vector2d;
    public var myEnd:Vector2d;

    public function Piece2d(begin:Vector2d, end:Vector2d) {
        myBegin = begin;
        myEnd = end;
    }

    public function distanceTo(point:Vector2d):Number {
        var line:Piece2d = this;
        var h:Number = distanceToLine(point);
        var result:Number = h;
        var l:Number = line.getPieceVector().length();
        var l_1:Number = line.myBegin.difference2d(point).length();
        var l_2:Number = line.myEnd.difference2d(point).length();
        if (l_1 * l_1 > h * h + l * l || l_2 * l_2 > h * h + l * l) {
            result = Math.min(l_1, l_2);
        }
        return Math.abs(result);
    }

    public function distanceToLine(point:Vector2d):Number {
        var line:Piece2d = this;
        var length:Number = line.getPieceVector().length();
        if (length < Utils.EPSILON) {
            return line.myBegin.difference2d_shared(point).length();
        }
        var cos:Number = ( line.myBegin.myY - line.myEnd.myY ) / length;
        var sin:Number = ( line.myEnd.myX - line.myBegin.myX) / length;
        var p:Number = (line.myBegin.myX * line.myEnd.myY - line.myBegin.myY * line.myEnd.myX) / length;

        if (p < 0) {
            cos = -cos;
            sin = -sin;
            p = - p;
        }
        var result:Number = point.myX * cos + point.myY * sin + p;
        return result;
    }

    public function intersectsOld(piece:Piece2d, intersectionPoint:Vector2d = null):Boolean {
        //В дельфи отрезок задавался начальной точкой и вектором отрезка, а здесь - двумя точками
        var cross:Boolean;
        var aortog:Vector2d;
        var bortog:Vector2d;
        var abortog:Number;
        var aortogb:Number;
        var lb:Number;
        var la:Number, ua0:Number, ub0:Number, ka:Number, kb:Number, ua:Number, ub:Number;
        var a0:Vector2d, a:Vector2d, b0:Vector2d, b:Vector2d, a0diffb0:Vector2d, b0diffa0:Vector2d, r:Vector2d, ortb:Vector2d, orta:Vector2d;
        a0 = myBegin;
        a = myEnd.difference2d(myBegin);
        b0 = piece.myBegin;
        b = piece.myEnd.difference2d(piece.myBegin);

        cross = false;
        r = Vector2d.getZero();
        aortog = new Vector2d(-a.myY, a.myX);
        bortog = new Vector2d(-b.myY, b.myX);
        abortog = a.scalarProduct(bortog);
        aortogb = b.scalarProduct(aortog);
        lb = b.length();
        la = a.length();
        a0diffb0 = a0.difference2d(b0);
        b0diffa0 = b0.difference2d(a0);
        if (Utils.equal(a0diffb0.length(), 0)) {
            cross = true;
            r = a0;
        } else if ((lb == 0) && (la == 0)) {
            cross = (myBegin.myX == piece.myBegin.myX) && (myBegin.myY == piece.myBegin.myY);
            if (cross) r = a0;
        } else if (la == 0) {
            if (Utils.equal(a0diffb0.scalarProduct(bortog), 0)) {
                ortb = b.numberProduct2d(1 / lb);
                ua0 = ortb.scalarProduct(a0diffb0);
                if (Utils.inRange(ua0, 0, lb)) {
                    r = a0;
                    cross = true;
                }
            }
        } else if (Utils.equal(lb, 0)) {
            if (Utils.equal(b0diffa0.scalarProduct(aortog), 0)) {
                orta = a.numberProduct2d(1 / la);
                ub0 = b0diffa0.scalarProduct(orta);
                if (Utils.inRange(ub0, 0, la)) {
                    r = b0;
                    cross = true;
                }
            }
        } else if (!(Utils.equal(abortog, 0))) {
            ka = b0diffa0.scalarProduct(bortog) / abortog;
            kb = a0diffb0.scalarProduct(aortog) / aortogb;
            r = a0.sum2d(a.numberProduct2d(ka));
            cross = Utils.inRange(ka, 0, 1) && Utils.inRange(kb, 0, 1);
        } else if (!(Utils.equal(b0.difference2d(a0).scalarProduct(bortog), 0))) {
            cross = false;
        } else {
            ortb = b.numberProduct2d(1 / lb);
            ua0 = a0diffb0.scalarProduct(ortb);
            ua = a.scalarProduct(ortb);
            if (Utils.isBetween(0, ua0, ua)) {
                r = b0;
                cross = true;
            } else if (Utils.isBetween(lb, ua0, ua)) {
                r = b0.sum2d(b);
                cross = true;
            }
        }
        if (intersectionPoint != null) {
            intersectionPoint.myX = r.myX;
            intersectionPoint.myY = r.myY;
        }
        return cross;
    }


    public function intersects(piece:Piece2d, intersect:Vector2d = null, vectorKeeper:Vector2dKeeper = null):Boolean {
        //В дельфи отрезок задавался начальной точкой и вектором отрезка, а здесь - двумя точками
        var cross:Boolean;
        var aortog:Vector2d;
        var bortog:Vector2d;
        var abortog:Number;
        var aortogb:Number;
        var lb:Number;
        var la:Number, ua0:Number, ub0:Number, ka:Number, kb:Number, ua:Number, ub:Number;
        var a0:Vector2d, a:Vector2d, b0:Vector2d, b:Vector2d, a0diffb0:Vector2d, b0diffa0:Vector2d, r:Vector2d, ortb:Vector2d, orta:Vector2d;
        var temp:Number;
        cross = false;
        a0 = myBegin;
        b0 = piece.myBegin;
        if (vectorKeeper == null) {
            a = myEnd.difference2d(myBegin);
            b = piece.myEnd.difference2d(piece.myBegin);
            aortog = new Vector2d(-a.myY, a.myX);
            bortog = new Vector2d(-b.myY, b.myX);
        } else {
            a = myEnd.setDifferenceTo(myBegin, vectorKeeper.myDifference1);
            b = piece.myEnd.setDifferenceTo(piece.myBegin, vectorKeeper.myDifference2);
            aortog = a.setOrtogonal2dTo(vectorKeeper.myRotated1);
            bortog = b.setOrtogonal2dTo(vectorKeeper.myRotated2);
        }
        abortog = a.scalarProduct(bortog);

        lb = b.length();
        la = a.length();
        temp = bortog.scalarProductDifference(a0, b0);
        //a0diffb0 = a0.difference2d(b0);
        //b0diffa0 = b0.difference2d(a0);
        //if (a0.equals(b0)) {
        if (Utils.equal(a0.myX, b0.myX) && Utils.equal(a0.myY, b0.myY)) {
            cross = true;
        } else if ((lb == 0) && (la == 0)) {
            cross = (myBegin.myX == piece.myBegin.myX) && (myBegin.myY == piece.myBegin.myY);
        } else if (la == 0) {
            //Utils.equal(bortog.scalarProductDifference(a0,b0), 0)
            //temp = bortog.scalarProductDifference(a0,b0);
            //так надо
            if (temp > -0.000000000001 && temp < 0.000000000001) {
                //ortb = b.numberProduct2d(1/lb);
                //ua0 = ortb.scalarProductDifference(a0,b0);
                ua0 = b.scalarProductDifference(a0, b0) / lb;
                //Utils.inRange(ua0, 0, lb)
                if (0 <= ua0 && ua0 <= lb) {
                    cross = true;
                }
            }
            //Utils.equal(lb, 0)
        } else if (lb < 0.000000000001) {
            //Utils.equal(aortog.scalarProductDifference(b0,a0), 0)
            temp = aortog.scalarProductDifference(b0, a0);
            if (temp > -0.000000000001 && temp < 0.000000000001) {
                //orta = a.numberProduct2d(1/la);
                //ub0 = orta.scalarProductDifference(b0,a0);
                ub0 = a.scalarProductDifference(b0, a0) / la;
                //Utils.inRange(ub0, 0, la)
                if (0 <= ub0 && ub0 <= la) {
                    cross = true;
                }
            }
            //!(Utils.equal(abortog, 0))
        } else if (abortog < -0.000000000001 || abortog > 0.000000000001) {
            //ka = bortog.scalarProductDifference(b0,a0) / abortog;
            ka = -temp / abortog;
            aortogb = b.scalarProduct(aortog);
            kb = aortog.scalarProductDifference(a0, b0) / aortogb;
            //cross = Utils.inRange(ka, 0, 1) && Utils.inRange(kb, 0, 1);
            cross = ka >= 0 && ka <= 1 && kb >= 0 && kb <= 1;
        }  //!(Utils.equal(bortog.scalarProductDifference(a0,b0), 0))
        else if (temp < -0.000000000001 || temp > 0.000000000001) {
                cross = false;
            } else {
                //ortb = b.numberProduct2d(1/lb);
                //b.copyTo2d(aortog);
                //aortog.multiply2d(1/lb);
                //ua0 = aortog.scalarProductDifference(a0,b0);
                ua0 = b.scalarProductDifference(a0, b0) / lb;
                ua = a.scalarProduct(b) / lb;
                //Utils.isBetween(0, ua0, ua)
                if (ua0 * ua <= 0) {
                    cross = true;
                } else if (Utils.isBetween(lb, ua0, ua)) {
                    cross = true;
                }
            }
        return cross;
    }

    public function getPieceVector():Vector2d {
        return myEnd.difference2d_shared(myBegin);
    }

    public function getEquationCoefficients():Array {
        var res:Array;
        if (Utils.equal(myBegin.myX, myEnd.myX)) {
            throw new TSError('Невозможно вычислить уравнение вертикальной прямой');
        } else {
            res = new Array();
            res[0] = (myEnd.myY - myBegin.myY) / (myEnd.myX - myBegin.myX);
            res[1] = myBegin.myY - res[0] * myBegin.myX;
        }
        return res;
    }

    /**
     * Прямые пересекаются, если есть хотя бы одна общая точка
     */
    public function lineIntersects(piece:Piece2d, intersectionPoint:Vector2d = null):Boolean {
        var c1:Array;
        var c2:Array;
        if (myBegin.equals(myEnd) || piece.myBegin.equals(piece.myEnd)) {
            throw new TSError('Невозможно определить прямую по одной точке');
        }
        if (Utils.equal(myBegin.myX, myEnd.myX)
                && Utils.equal(piece.myBegin.myX, piece.myEnd.myX)
                ) {
            if (Utils.equal(myBegin.myX, piece.myBegin.myX)) {
                if (intersectionPoint != null) myBegin.copyTo(intersectionPoint);
                return true;
            } else {
                return false;
            }
        }
        if (Utils.equal(myBegin.myX, myEnd.myX)) {
            if (intersectionPoint != null) {
                c2 = piece.getEquationCoefficients();
                intersectionPoint.myX = myBegin.myX;
                intersectionPoint.myY = c2[0] * myBegin.myX + c2[1];
            }
            return true;
        }
        if (Utils.equal(piece.myBegin.myX, piece.myEnd.myX)) {
            if (intersectionPoint != null) {
                c1 = getEquationCoefficients();
                intersectionPoint.myX = piece.myBegin.myX;
                intersectionPoint.myY = c1[0] * piece.myBegin.myX + c1[1];
            }
            return true;
        }
        c1 = getEquationCoefficients();
        c2 = piece.getEquationCoefficients();
        var det:Number = c2[0] - c1[0];
        if (Utils.equal(det, 0)) {
            if (Utils.equal(c1[1], c2[1])) {
                myBegin.copyTo(intersectionPoint);
                return true;
            } else {
                return false;
            }
        } else {
            var detX:Number = c1[1] - c2[1];
            var detY:Number = c2[0] * c1[1] - c1[0] * c2[1];
            if (intersectionPoint != null) {
                intersectionPoint.myX = detX / det;
                intersectionPoint.myY = detY / det;
            }
        }
        return true;
    }

    public static function getXOrt():Piece2d {
        return new Piece2d(new Vector2d(0, 0), new Vector2d(1, 0));
    }

    public function equals(o:Object):Boolean {
        if (! (o is Piece2d)) {
            return false;
        }
        var piece:Piece2d = o as Piece2d;
        return myBegin.equals(piece.myBegin) && myEnd.equals(piece.myEnd);
    }

    public function getMiddle(): Vector2d {
        return myBegin.sum2d(myEnd).numberProduct2d(0.5);
    }

    public function getProjection(v: Vector2d): Vector2d {
        var res: Vector2d = getPieceVector().getProjection(v.difference2d(myBegin)).sum2d(myBegin);
        return res;
    }

    public function get length(): Number {
        return myBegin.distanceTo(myEnd);
    }

    public function getNearest(point: Vector2d): Vector2d {
        var proj: Vector2d = getProjection(point);
        var len: Number = myEnd.distanceTo(myBegin);
        var beg_dist: Number = proj.distanceTo(myBegin);
        var end_dist: Number = proj.distanceTo(myEnd);
        if ((beg_dist > len) || (end_dist > len)) {
            if (beg_dist > end_dist) {
                return myEnd;
            } else {
                return myBegin;
            }
        }
        return proj;
    }

}
}