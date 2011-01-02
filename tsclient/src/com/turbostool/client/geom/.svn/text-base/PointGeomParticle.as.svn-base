package com.turbostool.client.geom
{
import com.turbostool.client.utils.*;

public class PointGeomParticle implements IGeomParticle, IPosition2d {
    private var myRProperty:Vector2d;

    public function checkCollision(geom:IGeomParticle):Boolean {
        if (geom is PointGeomParticle) return false;
        return geom.checkCollision(this);
    }

    public function get myR():Vector2d {
        return myRProperty;
    }

    public function set myR(setValue:Vector2d):void {
        myRProperty = setValue;
    }

    public function PointGeomParticle(r:Vector2d) {
        myR = r;
    }

    public function calcCollisionVectors(
            previous:IGeomParticle,
            second:IGeomParticle,
            secondPrevious:IGeomParticle,
            collisionPoint:Vector2d,
            normal:Ort2d
            ):void {
        if (!(previous is PointGeomParticle))
            throw new TSError('previous должно быть PointGeomParticle');
        if (second is PointGeomParticle) {
            if (!(secondPrevious is PointGeomParticle))
                throw new TSError('secondPrevious должно быть PointGeomParticle');
            throw new TSError('точка не может столкнуться с точкой');
            return;
        }
        second.calcCollisionVectors(secondPrevious, this, previous, collisionPoint, normal);
    }

    public function clone():Clonable {
        return new PointGeomParticle(myR.clone() as Vector2d);
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is PointGeomParticle)) return false;
        var point:PointGeomParticle = o as PointGeomParticle;
        return myR.equals(point.myR);
    }

    public function copyTo(geom:IGeomParticle):void {
        if (!(geom is PointGeomParticle))
            throw new TSError(geom + ' не PointGeomParticle. Не скопировать');
        (geom as PointGeomParticle).myR = myR;
    }

    public function createGlobal(localFrame:LocalFrame2d):IGeomParticle {
        var point:PointGeomParticle = clone() as PointGeomParticle;
        point.myR = localFrame.getGlobal(myR);
        return point;
    }
}
}