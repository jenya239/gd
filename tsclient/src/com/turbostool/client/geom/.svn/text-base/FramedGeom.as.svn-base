package com.turbostool.client.geom
{
import com.turbostool.client.utils.Clonable;
import com.turbostool.client.utils.Ort2d;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Vector2d;

public class FramedGeom implements IGeomParticle, IGeomed
{
    private var myFrameProperty:LocalFrame2d;
    private var myGeomProperty:IGeomParticle;

    public function FramedGeom(frame:LocalFrame2d, geom:IGeomParticle) {
        myFrameProperty = frame;
        myGeomProperty = geom;
    }

    public function calcCollisionVectors(
            previous:IGeomParticle,
            second:IGeomParticle,
            secondPrevious:IGeomParticle,
            collisionPoint:Vector2d,
            normal:Ort2d
            ):void {
        if (!(previous is FramedGeom))
            throw new TSError('previous должно быть FramedGeom');
        var firstParticle:IGeomParticle = getGlobalParticle();
        var firstPreviousParticle:IGeomParticle = (previous as FramedGeom).getGlobalParticle();
        var secondParticle:IGeomParticle = second;
        var secondPreviousParticle:IGeomParticle = secondPrevious;
        if (second is FramedGeom) {
            if (!(secondPrevious is FramedGeom))
                throw new TSError('secondPrevious должно быть FramedGeom');
            secondParticle = (secondParticle as FramedGeom).getGlobalParticle();
            secondPreviousParticle = (secondPreviousParticle as FramedGeom).getGlobalParticle();
        }
        firstParticle.calcCollisionVectors(firstPreviousParticle,
                secondParticle,
                secondPreviousParticle,
                collisionPoint,
                normal
                );
    }

    public function checkCollision(geom:IGeomParticle):Boolean {
        var firstParticle:IGeomParticle = getGlobalParticle();
        var secondParticle:IGeomParticle = geom;
        if (geom is FramedGeom) {
            secondParticle = (secondParticle as FramedGeom).getGlobalParticle();
        }
        return firstParticle.checkCollision(secondParticle);
    }

    public function copyTo(geom:IGeomParticle):void {
        if (!(geom is FramedGeom))
            throw new TSError(geom + ' не FramedGeom. Не скопировать');
        var fg:FramedGeom = (geom as FramedGeom);
        myFrame.copyTo(fg.myFrame);
        myGeom.copyTo(fg.myGeom);
    }

    public function createGlobal(localFrame:LocalFrame2d):IGeomParticle {
        var res:FramedGeom = new FramedGeom(localFrame.superposition(myFrame), myGeom);
        return res;
    }

    public function clone():Clonable {
        var res:FramedGeom = new FramedGeom(myFrame.clone() as LocalFrame2d, myGeom.clone() as IGeomParticle);
        return res;
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is FramedGeom)) return false;
        var fr:FramedGeom = (o as FramedGeom);
        return myFrame.equals(fr.myFrame) && myGeom.equals(fr.myGeom);
    }

    public function get myFrame():LocalFrame2d {
        return myFrameProperty;
    }

    public function set myFrame(setValue:LocalFrame2d):void {
        myFrameProperty = setValue;
    }

    public function getGeom():IGeomParticle {
        return myGeomProperty;
    }

    public function get myGeom():IGeomParticle {
        return myGeomProperty;
    }

    public function set myGeom(setValue:IGeomParticle):void {
        myGeomProperty = setValue;
    }

    public function getGlobalParticle():IGeomParticle {
        return myGeom.createGlobal(myFrame);
    }


}
}