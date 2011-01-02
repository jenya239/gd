package com.turbostool.client.geom
{
import com.turbostool.client.utils.*;

public interface IGeomParticle extends Clonable
{
    function copyTo(geom:IGeomParticle):void;

    function createGlobal(localFrame:LocalFrame2d):IGeomParticle;

    function equals(o:Object):Boolean;
}
}