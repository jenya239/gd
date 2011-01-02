package com.turbostool.client.geom
{
import com.turbostool.client.utils.Clonable;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Vector2d;

/**
 * Проще всего прямоугольником в данном случае считать нечто, что возвращает width, height и R
 * способом, который реализован
 */
public class Rectangle extends Polygon{

    public function Rectangle(center:Vector2d, width:Number, height:Number, insideActive:Boolean = true) {
        var dx:Number = width / 2;
        var dy:Number = height / 2;
        super(insideActive);
        super.addVertex(new Vector2d(center.myX - dx, center.myY - dy));
        super.addVertex(new Vector2d(center.myX + dx, center.myY - dy));
        super.addVertex(new Vector2d(center.myX + dx, center.myY + dy));
        super.addVertex(new Vector2d(center.myX - dx, center.myY + dy));
    }

    override public function addVertex(v:Vector2d):Boolean {
        throw new TSError('У прямоугольника только четыре угла');
    }

    public function get myWidth():Number {
        return (getVertices().getItemAt(1) as Vector2d).myX - (getVertices().getItemAt(0) as Vector2d).myX;
    }

    public function set myWidth(setValue:Number):void {
        var dx:Number = setValue / 2;
        var r:Vector2d = myR;
        (getVertices().getItemAt(0) as Vector2d).myX = r.myX - dx;
        (getVertices().getItemAt(1) as Vector2d).myX = r.myX + dx;
        (getVertices().getItemAt(2) as Vector2d).myX = r.myX + dx;
        (getVertices().getItemAt(3) as Vector2d).myX = r.myX - dx;
    }

    public function get myHeight():Number {
        return (getVertices().getItemAt(2) as Vector2d).myY - (getVertices().getItemAt(1) as Vector2d).myY;
    }

    public function set myHeight(setValue:Number):void {
        var dy:Number = setValue / 2;
        var r:Vector2d = myR;
        (getVertices().getItemAt(0) as Vector2d).myY = r.myY - dy;
        (getVertices().getItemAt(1) as Vector2d).myY = r.myY - dy;
        (getVertices().getItemAt(2) as Vector2d).myY = r.myY + dy;
        (getVertices().getItemAt(3) as Vector2d).myY = r.myY + dy;
    }

    /**
     * return центр прямоугольника
     */
    public function get myR():Vector2d {
        return (getVertices().getItemAt(0) as Vector2d).sum2d(
                (
                        (getVertices().getItemAt(2) as Vector2d)
                                .difference2d_shared(getVertices().getItemAt(0) as Vector2d)
                        ).numberProduct2d(0.5)
                );
    }

    public function set myR(setValue:Vector2d):void {
        var dx:Number = myWidth / 2;
        var dy:Number = myHeight / 2;
        (getVertices().getItemAt(0) as Vector2d).myX = setValue.myX - dx;
        (getVertices().getItemAt(0) as Vector2d).myY = setValue.myY - dy;
        (getVertices().getItemAt(1) as Vector2d).myX = setValue.myX + dx;
        (getVertices().getItemAt(1) as Vector2d).myY = setValue.myY - dy;
        (getVertices().getItemAt(2) as Vector2d).myX = setValue.myX + dx;
        (getVertices().getItemAt(2) as Vector2d).myY = setValue.myY + dy;
        (getVertices().getItemAt(3) as Vector2d).myX = setValue.myX - dx;
        (getVertices().getItemAt(3) as Vector2d).myY = setValue.myY + dy;
    }

    override public function clone():Clonable {
        return new Rectangle(myR, myWidth, myHeight);
    }

    override public function removeAllVertices():void {
        throw new TSError('У прямоугольника всегда должно быть четыре угла');
    }

    override public function copyTo(geom:IGeomParticle):void {
        if (!(geom is Rectangle))
            throw new TSError(geom + ' не Rectangle. Не скопировать');
        var rect:Rectangle = geom as Rectangle;
        rect.myR = myR;
        rect.myWidth = myWidth;
        rect.myHeight = myHeight;
    }

    /**
     * return Vector2dSequence, а не Rectangle, хотя ведь и написано IGeomParticle...
     */
    override public function createGlobal(localFrame:LocalFrame2d):IGeomParticle {
        return super.createGlobal(localFrame);
    }

    override public function createParallel(roadWidth:Number):Polygon {
        return new Rectangle(
                myR,
                myWidth + 2 * roadWidth,
                myHeight + 2 * roadWidth,
                myInsideActive
                );
    }

    override public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Rectangle)) return false;
        var rect:Rectangle = o as Rectangle;
        return super.equals(rect);
    }
}
}