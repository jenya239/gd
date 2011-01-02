package com.turbostool.client.geom
{
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.Iterator;

public class Polygon extends Vector2dSequence {
    private var myInsideActiveProperty:Boolean;

    public function Polygon(insideActive:Boolean = true) {
        myInsideActive = insideActive;
    }

    public function getOutsidePoint():Vector2d {
        var it:Iterator = vertexIterator();
        var maxX:Number = (it.next() as Vector2d).myX;
        while (it.hasNext()) {
            var x:Number = (it.next() as Vector2d).myX;
            maxX = (x > maxX) ? x : maxX;
        }
        return new Vector2d(maxX + 1, 0);
    }

    public function isInside(r:Vector2d):Boolean {
        var it:Iterator = vertexIterator();
        while (it.hasNext()) {
            if (r.equals(it.next())) return true;
        }

        var testPiece:Piece2d = new Piece2d(r, getOutsidePoint());
        it = pieceIterator();
        var intersectCount:int = 0;
        while (it.hasNext()) {
            if (testPiece.intersects(it.next() as Piece2d)) {
                intersectCount++;
            }
        }
        return (intersectCount % 2) != 0;
    }

    public function get myInsideActive():Boolean {
        return myInsideActiveProperty;
    }

    public function set myInsideActive(setValue:Boolean):void {
        myInsideActiveProperty = setValue;
    }

    override public function clone():Clonable {
        var pol:Polygon = new Polygon();
        var it1:Iterator = vertexIterator();
        while (it1.hasNext()) {
            pol.addVertex((it1.next() as Vector2d).clone() as Vector2d);
        }
        pol.myInsideActive = myInsideActive;
        return pol;
    }

    public function createParallel(roadWidth:Number):Polygon {
        var it:Iterator;
        var piece:Piece2d;
        var newPiece:Piece2d;
        var pol:Polygon = new Polygon();
        it = pieceIterator();
        while (it.hasNext()) piece = it.next() as Piece2d;
        var shift:Vector2d =
                piece.getPieceVector().ortogonal2d().ort2d().numberProduct2d(roadWidth);
        var prevPiece:Piece2d = new Piece2d(piece.myBegin.sum2d(shift), piece.myEnd.sum2d(shift));
        it = pieceIterator();
        while (it.hasNext()) {
            piece = it.next() as Piece2d;
            shift = piece.getPieceVector().ortogonal2d().ort2d().numberProduct2d(roadWidth);
            newPiece = new Piece2d(piece.myBegin.sum2d(shift), piece.myEnd.sum2d(shift));
            var intersectPoint:Vector2d = Vector2d.getZero();
            newPiece.lineIntersects(prevPiece, intersectPoint);
            pol.addVertex(intersectPoint);
            prevPiece = newPiece;
        }
        return pol;
    }

    override public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Polygon)) return false;
        var pol:Polygon = o as Polygon;
        return super.equals(pol) && (myInsideActive == pol.myInsideActive);
    }

    override public function copyTo(geom:IGeomParticle):void {
        if (!(geom is Polygon))
            throw new TSError(geom + ' не Polygon. Не скопировать');
        super.copyTo(geom);
        (geom as Polygon).myInsideActive = myInsideActive;
    }

    override public function createGlobal(localFrame:LocalFrame2d):IGeomParticle {
        var pol:Polygon = new Polygon();
        pol.myInsideActive = myInsideActive;
        var it1:Iterator = vertexIterator();
        while (it1.hasNext()) {
            var r:Vector2d = it1.next() as Vector2d;
            pol.addVertex(localFrame.getGlobal(r));
        }
        return pol;
    }

    public function removeVertex(index: int): void {
        //need optimization
        getVertices().removeItem(getVertices().getItemAt(index));
    }

}
}