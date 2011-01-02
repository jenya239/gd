package com.turbostool.client.geom
{
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.*;

public class Vector2dSequence implements IGeomParticle {
    private var myVertices:ArrayList;

    public function Vector2dSequence() {
        myVertices = new ArrayList();
    }

    public function addVertex(v:Vector2d):Boolean {
        return myVertices.addItem(v);
    }

    public function addVertices(vc: Collection): void {
        myVertices.addCollection(vc);
    }

    public function getVertices():ArrayList {
        return myVertices;
    }

    public function get myVertexArray():Array {
        return myVertices.toArray();
    }

    public function vertexIterator():Iterator {
        return myVertices.iterator();
    }

    public function pieceIterator():Iterator {
        return new PieceIterator(this);
    }

    public function equals(o:Object):Boolean {
        if (o == this) return true;
        if (! (o is Vector2dSequence)) return false;
        var seq:Vector2dSequence = (o as Vector2dSequence);
        if (this.myVertices.length() != seq.myVertices.length()) return false;
        var it1:Iterator = vertexIterator();
        var it2:Iterator = seq.vertexIterator();
        while (it1.hasNext()) {
            if (!((it1.next() as Vector2d).equals(it2.next() as Vector2d))) {
                return false;
            }
        }
        return true;
    }

    private static function calcPointCollisionVectors(
            sequence:Vector2dSequence,
            piece:Piece2d,
            collisionPoint:Vector2d,
            normal:Ort2d
            ):void {
        var it:Iterator = sequence.pieceIterator();
        while (it.hasNext()) {
            var seqPiece:Piece2d = it.next() as Piece2d;
            if (piece.intersects(seqPiece, collisionPoint)) {
                var ort:Ort2d = seqPiece.getPieceVector().ortogonal2d().ort2d();
                if (ort.scalarProduct(piece.myBegin.difference2d_shared(seqPiece.myBegin)) < 0) {
                    ort = Ort2d.createFromVector2d(ort.numberProduct2d(-1));
                }
                normal.myX = ort.myX;
                normal.myY = ort.myY;
                return;
            }
        }
        throw new TSError(sequence + ':Vector2dSequence и '
                + piece + ':Piece2d не сталкивались');
    }

    public function clone():Clonable {
        var seq:Vector2dSequence = new Vector2dSequence;
        var it1:Iterator = vertexIterator();
        while (it1.hasNext()) {
            seq.addVertex((it1.next() as Vector2d).clone() as Vector2d);
        }
        return seq;
    }

    public function removeAllVertices():void {
        myVertices.clear();
    }

    public function copyTo(geom:IGeomParticle):void {
        if (!(geom is Vector2dSequence))
            throw new TSError(geom + ' не Vector2dSequence. Не скопировать');
        var seq:Vector2dSequence = geom as Vector2dSequence;
        seq.removeAllVertices();
        var it:Iterator = vertexIterator();
        while (it.hasNext()) {
            seq.addVertex(it.next() as Vector2d);
        }
    }

    public function createGlobal(localFrame:LocalFrame2d):IGeomParticle {
        var seq:Vector2dSequence = new Vector2dSequence;
        var it1:Iterator = vertexIterator();
        while (it1.hasNext()) {
            var r:Vector2d = (it1.next() as Vector2d).clone() as Vector2d;
            seq.addVertex(localFrame.getGlobal(r));
        }
        return seq;
    }

    public function toString():String {
        var s:String = '[';
        var it:Iterator = vertexIterator();
        while (it.hasNext()) {
            var v:Vector2d = it.next() as Vector2d;
            s += v + '; ';
        }
        s = s.substr(0, s.length - 2);
        s += ']';
        return s;
    }

    public function lastVertex():Vector2d {
        return myVertices.lastElement() as Vector2d;
    }

    public function getMiddlePoint(): Vector2d {
        if (myVertices.length() == 0) {
            throw new TSError("В полигоне нет вершин");
        }
        var minX: Number = Number.POSITIVE_INFINITY;
        var maxX: Number = Number.NEGATIVE_INFINITY;
        var minY: Number = Number.POSITIVE_INFINITY;
        var maxY: Number = Number.NEGATIVE_INFINITY;
        var it: Iterator = vertexIterator();
        while (it.hasNext()) {
            var point: Vector2d = it.next() as Vector2d;
            maxX = Math.max(point.myX, maxX);
            minX = Math.min(point.myX, minX);
            maxY = Math.max(point.myY, maxY);
            minY = Math.min(point.myY, minY);
        }
        return new Vector2d((maxX + minX) / 2, (maxY + minY) / 2);
    }

    public function setMiddlePoint(newMiddle: Vector2d):void {
        var delta:Vector2d = getMiddlePoint().multiply2d(-1).add2d(newMiddle);
        var array:Array = myVertices.toArray();
        for (var i:int = 0; i < array.length; ++i) {
            (array[i] as Vector2d).add2d(delta);
        }
    }

    public function get myVertexCount(): uint {
        return getVertices().length();
    }

    public function reverse(): void {
        myVertices.reverse();
    }

}
}