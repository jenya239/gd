package com.turbostool.client.geom {
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.CollectionIterator;

public class PieceIterator extends CollectionIterator {
    private var myIsPolygon:Boolean;
    private var myLastNextVertexAvailability:Boolean;
    private var myPriviousVertex:Vector2d;
    private var myVertices:Collection;

    public function PieceIterator(sequence:Vector2dSequence) {
        super(sequence.getVertices());
        if (sequence.getVertices().length() < 2)
            throw new TSError('В многоугольнике должно быть более одной вершины');
        myIsPolygon = sequence is Polygon;
        myLastNextVertexAvailability = false;
        myPriviousVertex = null;
        myVertices = sequence.getVertices();
    }

    override public function hasNext():Boolean {
        if (myIsPolygon) {
            if (super.hasNext()) {
                return super.hasNext();
            } else {
                return myLastNextVertexAvailability;
            }
        } else {
            return super.hasNext();
        }
    }

    override public function next():Object {
        var vertex:Vector2d;
        myLastNextVertexAvailability = super.hasNext();
        if (myPriviousVertex == null) myPriviousVertex = super.next() as Vector2d;
        if (myIsPolygon) {
            vertex = (myLastNextVertexAvailability)
                    ? super.next() as Vector2d
                    : myVertices.iterator().next() as Vector2d;
        } else {
            vertex = super.next() as Vector2d;
        }
        var piece:Piece2d = new Piece2d(myPriviousVertex, vertex);
        myPriviousVertex = vertex;
        return piece;
    }
}
}

/*package com.turbostool.client.geom
 {
 import com.turbostool.client.utils.collections.Iterator;
 import com.turbostool.client.utils.Vector2d;

 public class VertexIterator implements Iterator {
 private var myVertices:Array;
 private var myCurrentPosition:int;

 public function VertexIterator(sequence:Vector2dSequence) {
 myCurrentPosition = 0;
 myVertices = sequence.getVertices();
 }

 public function hasNext():Boolean {
 return (myCurrentPosition < myVertices.length);
 }

 public function next():Vector2d	{
 var res:Vector2d = myVertices[myCurrentPosition] as Vector2d;
 myCurrentPosition++;
 return res
 }

 }
 }*/