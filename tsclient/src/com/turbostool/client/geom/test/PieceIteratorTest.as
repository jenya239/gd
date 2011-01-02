package com.turbostool.client.geom.test
{

import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.geom.Polygon;
import com.turbostool.client.geom.Vector2dSequence;
import com.turbostool.client.utils.*;
import com.turbostool.client.utils.collections.Iterator;

import flexunit.framework.TestCase;
import flexunit.framework.TestSuite;

;

public class PieceIteratorTest extends TestCase
{
    public function PieceIteratorTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new PieceIteratorTest("testPieceIterator"));
        return ts;
    }

    public function testPieceIterator():void {
        var r1:Vector2d = new Vector2d(10, 2);
        var r2:Vector2d = new Vector2d(103, 2);
        var r3:Vector2d = new Vector2d(104, 2);
        var r4:Vector2d = new Vector2d(105, 2);
        var seq:Vector2dSequence = new Vector2dSequence();
        seq.addVertex(r1);
        seq.addVertex(r2);
        seq.addVertex(r3);
        seq.addVertex(r4);
        var it:Iterator = seq.pieceIterator();
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r1, r2)).equals(it.next()));
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r2, r3)).equals(it.next()));
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r3, r4)).equals(it.next()));
        assertFalse(it.hasNext());


        var pol:Polygon = new Polygon();
        pol.addVertex(r1);
        pol.addVertex(r2);
        pol.addVertex(r3);
        pol.addVertex(r4);
        it = pol.pieceIterator();
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r1, r2)).equals(it.next()));
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r2, r3)).equals(it.next()));
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r3, r4)).equals(it.next()));
        assertTrue(it.hasNext());
        assertTrue((new Piece2d(r4, r1)).equals(it.next()));
        assertFalse(it.hasNext());
    }

}
}