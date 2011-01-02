package com.turbostool.client.geom.test
{

import com.turbostool.client.geom.*;
import com.turbostool.client.utils.*;

import flexunit.framework.TestCase;

public class Vector2dSequenceTest extends TestCase
{
    public function Vector2dSequenceTest(methodName:String = null) {
        super(methodName);
    }

    public function testVector2dSequence():void {
        var seq:Vector2dSequence = new Vector2dSequence();
        seq.addVertex(new Vector2d(10, 9));
        seq.addVertex(new Vector2d(100, 90));
        seq.removeAllVertices();
        assertEquals(0, seq.getVertices().length());
    }

    public function testEquals():void {
        var seq:Vector2dSequence = new Vector2dSequence();
        seq.addVertex(new Vector2d(10, 9));
        seq.addVertex(new Vector2d(100, 90));
        seq.addVertex(new Vector2d(200, 100));
        seq.addVertex(new Vector2d(10, 50));
        seq.addVertex(new Vector2d(10, 90));
        var seq2:Vector2dSequence = new Vector2dSequence();
        seq2.addVertex(new Vector2d(10, 9));
        seq2.addVertex(new Vector2d(100, 90));
        seq2.addVertex(new Vector2d(200, 100));
        seq2.addVertex(new Vector2d(10, 50));
        seq2.addVertex(new Vector2d(10, 90));
        assertTrue(seq.equals(seq2));
        assertTrue(seq2.equals(seq));
        assertTrue(seq.equals(seq));
        assertTrue(seq2.equals(seq2));
        seq2.addVertex(new Vector2d(10, 90));
        assertFalse(seq.equals(seq2));
        seq.addVertex(new Vector2d(100, 90));
        assertFalse(seq.equals(seq2));
        assertFalse(seq.equals(new Vector2d(100, 90)));

        assertTrue(seq.equals(seq.clone()));
        seq.copyTo(seq2);
        assertTrue(seq.equals(seq2));
    }

    public function testGlobal():void {
        var seq:Vector2dSequence = new Vector2dSequence();
        seq.addVertex(new Vector2d(10, 9));
        seq.addVertex(new Vector2d(100, 90));
        seq.addVertex(new Vector2d(200, 100));
        var frame:LocalFrame2d = new LocalFrame2d(new Vector2d(9, 9), Math.PI / 2);
        var test:Vector2dSequence = new Vector2dSequence();
        test.addVertex(new Vector2d(0, 19));
        test.addVertex(new Vector2d(-81, 109));
        test.addVertex(new Vector2d(-91, 209));
        assertTrue(seq.createGlobal(frame) is Vector2dSequence);
        var res:Vector2dSequence = seq.createGlobal(frame) as Vector2dSequence;
        assertFalse(seq.equals(res));
        assertTrue(res, res.equals(test));
    }

    public function testString():void {
        var seq:Vector2dSequence = new Vector2dSequence();
        seq.addVertex(new Vector2d(10, 9));
        seq.addVertex(new Vector2d(100, 90));
        assertEquals('[(10; 9; 0); (100; 90; 0)]', seq.toString());
    }


}
}