package com.turbostool.client.utils.collections.test
{

import com.turbostool.client.utils.collections.*;

import flexunit.framework.TestCase;

public class PairIteratorTest extends TestCase
{
    public function PairIteratorTest(methodName:String = null) {
        super(methodName);
    }

    public function testPairIterator():void {
        var o1:Object = new Object();
        var o2:Object = new Object();
        var o3:Object = new Object();
        var o4:Object = new Object();
        var al1:ArrayList = new ArrayList();
        al1.addItem(o1);
        al1.addItem(o2);
        al1.addItem(o3);
        al1.addItem(o4);
        var pi:PairIterator = new PairIterator(al1);
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o1, o2)));
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o1, o3)));
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o1, o4)));
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o2, o3)));
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o2, o4)));
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o3, o4)));
        assertFalse(pi.hasNext());

        al1 = new ArrayList();
        al1.addItem(o1);
        al1.addItem(o2);
        pi = new PairIterator(al1);
        assertTrue(pi.hasNext());
        assertTrue((pi.next() as Pair).equals(new Pair(o1, o2)));
        assertFalse(pi.hasNext());
    }
}
}