package com.turbostool.client.utils.collections.test {

import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.CollectionIterator;

import flexunit.framework.TestCase;

public class ArrayListTest extends TestCase {
    public function ArrayListTest(methodName:String = null) {
        super(methodName);
    }

    public function testArrayList():void {
        var list:ArrayList = new ArrayList();
        assertEquals(0, list.length());
        assertTrue(list.isEmpty());
        var o1:Object = new Object();
        var o2:Object = new Object();
        list.addItem(o1);
        assertEquals(1, list.length());
        assertFalse(list.isEmpty());
        assertTrue(list.contains(o1));
        assertFalse(list.contains(o2));
        list.addItem(o2);
        assertTrue(list.contains(o1));
        assertTrue(list.contains(o2));

        var it:CollectionIterator = list.iterator() as CollectionIterator;
        assertTrue(it.hasNext());
        assertEquals(o1, it.next());
        assertTrue(it.hasNext());
        assertEquals(o2, it.next());
        assertFalse(it.hasNext());
        var catched:Boolean = false;
        try {
            it.next();
        } catch (e:Error) {
            catched = true;
        }
        assertTrue(catched);

        catched = false;
        try {
            list.getItemAt(2);
        } catch (e:Error) {
            catched = true;
        }
        assertTrue(catched);
        list.removeItem(o1);
        assertEquals(1, list.length());
        assertFalse(list.isEmpty());
        assertTrue(list.contains(o2));
        assertFalse(list.contains(o1));
        list.clear();
        assertEquals(0, list.length());
        assertTrue(list.isEmpty());
    }

    public function testAddCollection():void {
        var o1:Object = new Object();
        var o2:Object = new Object();
        var o3:Object = new Object();
        var o4:Object = new Object();
        var al1:ArrayList = new ArrayList();
        var al2:ArrayList = new ArrayList();
        al1.addCollection(al2);
        assertTrue(al1.isEmpty());
        al1.addItem(o1);
        al1.addItem(o2);
        al2.addItem(o3);
        al2.addItem(o4);
        al1.addCollection(al2);
        assertEquals(4, al1.length());
        assertTrue(al1.contains(o1));
        assertTrue(al1.contains(o2));
        assertTrue(al1.contains(o3));
        assertTrue(al1.contains(o4));
    }

    public function testInsertAfter(): void {
        var o: Object = new Object();
        var o0: Object = new Object();
        var o1: Object = new Object();
        var o2: Object = new Object();
        var o3: Object = new Object();
        var list: ArrayList = new ArrayList();
        list.addItem(o0);
        list.addItem(o1);
        list.addItem(o2);
        list.addItem(o3);
        list.insertAfter(2, o);
        assertEquals(o0, list.getItemAt(0));
        assertEquals(o1, list.getItemAt(1));
        assertEquals(o2, list.getItemAt(2));
        assertEquals(o, list.getItemAt(3));
        assertEquals(o3, list.getItemAt(4));
        assertEquals(5, list.length());
    }

    public function testInsertSorted(): void {
        var first: Number = 50;
        var second: Number = 10;
        var third: Number = 17;
        var four: Number = 60;
        var list: ArrayList = new ArrayList();
        list.insertSorted(first, compareNumber);
        list.insertSorted(second, compareNumber);
        list.insertSorted(third, compareNumber);
        list.insertSorted(four, compareNumber);
        assertEquals(second, list.getItemAt(0));
        assertEquals(list.getItemAt(1), third);
        assertEquals(list.getItemAt(2), first);
        assertEquals(list.getItemAt(3), four);

    }

    private function compareNumber(n1: Number, n2: Number): int {
        return Utils.sign(n1 - n2);
    }
}
}