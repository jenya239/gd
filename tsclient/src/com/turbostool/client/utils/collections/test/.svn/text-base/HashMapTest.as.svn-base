package com.turbostool.client.utils.collections.test
{

import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.HashMap;

import flexunit.framework.TestCase;

public class HashMapTest extends TestCase
{
    public function HashMapTest(methodName:String = null) {
        super(methodName);
    }

    //get
    //set
    //remove
    //synchonize

    public function testGet():void {
        var hash:HashMap = new HashMap();
        var key:Number = 1;
        var value:Number = 3;
        hash.setValue(key, value);
        assertEquals(value, hash.getValue(key));
        var key2:Number = 2;
        var value2:Number = 4;
        hash.setValue(key2, value2);
        assertEquals(value2, hash.getValue(key2));
        var value3:Number = 5;
        hash.setValue(key, value3);
        assertEquals(value3, hash.getValue(key));
    }

    public function testRemove():void {
        var hash:HashMap = new HashMap();
        var key:Number = 1;
        var value:Number = 3;
        hash.setValue(key, value);
        var key2:Number = 2;
        var value2:Number = 4;
        hash.setValue(key2, value2);
        hash.remove(key);
        assertFalse(hash.containsKey(key));
    }

    public function testSynchronize():void {
        var hash:HashMap = new HashMap();
        var key:Number = 1;
        var value:Number = 3;
        hash.setValue(key, value);
        var key2:Number = 2;
        var value2:Number = 4;
        hash.setValue(key2, value2);
        var array:ArrayList = new ArrayList();
        var valkey:Number = 56;
        array.addItem(valkey);
        array.addItem(key);
        hash.synchronizeWithCollection(array, createValue);
        assertEquals(value, hash.getValue(key));
        assertEquals(valkey, hash.getValue(valkey));
        assertFalse(hash.containsKey(key2));
        function createValue(arg1:Object):Object {
            return arg1;
        }

    }
}
}