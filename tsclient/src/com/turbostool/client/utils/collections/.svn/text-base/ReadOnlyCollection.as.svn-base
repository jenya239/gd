package com.turbostool.client.utils.collections {
import com.turbostool.client.utils.Assert;
import com.turbostool.client.utils.TSError;

public class ReadOnlyCollection implements Collection {
    private var myCollection: Collection;

    private function fail(): void {
        throw new TSError("коллекция только для чтения. руками не трогать!");
    }

    public function ReadOnlyCollection(col: Collection) {
        Assert.assertNotNull(col);
        myCollection = col;
    }

    public function addItem(item:Object) : Boolean {
        fail();
        return false;
    }

    public function clear() : void {
        fail();
    }

    public function contains(item:Object) : Boolean {
        return myCollection.contains(item);
    }

    public function isEmpty() : Boolean {
        return myCollection.isEmpty();
    }

    public function iterator() : Iterator {
        return myCollection.iterator();
    }

    public function length() : Number {
        return myCollection.length();
    }

    public function getItemAt(index:Number) : Object {
        return myCollection.getItemAt(index);
    }

    public function removeItem(item:Object) : Boolean {
        fail();
        return false;
    }

    public function toArray() : Array {
        return myCollection.cloneCollection().toArray();
    }

    public function addCollection(collection:Collection):void {
        fail();
    }

    public function subtract(collection:Collection):void {
        fail();
    }

    public function pairIterator():Iterator {
        return myCollection.pairIterator();
    }

    public function cloneCollection():Collection {
        return myCollection.cloneCollection();
    }

    public function sort(compareFunction:Function):void {
        fail();
    }

    public function removeCollection(collection:Collection):void {
        fail();
    }

    public function lastElement():Object {
        return myCollection.lastElement();
    }
}
}